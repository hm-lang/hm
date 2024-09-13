const common = @import("common.zig");
const OrElse = common.OrElse;
const OwnedList = @import("owned_list.zig").OwnedList;
const SmallString = @import("string.zig").Small;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Token = @import("token.zig").Token;
const node_zig = @import("node.zig");
const Node = node_zig.Node;
const TokenIndex = node_zig.TokenIndex;
const NodeIndex = node_zig.NodeIndex;
const operator_zig = @import("operator.zig");
const Operator = operator_zig.Operator;
const Operation = operator_zig.Operation;
const Until = @import("until.zig").Until;

const std = @import("std");

const OwnedNodes = OwnedList(Node);
const OwnedNodeIndices = OwnedList(NodeIndex);

const ParserError = error{
    out_of_memory,
    out_of_statements,
    broken_invariant,
    syntax,
    unimplemented,
};

// TODO: this is probably better as `Nodifier` (like `Token` -> `Tokenizer`).
// we still need a `parser` after getting the nodes, to turn it into grammar.
// and then `interpret` or `transpile` after getting those.
// alternatively, we keep this and then for `complete` we pass in a `Grammar`
// class that is an `anytype` and which has methods like `nestScope`,
// `unnestScope`, `defineFunction`, `declareFunction`, etc.
pub const Parser = struct {
    // The parser will free this at the end.
    tokenizer: Tokenizer = .{},
    nodes: OwnedNodes = OwnedNodes.init(),
    farthest_token_index: usize = 0,

    pub fn deinit(self: *Self) void {
        self.tokenizer.deinit();
        self.nodes.deinit();
    }

    pub fn complete(self: *Self) ParserError!void {
        if (self.nodes.count() > 0) {
            return;
        }
        const root_node_index = try self.appendNextEnclosed(0, .none);
        // So that `nodejindex == 0` appears to be invalid, the
        // root should be appended first, then its child nodes.
        std.debug.assert(root_node_index == 0);
    }

    fn appendNextEnclosed(self: *Self, tab: u16, open: Open) ParserError!NodeIndex {
        // So that we go roughly in order, append the enclosed first, then child nodes.
        // but we don't know what it is yet, so just make a placeholder.
        const enclosed_node_index = try self.justAppendNode(.end);

        var enclosed_start_index: usize = 0;
        var previous_statement_index: usize = 0;
        // TODO: return a `Tabbed` struct that gives the `tab` and a `start_parsing_index`
        //      if the tab is indented, append another Open.none enclosed block.
        while (self.getSameBlockNextNonSpacingTokenIndex(tab)) |start_parsing_index| {
            self.farthest_token_index = start_parsing_index;
            const current_statement_index = try self.appendNextStatement(tab, Until.closing(open), .{
                .fail_with = "statement needs an expression",
            });
            if (enclosed_start_index == 0) {
                enclosed_start_index = current_statement_index;
            } else {
                self.nodes.inBounds(previous_statement_index).setStatementNext(current_statement_index) catch {
                    return ParserError.broken_invariant;
                };
            }
            previous_statement_index = current_statement_index;
        }

        // TODO: enclosed.inner_tab = inner_index.tab; ???
        self.nodes.set(enclosed_node_index, Node{ .enclosed = .{
            .tab = tab,
            .open = open,
            .start = enclosed_start_index,
        } }) catch unreachable;
        return enclosed_node_index;
    }

    fn appendNextStatement(self: *Self, tab: u16, until: Until, or_else: OrElse) ParserError!NodeIndex {
        // To make nodes mostly go in order, append the node first.
        const statement_index = try self.justAppendNode(.end);

        const expression_index = try self.appendNextExpression(tab, until, or_else);

        self.nodes.set(statement_index, Node{ .statement = .{
            .node = expression_index,
        } }) catch unreachable;

        return statement_index;
    }

    /// Supports starting with spacing *or not* (e.g., for the start of a statement
    /// where we don't want to check the indent yet).
    fn appendNextExpression(self: *Self, tab: u16, until: Until, or_else: OrElse) ParserError!NodeIndex {
        errdefer {
            if (or_else.be_noisy()) |error_message| {
                self.addTokenizerError(error_message);
            }
        }
        // The current left node which can interact with the next operation,
        // is the last element of `hierarchy`, with nesting all the way up
        // to the "root node" (`hierarchy.inBounds(0)`) which should be returned.
        var hierarchy = OwnedNodeIndices.init();
        // TODO: will we ever run into any malformed input where we need `until`
        // inside the `appendNextStandaloneExpression`?  e.g., `(whatever, +)`?
        // i think that will be some other syntax error, though.
        // TODO: add `tab` to the StatementNode so we can see if it's been indented
        _ = try self.appendNextStandaloneExpression(&hierarchy, tab, or_else);
        defer hierarchy.deinit();

        while (true) {
            const operation = try self.seekNextOperation(tab, until);
            switch (operation.operator) {
                .none => return hierarchy.inBounds(0),
                .comma => {
                    self.farthest_token_index += 1;
                    return hierarchy.inBounds(0);
                },
            }
            if (operation.type == .postfix) {
                try self.appendPostfixOperation(&hierarchy, operation);
            } else if (self.appendNextStandaloneExpression(&hierarchy, tab, .only_try)) |right_index| {
                try self.appendInfixOperation(&hierarchy, operation, right_index);
            } else |error_getting_right_hand_expression| {
                if (!operation.operator.isPostfixable()) {
                    if (or_else.be_noisy()) |_| {
                        self.addTokenizerError("infix operator needs right-hand expression");
                    }
                    return error_getting_right_hand_expression;
                }
                // We check for postfixable operations only after infix errors because
                // we need to verify there's no standalone expression available after
                // the operation (since infix would take precedence), and we don't want
                // to re-implement logic that checks for a standalone expression.
                try self.appendPostfixOperation(&hierarchy, .{
                    .operator = operation.operator,
                    .type = .postfix,
                });
                // This is not very pretty but some version of this logic (here or elsewhere)
                // needs to exist for declaring things like `Int;` in one line.
                // TODO: maybe we need to restore `self.farthest_token_index` in `appendNextStandaloneExpression`.
                switch (try self.peekToken()) {
                    .close => |close| {
                        if (until.shouldBreakAtClose(close)) {
                            self.farthest_token_index += 1;
                            return hierarchy.inBounds(0);
                        }
                        self.farthest_token_index -= 1;
                    },
                    .spacing => {
                        // The only way we can get here is if we newline'd it;
                        // keep the farthest_token_index steady.
                    },
                    .file_end => return hierarchy.inBounds(0),
                    else => {
                        self.farthest_token_index -= 1;
                    },
                }
            }
        }
    }

    // Returns the next postfix or infix operation.
    // Prefix operations are taken care of inside of `appendNextStandaloneExpression`.
    fn seekNextOperation(self: *Self, tab: u16, until: Until) ParserError!Operation {
        const restore_index = self.farthest_token_index;

        self.farthest_token_index = self.getSameStatementNextNonSpacingTokenIndex(tab) orelse {
            return .{ .operator = .none };
        };
        const operation: Operation = switch (try self.peekToken()) {
            .operator => |operator| blk: {
                if (operator.isInfixable()) {
                    self.farthest_token_index += 1;
                    break :blk .{ .operator = operator, .type = .infix };
                } else if (operator.isPostfixable()) {
                    self.farthest_token_index += 1;
                    break :blk .{ .operator = operator, .type = .postfix };
                } else {
                    std.debug.assert(operator.isPrefixable());
                    // Back up so that the next standalone expression starts at the
                    // spacing before this prefix:
                    // TODO: this will probably break tab functionality
                    self.farthest_token_index -= 1;
                    // Pretend that we have an operator before this prefix.
                    break :blk .{ .operator = .implicit_member_access, .type = .infix };
                }
            },
            .close => |close| blk: {
                if (until.shouldBreakAtClose(close)) {
                    self.farthest_token_index += 1;
                    return .{ .operator = .none };
                }
                // Same as the `else` block below:
                self.farthest_token_index -= 1;
                break :blk .{ .operator = .implicit_member_access, .type = .infix };
            },
            else => blk: {
                // We encountered another realizable token, back up so that
                // we maintain the invariant that there's a space before the next real element.
                self.farthest_token_index -= 1;
                break :blk .{ .operator = .implicit_member_access, .type = .infix };
            },
        };

        if (until.shouldBreakBeforeOperation(operation)) {
            self.farthest_token_index = restore_index;
            return .{ .operator = .none };
        }
        return operation;
    }

    /// Adds an atom with possible prefix (but NOT postfix) operators.
    /// Includes things like `1.234`, `My_variable`, `+4.56`, `-7.89`,
    /// `++Index` or `!Countdown` as well.  For member access like
    /// `First_identifier Second_identifier`, just grab the first one.
    /// NOTE: do NOT add the returned index into `hierarchy`, we'll do that for you.
    /// Supports starting with spacing *or not* (e.g., for the start of a statement).
    fn appendNextStandaloneExpression(self: *Self, hierarchy: *OwnedNodeIndices, tab: u16, or_else: OrElse) ParserError!NodeIndex {
        switch (try self.peekToken()) {
            .spacing => |spacing| {
                try self.assertSyntax(self.shouldContinueStatementAfterSpacing(spacing, tab), or_else.map(expected_spacing));
                self.farthest_token_index += 1;
            },
            else => {},
        }

        switch (try self.peekToken()) {
            .starts_upper, .number => {
                const atomic_index = try self.justAppendNode(Node{
                    .atomic_token = self.farthest_token_index,
                    .tab = try self.tabAt(self.farthest_token_index - 1),
                });
                self.farthest_token_index += 1;

                hierarchy.append(atomic_index) catch return ParserError.out_of_memory;
                return atomic_index;
            },
            .starts_lower => {
                const callable_index = try self.justAppendNode(Node{
                    .callable_token = self.farthest_token_index,
                    .tab = try self.tabAt(self.farthest_token_index - 1),
                });
                self.farthest_token_index += 1;

                hierarchy.append(callable_index) catch return ParserError.out_of_memory;
                return callable_index;
            },
            .open => |open| {
                self.farthest_token_index += 1;
                const enclosed_index = try self.appendNextEnclosed(Node{ .enclosed = .{
                    .open = open,
                    .outer_tab = try self.tabAt(self.farthest_token_index - 1),
                } });
                hierarchy.append(enclosed_index) catch return ParserError.out_of_memory;
                return enclosed_index;
            },
            .operator => |operator| {
                if (!operator.isPrefixable()) {
                    if (or_else.be_noisy()) |_| {
                        self.addTokenizerError("not a prefix operator");
                    }
                    return ParserError.syntax;
                }
                self.farthest_token_index += 1;
                // We need to parse a different way because we can't break the hierarchy invariant here.
                // Start with the prefix to maintain a rough left-to-right direction inside `self.nodes`.
                const prefix_index = try self.justAppendNode(Node{
                    .prefix = .{
                        .operator = operator,
                        .node = 0, // break the invariant here
                    },
                });
                // We need every operation *stronger* than this prefix to be attached to this prefix.
                // TODO: add `tab` to the StatementNode so we can see if it's been indented
                const inner_index = try self.appendNextExpression(tab, Until.prefix_strength_wins(operator), or_else);
                switch (self.nodes.items()[prefix_index]) {
                    // restore the invariant:
                    .prefix => |*prefix| {
                        prefix.node = inner_index;
                    },
                    else => return ParserError.broken_invariant,
                }
                // We don't need to append `inner_index` because we know it will not reappear.
                // It was stronger than `prefix_index` and so should never be split out.
                hierarchy.append(prefix_index) catch return ParserError.out_of_memory;
                return prefix_index;
            },
            else => {
                if (or_else.be_noisy()) |_| {
                    self.addTokenizerError("expected an expression");
                }
                return ParserError.syntax;
            },
        }
    }

    fn appendPostfixOperation(self: *Self, hierarchy: *OwnedNodeIndices, operation: Operation) ParserError!void {
        const operation_precedence = operation.precedence(Operation.Compare.on_right);
        var hierarchy_index = hierarchy.count();
        var left_index: NodeIndex = 0;
        while (hierarchy_index > 0) {
            hierarchy_index -= 1;
            left_index = hierarchy.inBounds(hierarchy_index);
            const left_operation = self.nodeInBounds(left_index).operation();
            // lower precedence means higher priority.
            if (left_operation.isPostfix() or left_operation.precedence(Operation.Compare.on_left) <= operation_precedence) {
                // `left` has higher priority; we should continue up the hierarchy
                // until we find the spot that this new operation should take.
                _ = hierarchy.pop();
            } else {
                // `operation` has higher priority, so we need to invert the nodes a bit.
                // break an invariant here:
                const inner_index = self.nodeInBounds(left_index).swapRight(0) catch {
                    self.addTokenizerError("cannot postfix this");
                    return ParserError.syntax;
                };
                const next_index = try self.justAppendNode(.{ .postfix = .{
                    .operator = operation.operator,
                    .node = inner_index,
                } });
                // Restore the invariant.  Don't hold onto a reference to the `left_index` node
                // because it can be invalidated by appending (i.e., in the previous statement).
                _ = self.nodeInBounds(left_index).swapRight(next_index) catch unreachable;
                // Fix up the hierarchy at the end:
                hierarchy.append(next_index) catch return ParserError.out_of_memory;
                hierarchy.append(inner_index) catch return ParserError.out_of_memory;
                return;
            }
        }
        if (left_index == 0) {
            return ParserError.broken_invariant;
        }
        hierarchy.append(try self.justAppendNode(.{ .postfix = .{
            .operator = operation.operator,
            .node = left_index,
        } })) catch return ParserError.out_of_memory;
    }

    fn appendInfixOperation(self: *Self, hierarchy: *OwnedNodeIndices, operation: Operation, right_index: NodeIndex) ParserError!void {
        const operation_precedence = operation.precedence(Operation.Compare.on_right);
        var hierarchy_index = hierarchy.count();
        var left_index: NodeIndex = 0;
        while (hierarchy_index > 0) {
            hierarchy_index -= 1;
            left_index = hierarchy.inBounds(hierarchy_index);
            const left_operation = self.nodeInBounds(left_index).operation();
            // lower precedence means higher priority.
            if (left_operation.isPostfix() or left_operation.precedence(Operation.Compare.on_left) <= operation_precedence) {
                // `left` has higher priority; we should continue up the hierarchy
                // until we find the spot that this new operation should take.
                _ = hierarchy.pop();
            } else {
                // `operation` has higher priority, so we need to invert the nodes a bit.
                // break an invariant here:
                const inner_index = self.nodeInBounds(left_index).swapRight(0) catch {
                    self.addTokenizerError("cannot right-operate on this");
                    return ParserError.syntax;
                };
                const next_index = try self.justAppendNode(.{ .binary = .{
                    .operator = operation.operator,
                    .left = inner_index,
                    .right = right_index,
                } });
                // Restore the invariant.  Don't hold onto a reference to the `left_index` node
                // because it can be invalidated by appending (i.e., in the previous statement).
                _ = self.nodeInBounds(left_index).swapRight(next_index) catch unreachable;
                // Fix up the hierarchy at the end:
                hierarchy.append(next_index) catch return ParserError.out_of_memory;
                hierarchy.append(inner_index) catch return ParserError.out_of_memory;
                return;
            }
        }
        if (left_index == 0) {
            return ParserError.broken_invariant;
        }
        hierarchy.append(try self.justAppendNode(.{ .binary = .{
            .operator = operation.operator,
            .left = left_index,
            .right = right_index,
        } })) catch return ParserError.out_of_memory;
    }

    fn justAppendNode(self: *Self, node: Node) ParserError!NodeIndex {
        const index = self.nodes.count();
        self.nodes.append(node) catch {
            return ParserError.out_of_memory;
        };
        return index;
    }

    fn nextToken(self: *Self) ParserError!Token {
        const token = try self.peekToken();
        self.farthest_token_index += 1;
        return token;
    }

    fn peekToken(self: *Self) ParserError!Token {
        return self.tokenAt(self.farthest_token_index);
    }

    fn tokenAt(self: *Self, at_index: usize) ParserError!Token {
        return self.tokenizer.at(at_index) catch {
            // TODO: probably should distinguish between out of memory (rethrow)
            // and out of tokens => out_of_statements
            return ParserError.out_of_statements;
        };
    }

    fn tabAt(self: *Self, at_index: usize) ParserError!Token {
        return switch (try self.tokenAt(at_index)) {
            .spacing => |spacing| spacing.absolute,
            else => return ParserError.broken_invariant,
        };
    }

    /// For inside a block, the next statement index to continue with
    fn getSameBlockNextNonSpacingTokenIndex(self: *Self, tab: u16) ?TokenIndex {
        // TODO: ignore comments as well
        switch (self.peekToken() catch return null) {
            .file_end => return null,
            // TODO: ignore infix operators for indent level:
            //&|MyValue:
            //&|        +SomeValue  # keep prefix operators at tab indent
            //&|    -   CoolStuff   # infix operator should be ignored
            .spacing => |spacing| if (spacing.getNewlineTab()) |newline_tab| {
                if (newline_tab % 4 != 0) {
                    self.addTokenizerError("tabs should be 4-wide");
                    return null;
                }
                if (newline_tab < tab) {
                    return null;
                }
                return self.farthest_token_index + 1;
            } else {
                // Not a newline, just continuing in the same statement
                return self.farthest_token_index + 1;
            },
            else => return null,
        }
    }

    /// For inside a statement, the next index that we should continue with.
    fn getSameStatementNextNonSpacingTokenIndex(self: *Self, tab: u16) ?TokenIndex {
        // TODO: ignore comments as well
        switch (self.peekToken() catch return null) {
            .file_end => return null,
            .spacing => |spacing| if (self.shouldContinueStatementAfterSpacing(spacing, tab)) {
                return self.farthest_token_index + 1;
            } else {
                return null;
            },
            else => return null,
        }
    }

    /// This needs to be a Parser method because it will look for open
    /// parens/braces/brackets on the next line.
    fn shouldContinueStatementAfterSpacing(self: *Self, spacing: Token.Spacing, tab: u16) bool {
        _ = self;
        // TODO: check for being the first token since starting an expression
        if (spacing.getNewlineTab()) |new_tab| {
            // TODO: this needs to be more complicated based on braces etc.
            return new_tab >= tab + 8;
        }
        // We are just continuing on one line
        return true;
    }

    fn assertAndConsumeNextTokenIf(self: *Self, expected_tag: Token.Tag, or_else: OrElse) ParserError!void {
        const next_token = try self.peekToken();
        errdefer {
            common.debugPrint("actual tag is {d}\n", .{@intFromEnum(next_token.tag())});
        }
        try self.assertSyntax(next_token.tag() == expected_tag, or_else);
        self.farthest_token_index += 1;
    }

    fn assertSyntax(self: *Self, value: bool, or_else: OrElse) ParserError!void {
        if (value) {
            return;
        }
        if (or_else.be_noisy()) |error_message| {
            self.addTokenizerError(error_message);
        }
        return ParserError.syntax;
    }

    fn addTokenizerError(self: *Self, error_message: []const u8) void {
        self.tokenizer.addErrorAt(self.farthest_token_index, error_message);
    }

    pub fn printTokenDebugInfo(self: *Self) void {
        self.tokenizer.printDebugInfoAt(self.farthest_token_index);
    }

    fn nodeInBounds(self: *Self, index: usize) *Node {
        return &self.nodes.items()[index];
    }

    const Open = Token.Open;
    const Close = Token.Close;
    const Self = @This();
};

const Tabbed = struct {
    tab: u16,
    start_parsing_index: NodeIndex = 0,
};

const expected_spacing = "expected spacing between each identifier";

test "parser simple expressions" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "3.456",
        "    hello_you",
        "+1.234",
        "  -5.678",
        "    $$$Foe",
        "        Fum--",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 1, .tab = 0 } },
        Node{ .atomic_token = 1 }, // 3.456
        Node{ .statement = .{ .node = 3, .tab = 4 } },
        Node{ .callable = .{ .name_token = 3 } }, // hello_you
        Node{ .statement = .{ .node = 5, .tab = 0 } },
        // [5]:
        Node{ .prefix = .{ .operator = Operator.plus, .node = 6 } },
        Node{ .atomic_token = 7 }, // 1.234
        Node{ .statement = .{ .node = 8, .tab = 2 } },
        Node{ .prefix = .{ .operator = Operator.minus, .node = 9 } },
        Node{ .atomic_token = 11 }, // 5.678
        // [10]:
        Node{ .statement = .{ .node = 11, .tab = 4 } },
        Node{ .prefix = .{ .operator = Operator.lambda3, .node = 12 } },
        Node{ .atomic_token = 15 }, // Foe
        Node{ .statement = .{ .node = 15, .tab = 8 } },
        Node{ .atomic_token = 17 }, // Fum
        // [15]:
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 14 } },
        .end,
    });
    // No tampering done with the file, i.e., no errors.
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "parser multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Wompus * 3.14"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        Node{ .statement = .{ .node = 3, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Wompus
        Node{ .atomic_token = 5 }, // 3.14
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 2 } },
        .end,
    });
}

test "parser simple (and postfix) implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Pi Sky"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Sci Fi++"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Kite Sty Five!"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 3, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Pi
        Node{ .atomic_token = 3 }, // Sky
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 1, .right = 2 } },
        Node{ .statement = .{ .node = 8, .tab = 0 } },
        // [5]:
        Node{ .atomic_token = 5 }, // Sci
        Node{ .atomic_token = 7 }, // Fi
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 5, .right = 6 } },
        Node{ .postfix = .{ .operator = Operator.increment, .node = 7 } },
        Node{ .statement = .{ .node = 15, .tab = 0 } },
        // [10]:
        Node{ .atomic_token = 11 }, // Kite
        Node{ .atomic_token = 13 }, // Sty
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 10, .right = 11 } },
        Node{ .atomic_token = 15 }, // Five
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 12, .right = 13 } },
        // [15]:
        Node{ .postfix = .{ .operator = Operator.not, .node = 14 } },
        .end,
    });
}

test "parser complicated (and prefix) implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("--Why Shy Spy"));
    try parser.tokenizer.file.lines.append(try SmallString.init("!Chai Lie Fry"));
    try parser.tokenizer.file.lines.append(try SmallString.init("!Knife Fly Nigh!"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 1, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 6 } },
        Node{ .atomic_token = 3 }, // Why
        Node{ .atomic_token = 5 }, // Shy
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 2, .right = 3 } },
        // [5]:
        Node{ .atomic_token = 7 }, // Spy
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 4, .right = 5 } },
        Node{ .statement = .{ .node = 8, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 13 } },
        Node{ .atomic_token = 11 }, // Chai
        // [10]:
        Node{ .atomic_token = 13 }, // Lie
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 9, .right = 10 } },
        Node{ .atomic_token = 15 }, // Fry
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 11, .right = 12 } },
        Node{ .statement = .{ .node = 15, .tab = 0 } },
        // [15]:
        Node{ .prefix = .{ .operator = Operator.not, .node = 21 } },
        Node{ .atomic_token = 19 }, // Knife
        Node{ .atomic_token = 21 }, // Fly
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 16, .right = 17 } },
        Node{ .atomic_token = 23 }, // Nigh
        // [20]:
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 18, .right = 19 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 20 } },
        .end,
    });
}

test "simple prefix/postfix operators with multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "++Theta * Beta",
        "Zeta * ++Woga",
        "Yodus-- * Spatula",
        "Wobdash * Flobsmash--",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 4, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 2 } },
        Node{ .atomic_token = 3 }, // Theta
        Node{ .atomic_token = 7 }, // Beta
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 3 } },
        // [5]:
        Node{ .statement = .{ .node = 9, .tab = 0 } },
        Node{ .atomic_token = 9 }, // Zeta
        Node{ .prefix = .{ .operator = Operator.increment, .node = 8 } },
        Node{ .atomic_token = 15 }, // Woga
        Node{ .binary = .{ .operator = Operator.multiply, .left = 6, .right = 7 } },
        // [10]:
        Node{ .statement = .{ .node = 14, .tab = 0 } },
        Node{ .atomic_token = 17 }, // Yodus
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 11 } },
        Node{ .atomic_token = 23 }, // Spatula
        Node{ .binary = .{ .operator = Operator.multiply, .left = 12, .right = 13 } },
        // [15]:
        Node{ .statement = .{ .node = 18, .tab = 0 } },
        Node{ .atomic_token = 25 }, // Wobdash
        Node{ .atomic_token = 29 }, // Flobsmash
        Node{ .binary = .{ .operator = Operator.multiply, .left = 16, .right = 19 } },
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 17 } },
        // [20]:
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "complicated prefix/postfix operators with addition/multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Apple * !Berry Cantaloupe-- + 500"));
    try parser.tokenizer.file.lines.append(try SmallString.init("--Xeno Yak! - 3000 * Zelda"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 9, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Apple
        Node{ .prefix = .{ .operator = Operator.not, .node = 6 } },
        Node{ .atomic_token = 7 }, // Berry
        Node{ .atomic_token = 9 }, // Cantaloupe
        // [5]:
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 3, .right = 4 } },
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 5 } },
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 2 } },
        Node{ .atomic_token = 15 }, // 500
        Node{ .binary = .{ .operator = Operator.plus, .left = 7, .right = 8 } },
        // [10]:
        Node{ .statement = .{ .node = 17, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 14 } },
        Node{ .atomic_token = 19 }, // Xeno
        Node{ .atomic_token = 21 }, // Yak
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 12, .right = 13 } },
        // [15]:
        Node{ .postfix = .{ .operator = Operator.not, .node = 11 } },
        Node{ .atomic_token = 27 }, // 3000
        Node{ .binary = .{ .operator = Operator.minus, .left = 15, .right = 19 } },
        Node{ .atomic_token = 31 }, // Zelda
        Node{ .binary = .{ .operator = Operator.multiply, .left = 16, .right = 18 } },
        // [20]:
        .end,
    });
}

test "nested prefix/postfix operators" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "Abc Xyz-- !",
        "! ++Def Uvw",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 5, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Abc
        Node{ .atomic_token = 3 }, // Xyz
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 1, .right = 2 } },
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 3 } },
        // [5]:
        Node{ .postfix = .{ .operator = Operator.not, .node = 4 } },
        Node{ .statement = .{ .node = 7, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 8 } },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 11 } },
        Node{ .atomic_token = 13 }, // Def
        // [10]:
        Node{ .atomic_token = 15 }, // Uvw
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 9, .right = 10 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "deeply nested prefix/postfix operators" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("$$Yammer * Zen++!"));
    try parser.tokenizer.file.lines.append(try SmallString.init("!--$Oh Great * Hessian"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 4, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.lambda2, .node = 2 } },
        Node{ .atomic_token = 3 }, // Yammer
        Node{ .atomic_token = 7 }, // Zen
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 6 } },
        // [5]:
        Node{ .postfix = .{ .operator = Operator.increment, .node = 3 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 5 } },
        Node{ .statement = .{ .node = 15, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 9 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 13 } },
        // [10]:
        Node{ .prefix = .{ .operator = Operator.lambda1, .node = 11 } },
        Node{ .atomic_token = 19 }, // Oh
        Node{ .atomic_token = 21 }, // Great
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 10, .right = 12 } },
        Node{ .atomic_token = 25 }, // Hessian
        // [15]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 8, .right = 14 } },
        .end,
    });
}

test "order of operations with addition and multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Alpha * Gamma + Epsilon"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Panko + K_panko * 1000"));
    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 5, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Alpha
        Node{ .atomic_token = 5 }, // Gamma
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 2 } },
        Node{ .atomic_token = 9 }, // Epsilon
        // [5]:
        Node{ .binary = .{ .operator = Operator.plus, .left = 3, .right = 4 } },
        Node{ .statement = .{ .node = 9, .tab = 0 } },
        Node{ .atomic_token = 11 }, // Panko
        Node{ .atomic_token = 15 }, // K_panko
        Node{ .binary = .{ .operator = Operator.plus, .left = 7, .right = 11 } },
        // [10]:
        Node{ .atomic_token = 19 }, // 1000
        Node{ .binary = .{ .operator = Operator.multiply, .left = 8, .right = 10 } },
        .end,
    });
}

test "generic types" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "container54[of; i1234, at. str5[qusp], array[dongle]]",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 1, .tab = 0 } },
        Node{ .callable = .{ .name_token = 1, .generics = 12 } }, // container54
        Node{ .callable = .{ .name_token = 5 } }, // of
        Node{ .callable = .{ .name_token = 9 } }, // i1234
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 2, .right = 3 } },
        // [5]:
        Node{ .callable = .{ .name_token = 13 } }, // at
        Node{ .binary = .{ .operator = Operator.comma, .left = 4, .right = 9 } },
        Node{ .callable = .{ .name_token = 17, .generics = 8 } }, // str5
        Node{ .callable = .{ .name_token = 21 } }, // qusp
        Node{ .binary = .{ .operator = Operator.declare_temporary, .left = 5, .right = 7 } },
        // [10]:
        Node{ .callable = .{ .name_token = 27, .generics = 11 } }, // array
        Node{ .callable = .{ .name_token = 31 } }, // dongle
        Node{ .binary = .{ .operator = Operator.comma, .left = 6, .right = 10 } },
        .end,
    });
    // No errors when parsing:
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "simple function calls" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "superb(Brepus. 161, Canyon; Noynac, Candid)",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 1, .tab = 0 } },
        Node{ .callable = .{ .name_token = 1, .arguments = 10 } }, // superb
        Node{ .atomic_token = 5 }, // Brepus
        Node{ .atomic_token = 9 }, // 161
        Node{ .binary = .{ .operator = Operator.declare_temporary, .left = 2, .right = 3 } },
        // [5]:
        Node{ .atomic_token = 13 }, // Canyon
        Node{ .binary = .{ .operator = Operator.comma, .left = 4, .right = 8 } },
        Node{ .atomic_token = 17 }, // Noynac
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 5, .right = 7 } },
        Node{ .atomic_token = 21 }, // Candid
        // [10]:
        Node{ .binary = .{ .operator = Operator.comma, .left = 6, .right = 9 } },
        .end,
    });
    // No errors when parsing:
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "generic function calls" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "fungus[type1: t_array[str7], type2, type3; i64](Life: 17, Cardio!, Fritz; foo_fritz)",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 1, .tab = 0 } },
        Node{ .callable = .{ .name_token = 1, .generics = 9, .arguments = 19 } }, // fungus
        Node{ .callable = .{ .name_token = 5 } }, // type1
        Node{ .callable = .{ .name_token = 9, .generics = 4 } }, // t_array
        Node{ .callable = .{ .name_token = 13 } }, // str7
        // [5]:
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 3 } },
        Node{ .callable = .{ .name_token = 19 } }, // type2
        Node{ .binary = .{ .operator = Operator.comma, .left = 5, .right = 6 } },
        Node{ .callable = .{ .name_token = 23 } }, // type3
        Node{ .binary = .{ .operator = Operator.comma, .left = 7, .right = 11 } },
        // [10]:
        Node{ .callable = .{ .name_token = 27 } }, // i64
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 8, .right = 10 } },
        Node{ .atomic_token = 33 }, // Life
        Node{ .atomic_token = 37 }, // 17
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 12, .right = 13 } },
        // [15]:
        Node{ .atomic_token = 41 }, // Cardio
        Node{ .binary = .{ .operator = Operator.comma, .left = 14, .right = 17 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 15 } },
        Node{ .atomic_token = 47 }, // Fritz
        Node{ .binary = .{ .operator = Operator.comma, .left = 16, .right = 21 } },
        // [20]:
        Node{ .callable = .{ .name_token = 51 } }, // foo_fritz
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 18, .right = 20 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "declarations with missing right expressions" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
            common.debugPrint("# tokens:\n", parser.tokenizer.tokens);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("Esper;"));
        try parser.tokenizer.file.lines.append(try SmallString.init("Jesper."));
        try parser.tokenizer.file.lines.append(try SmallString.init("Esperk:"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 2, .tab = 0 } },
            Node{ .atomic_token = 1 }, // Esper
            Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 1 } },
            Node{ .statement = .{ .node = 5, .tab = 0 } },
            Node{ .atomic_token = 5 }, // Jesper
            // [5]:
            Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 4 } },
            Node{ .statement = .{ .node = 8, .tab = 0 } },
            Node{ .atomic_token = 9 }, // Esperk
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 7 } },
            .end,
        });
        // No errors in attempts to parse a RHS expression for the infix operators.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "Esper;",
            "Jesper.",
            "Esperk:",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("(Jarok:)"));
        try parser.tokenizer.file.lines.append(try SmallString.init("[Turmeric;]"));
        try parser.tokenizer.file.lines.append(try SmallString.init("{Quinine.}"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 1, .tab = 0 } },
            Node{ .enclosed = .{ .open = .paren, .root = 3 } },
            Node{ .atomic_token = 3 }, // Jarok
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 2 } },
            Node{ .statement = .{ .node = 5, .tab = 0 } },
            // [5]:
            Node{ .enclosed = .{ .open = .bracket, .root = 7 } },
            Node{ .atomic_token = 11 }, // Turmeric
            Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 6 } },
            Node{ .statement = .{ .node = 9, .tab = 0 } },
            Node{ .enclosed = .{ .open = .brace, .root = 11 } },
            // [10]:
            Node{ .atomic_token = 19 }, // Quinine
            Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 10 } },
            .end,
        });
        // No errors in attempts to parse a RHS expression for the infix operators.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "(Jarok:)",
            "[Turmeric;]",
            "{Quinine.}",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("funE1(F2., G3;, H4:):"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 10, .tab = 0 } },
            Node{ .callable = .{ .name_token = 1, .arguments = 8 } }, // funE1
            Node{ .atomic_token = 5 }, // F2
            Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 2 } },
            Node{ .atomic_token = 11 }, // G3
            // [5]:
            Node{ .binary = .{ .operator = Operator.comma, .left = 3, .right = 6 } },
            Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 4 } },
            Node{ .atomic_token = 17 }, // H4
            Node{ .binary = .{ .operator = Operator.comma, .left = 5, .right = 9 } },
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 7 } },
            // [10]:
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 1 } },
            .end,
        });
        // No errors in attempts to parse a RHS expression for the infix operators.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "funE1(F2., G3;, H4:):",
        });
    }
}

// TODO: should we remove `generics` and `arguments` from the `CallableNode` struct?
// i think i like `generics` and `arguments` on a `callable` because they do go
// together in the same way always.  in contrast, `generics` and `arguments` don't
// always mean the same thing for variables; in a declaration, `Array[int]:` means
// `Array: array[int]` but in an expression `Array[3]` means to grab the fourth value.
// we can handle `Array[3]` as `variable { .generics = "[3]" }` but it's not super
// consistent with `Array[3][4]` which would be implicit_member_access on the "[4]".
// if we keep it as is, then we'd have `implicit_member_access`es for both [3] and [4].
test "declaring a variable with arguments and/or generics" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "Set(543).",
        "Array[element_type](1, 2, 3):",
        "Lot[inner_type, at: index4];",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 5, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Set
        Node{ .enclosed = .{ .open = .paren, .root = 3 } },
        Node{ .atomic_token = 5 }, // 543
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 1, .right = 2 } },
        // [5]:
        Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 4 } },
        Node{ .statement = .{ .node = 18, .tab = 0 } },
        Node{ .atomic_token = 11 }, // Array
        Node{ .enclosed = .{ .open = .bracket, .root = 9 } },
        Node{ .callable = .{ .name_token = 15 } }, // element_type
        // [10]:
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 7, .right = 8 } },
        Node{ .enclosed = .{ .open = .paren, .root = 16 } },
        Node{ .atomic_token = 21 }, // 1
        Node{ .atomic_token = 25 }, // 2
        Node{ .binary = .{ .operator = Operator.comma, .left = 12, .right = 13 } },
        // [15]:
        Node{ .atomic_token = 29 }, // 3
        Node{ .binary = .{ .operator = Operator.comma, .left = 14, .right = 15 } },
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 10, .right = 11 } },
        Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 17 } },
        Node{ .statement = .{ .node = 28, .tab = 0 } },
        // [20]:
        Node{ .atomic_token = 35 }, // Lot
        Node{ .enclosed = .{ .open = .bracket, .root = 24 } },
        Node{ .callable = .{ .name_token = 39 } }, // inner_type
        Node{ .callable = .{ .name_token = 43 } }, // at
        Node{ .binary = .{ .operator = Operator.comma, .left = 22, .right = 26 } },
        // [25]:
        Node{ .callable = .{ .name_token = 47 } }, // index4
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 23, .right = 25 } },
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 20, .right = 21 } },
        Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 27 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "simple parentheses, brackets, and braces" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("+(Wow, Great)"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 1, .tab = 0 } },
            Node{ .prefix = .{ .operator = Operator.plus, .node = 2 } },
            Node{ .enclosed = .{ .open = .paren, .root = 5 } },
            Node{ .atomic_token = 5 }, // Wow
            Node{ .atomic_token = 9 }, // Great
            // [5]:
            Node{ .binary = .{ .operator = Operator.comma, .left = 3, .right = 4 } },
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("[wow, jam, time]!"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 7, .tab = 0 } },
            Node{ .enclosed = .{ .open = .bracket, .root = 6 } },
            Node{ .callable = .{ .name_token = 3 } }, // wow
            Node{ .callable = .{ .name_token = 7 } }, // jam
            Node{ .binary = .{ .operator = Operator.comma, .left = 2, .right = 3 } },
            // [5]:
            Node{ .callable = .{ .name_token = 11 } }, // time
            Node{ .binary = .{ .operator = Operator.comma, .left = 4, .right = 5 } },
            Node{ .postfix = .{ .operator = Operator.not, .node = 1 } },
            .end,
        });
        // No errors in attempts to parse `callable`.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "[wow, jam, time]!",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("{Boo: 33, hoo: 123 + 44}-57"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 12, .tab = 0 } },
            Node{ .enclosed = .{ .open = .brace, .root = 6 } },
            Node{ .atomic_token = 3 }, // Boo
            Node{ .atomic_token = 7 }, // 33
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 3 } },
            // [5]:
            Node{ .callable = .{ .name_token = 11 } }, // hoo
            Node{ .binary = .{ .operator = Operator.comma, .left = 4, .right = 8 } },
            Node{ .atomic_token = 15 }, // 123
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 5, .right = 10 } },
            Node{ .atomic_token = 19 }, // 44
            // [10]:
            Node{ .binary = .{ .operator = Operator.plus, .left = 7, .right = 9 } },
            Node{ .atomic_token = 25 }, // 57
            Node{ .binary = .{ .operator = Operator.minus, .left = 1, .right = 11 } },
            .end,
        });
        // Attempts to parse generics or arguments for `callable` don't add errors:
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "{Boo: 33, hoo: 123 + 44}-57",
        });
    }
}

test "trailing commas are ok" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("(C0, C1, C2,)"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 1, .tab = 0 } },
            Node{ .enclosed = .{ .open = .paren, .root = 7 } },
            Node{ .atomic_token = 3 }, // C0
            Node{ .atomic_token = 7 }, // C1
            Node{ .binary = .{ .operator = Operator.comma, .left = 2, .right = 3 } },
            // [5]:
            Node{ .atomic_token = 11 }, // C2
            Node{ .binary = .{ .operator = Operator.comma, .left = 4, .right = 5 } },
            Node{ .postfix = .{ .operator = Operator.comma, .node = 6 } },
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("[Bk0, Bk1,]-761"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 7, .tab = 0 } },
            Node{ .enclosed = .{ .open = .bracket, .root = 5 } },
            Node{ .atomic_token = 3 }, // Bk0
            Node{ .atomic_token = 7 }, // Bk1
            Node{ .binary = .{ .operator = Operator.comma, .left = 2, .right = 3 } },
            // [5]:
            Node{ .postfix = .{ .operator = Operator.comma, .node = 4 } },
            Node{ .atomic_token = 15 }, // 761
            Node{ .binary = .{ .operator = Operator.minus, .left = 1, .right = 6 } },
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("{Bc0,}+Abcdef"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 5, .tab = 0 } },
            Node{ .enclosed = .{ .open = .brace, .root = 3 } },
            Node{ .atomic_token = 3 }, // Bc0
            Node{ .postfix = .{ .operator = Operator.comma, .node = 2 } },
            Node{ .atomic_token = 11 }, // Abcdef
            // [5]:
            Node{ .binary = .{ .operator = Operator.plus, .left = 1, .right = 4 } },
            .end,
        });
    }
}

test "parser declare" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("Whatever: type1"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 3, .tab = 0 } },
            Node{ .atomic_token = 1 }, // Whatever
            Node{ .callable = .{ .name_token = 5 } }, // type1
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 1, .right = 2 } },
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("Writable_whatever; type2"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 3, .tab = 0 } },
            Node{ .atomic_token = 1 }, // Writable_whatever
            Node{ .callable = .{ .name_token = 5 } }, // type2
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 1, .right = 2 } },
            .end,
        });
    }
}

test "parser declare and assign" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("Declassign: type_assign1 = 12345"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 5, .tab = 0 } },
            Node{ .atomic_token = 1 }, // Declassign
            Node{ .callable = .{ .name_token = 5 } }, // type_assign1
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 1, .right = 2 } },
            Node{ .atomic_token = 9 }, // 12345
            // [5]:
            Node{ .binary = .{ .operator = Operator.assign, .left = 3, .right = 4 } },
            .end,
        });
        // No errors in attempts to parse `callable`.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "Declassign: type_assign1 = 12345",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("Oh_writable; type_assign2 = 7890"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 5, .tab = 0 } },
            Node{ .atomic_token = 1 }, // Declassign
            Node{ .callable = .{ .name_token = 5 } }, // type_assign2
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 1, .right = 2 } },
            Node{ .atomic_token = 9 }, // 7890
            // [5]:
            Node{ .binary = .{ .operator = Operator.assign, .left = 3, .right = 4 } },
            .end,
        });
        // No errors in attempts to parse `callable`.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "Oh_writable; type_assign2 = 7890",
        });
    }
}

test "parser declare and nested assigns" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("D1: D2; D3"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 3, .tab = 0 } },
            Node{ .atomic_token = 1 }, // D1
            Node{ .atomic_token = 5 }, // D2
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 1, .right = 5 } },
            Node{ .atomic_token = 9 }, // D3
            // [5]:
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 2, .right = 4 } },
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("X3 = Y4 = 750"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 3, .tab = 0 } },
            Node{ .atomic_token = 1 }, // X3
            Node{ .atomic_token = 5 }, // Y4
            Node{ .binary = .{ .operator = Operator.assign, .left = 1, .right = 5 } },
            Node{ .atomic_token = 9 }, // 750
            // [5]:
            Node{ .binary = .{ .operator = Operator.assign, .left = 2, .right = 4 } },
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("VarQ; i32 = Qu16 = VarU: i16 = 750"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .statement = .{ .node = 5, .tab = 0 } },
            Node{ .atomic_token = 1 }, // VarQ
            Node{ .callable = .{ .name_token = 5 } }, // i32
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 1, .right = 2 } },
            Node{ .atomic_token = 9 }, // Qu16
            // [5]:
            Node{ .binary = .{ .operator = Operator.assign, .left = 3, .right = 7 } },
            Node{ .atomic_token = 13 }, // VarU
            Node{ .binary = .{ .operator = Operator.assign, .left = 4, .right = 11 } },
            Node{ .callable = .{ .name_token = 17 } }, // i16
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 6, .right = 8 } },
            // [10]:
            Node{ .atomic_token = 21 }, // 750
            Node{ .binary = .{ .operator = Operator.assign, .left = 9, .right = 10 } },
            .end,
        });
    }
}

// TODO: error tests, e.g., "cannot postfix this"
