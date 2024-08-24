const common = @import("common.zig");
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

pub const Parser = struct {
    // The parser will free this at the end.
    tokenizer: Tokenizer = .{},
    nodes: OwnedNodes = OwnedNodes.init(),
    statement_indices: OwnedNodeIndices = OwnedNodeIndices.init(),
    valid_statement_count: usize = std.math.maxInt(usize),
    farthest_token_index: usize = 0,

    pub fn deinit(self: *Self) void {
        self.tokenizer.deinit();
        self.statement_indices.deinit();
        self.nodes.deinit();
    }

    pub fn at(self: *Self, statement_index: usize) ParserError!Node.Statement {
        if (statement_index >= self.valid_statement_count) {
            return ParserError.out_of_statements;
        }
        while (statement_index >= self.statement_indices.count()) {
            try self.addNextStatement();
            if (statement_index >= self.valid_statement_count) {
                return ParserError.out_of_statements;
            }
        }
        const node_index = self.statement_indices.at(statement_index) orelse {
            return ParserError.broken_invariant;
        };
        return switch (self.nodes.inBounds(node_index)) {
            .statement => |statement| statement,
            else => ParserError.broken_invariant,
        };
    }

    pub fn complete(self: *Self) ParserError!void {
        while (self.valid_statement_count > self.statement_indices.count()) {
            self.addNextStatement() catch |err| {
                if (err == ParserError.out_of_statements) {
                    break;
                }
                return err;
            };
        }
    }

    fn addNextStatement(self: *Self) ParserError!void {
        errdefer {
            self.valid_statement_count = self.statement_indices.count();
        }
        // So that `node == 0` appears to be invalid, append the
        // statement first, then its child nodes.  Only update
        // `statement_indices` after success.  Notice that this
        // will make the first statement appear to be invalid,
        // but only if it would be cross referenced in another
        // node, which it shouldn't be since it's the first.
        const statement_node_index = try self.justAppendNode(.end);
        self.nodes.set(statement_node_index, Node{
            .statement = try self.getNextStatement(),
        }) catch unreachable;
        self.statement_indices.append(statement_node_index) catch {
            return ParserError.out_of_memory;
        };
    }

    fn getNextStatement(self: *Self) ParserError!Node.Statement {
        const tab = switch (try self.peekToken(0)) {
            .spacing => |spacing| spacing.absolute,
            .end => return ParserError.out_of_statements,
            else => return ParserError.broken_invariant,
        };

        const node_index = try self.appendNextExpression(tab);

        return .{ .tab = tab, .node = node_index };
    }

    fn appendNextExpression(self: *Self, tab: u16) ParserError!NodeIndex {
        // The current left node which can interact with the next operation,
        // is the last element of `hierarchy`, with nesting all the way up
        // to the "root node" (`hierarchy.inBounds(0)`) which should be returned.
        var hierarchy = OwnedNodeIndices.init();
        _ = try self.appendNextStandaloneExpression(&hierarchy, tab);
        defer hierarchy.deinit();

        while (true) {
            const operation = try self.seekNextOperation(tab);
            if (operation.operator == .none) {
                try self.assertAndConsumeNextTokenIf(.newline, "expected newline");
                return hierarchy.inBounds(0);
            }
            if (operation.type == .postfix) {
                try self.appendPostfixOperation(&hierarchy, operation);
            } else {
                const right_index = try self.appendNextStandaloneExpression(&hierarchy, tab);
                try self.appendInfixOperation(&hierarchy, operation, right_index);
            }
        }
    }

    // Returns the next postfix or infix operation.
    // Prefix operations are taken care of inside of `appendNextStandaloneExpression`.
    fn seekNextOperation(self: *Self, tab: u16) ParserError!Operation {
        switch (try self.peekToken(0)) {
            // This was the last atom in the row.
            .newline => {
                // TODO: check if we had a double-indent on the next line and continue parsing if so.
                _ = tab;
                return .{ .operator = .none };
            },
            .end => return .{ .operator = .none },
            else => try self.assertAndConsumeNextTokenIf(.spacing, expected_spacing),
        }

        switch (try self.peekToken(0)) {
            .operator => |operator| {
                if (operator.isInfixable()) {
                    self.farthest_token_index += 1;
                    return .{ .operator = operator, .type = .infix };
                } else if (operator.isPostfixable()) {
                    self.farthest_token_index += 1;
                    return .{ .operator = operator, .type = .postfix };
                } else {
                    std.debug.assert(operator.isPrefixable());
                    // Back up so that the next standalone expression starts at the
                    // spacing before this prefix:
                    self.farthest_token_index -= 1;
                    // Pretend that we have an operator before this prefix.
                    return .{ .operator = .implicit_member_access, .type = .infix };
                }
            },
            else => {
                // We encountered another realizable token, back up so that
                // we maintain the invariant that there's a space before the next real element.
                self.farthest_token_index -= 1;
                return .{ .operator = .implicit_member_access, .type = .infix };
            },
        }

        (try self.peekToken(0)).printLine(common.debugStderr) catch {};
        self.addTokenizerError("expected end of line");
        return ParserError.unimplemented;
    }

    /// Adds an atom with possible prefix (but NOT postfix) operators.
    /// Includes things like `1.234`, `My_variable`, `+4.56`, `-7.89`,
    /// `++Index` or `!Countdown` as well.  For member access like
    /// `First_identifier Second_identifier`, just grab the first one.
    /// NOTE: do NOT add the returned index into `hierarchy`, we'll do that for you.
    // TODO: also include operations like `my_function(...)`
    fn appendNextStandaloneExpression(self: *Self, hierarchy: *OwnedNodeIndices, tab: u16) ParserError!NodeIndex {
        try self.assertAndConsumeNextTokenIf(.spacing, expected_spacing);

        switch (try self.peekToken(0)) {
            .starts_upper, .number => {
                const atomic_index = try self.justAppendNode(Node{
                    .atomic_token = self.farthest_token_index,
                });
                self.farthest_token_index += 1;

                hierarchy.append(atomic_index) catch return ParserError.out_of_memory;
                return atomic_index;
            },
            .operator => |operator| {
                if (!operator.isPrefixable()) {
                    self.addTokenizerError("not a prefix operator");
                    return ParserError.syntax;
                }
                common.debugStderr.print("appending prefix, breaking invariant with operator ", .{}) catch {};
                operator.printLine(common.debugStderr) catch {};
                self.farthest_token_index += 1;
                const prefix_index = try self.justAppendNode(Node{
                    .prefix = .{
                        .operator = operator,
                        // This breaks the invariant, but we need to append to `hierarchy` first.
                        // Then we need to append the next standalone expression.
                        // TODO: this may be bad because `hierarchy` may try to swap this out somewhere.
                        //       this may need to be a unique "negative" number that we hunt for
                        //       later and replace.
                        //  ALTERNATIVELY: we should pass in a `appendNextExpressionUpTo(operator_precedence)`
                        //  and then we know where we're supposed to put the prefix operator.
                        .node = 0,
                    },
                });
                hierarchy.append(prefix_index) catch return ParserError.out_of_memory;
                const next_index = try self.appendNextStandaloneExpression(hierarchy, tab);
                switch (self.nodes.items()[prefix_index]) {
                    // restore the invariant.
                    .prefix => |*prefix| {
                        prefix.node = next_index;
                        common.debugStderr.print("restoring prefix invariant ", .{}) catch {};
                        self.nodes.items()[prefix_index].printLine(common.debugStderr) catch {};
                    },
                    else => return ParserError.broken_invariant,
                }
                return prefix_index;
            },
            else => {
                self.addTokenizerError("expected an expression");
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
            const left = &self.nodes.items()[left_index];
            const left_operation = left.operation();
            // lower precedence means higher priority.
            if (left_operation.isPostfix() or left_operation.precedence(Operation.Compare.on_left) <= operation_precedence) {
                // `left` has higher priority; we should continue up the hierarchy
                // until we find the spot that this new operation should take.
                _ = hierarchy.pop();
            } else {
                // `operation` has higher priority, so we need to invert the nodes a bit.
                // break an invariant here:
                const inner_index = left.swapRight(0) catch {
                    self.addTokenizerError("cannot postfix this");
                    common.debugStderr.print("trying to append postfix but got ", .{}) catch {};
                    left.printLine(common.debugStderr) catch {};
                    self.printTokenDebugInfo();
                    return ParserError.unimplemented;
                };
                const next_index = try self.justAppendNode(.{ .postfix = .{
                    .operator = operation.operator,
                    .node = inner_index,
                } });
                // restore the invariant:
                _ = left.swapRight(next_index) catch unreachable;
                common.debugStderr.print("\nhierarchy so far, adding {d} and {d}: ", .{ next_index, inner_index }) catch {};
                hierarchy.printLine(common.debugStderr) catch {};
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
            const left = &self.nodes.items()[left_index];
            const left_operation = left.operation();
            // lower precedence means higher priority.
            if (left_operation.isPostfix() or left_operation.precedence(Operation.Compare.on_left) <= operation_precedence) {
                // `left` has higher priority; we should continue up the hierarchy
                // until we find the spot that this new operation should take.
                _ = hierarchy.pop();
            } else {
                // `operation` has higher priority, so we need to invert the nodes a bit.
                // break an invariant here:
                const inner_index = left.swapRight(0) catch {
                    self.addTokenizerError("cannot right-operate on this");
                    common.debugStderr.print("trying to append infix but got ", .{}) catch {};
                    left.printLine(common.debugStderr) catch {};
                    self.printTokenDebugInfo();
                    return ParserError.unimplemented;
                };
                const next_index = try self.justAppendNode(.{ .binary = .{
                    .operator = operation.operator,
                    .left = inner_index,
                    .right = right_index,
                } });
                // restore the invariant:
                _ = left.swapRight(next_index) catch unreachable;
                // Fix up the hierarchy at the end:
                common.debugStderr.print("\nhierarchy so far, adding {d} and {d}: ", .{ next_index, inner_index }) catch {};
                hierarchy.printLine(common.debugStderr) catch {};
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
        const token = try self.peekToken(0);
        self.farthest_token_index += 1;
        return token;
    }

    fn peekToken(self: *Self, delta: usize) ParserError!Token {
        return self.tokenizer.at(self.farthest_token_index + delta) catch {
            // TODO: probably should distinguish between out of memory (rethrow)
            // and out of tokens => out_of_statements
            return ParserError.out_of_statements;
        };
    }

    fn assertAndConsumeNextTokenIf(self: *Self, expected_tag: Token.Tag, error_message: []const u8) ParserError!void {
        const next_token = try self.peekToken(0);
        if (next_token.tag() == expected_tag) {
            self.farthest_token_index += 1;
            return;
        }
        common.debugStderr.print("actual tag is {d}\n", .{@intFromEnum(next_token.tag())}) catch {};
        self.addTokenizerError(error_message);
        return ParserError.syntax;
    }

    const Self = @This();

    fn addTokenizerError(self: *Self, error_message: []const u8) void {
        self.tokenizer.addErrorAt(self.farthest_token_index, error_message);
    }

    pub fn printTokenDebugInfo(self: *Self) void {
        self.tokenizer.printDebugInfoAt(self.farthest_token_index);
    }
};

const expected_spacing = "expected spacing between each identifier";

test "parser simple expressions" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("3.456"));
    try parser.tokenizer.file.lines.append(try SmallString.init("    Hello_you"));
    try parser.tokenizer.file.lines.append(try SmallString.init("+1.234"));
    try parser.tokenizer.file.lines.append(try SmallString.init("  -5.678"));
    try parser.tokenizer.file.lines.append(try SmallString.init("    ++Foe"));
    try parser.tokenizer.file.lines.append(try SmallString.init("        Fum--"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 1, .tab = 0 } },
        Node{ .atomic_token = 1 }, // 3.456
        Node{ .statement = .{ .node = 3, .tab = 4 } },
        Node{ .atomic_token = 4 }, // Hello_you
        Node{ .statement = .{ .node = 5, .tab = 0 } },
        // [5]:
        Node{ .prefix = .{ .operator = Operator.plus, .node = 6 } },
        Node{ .atomic_token = 9 }, // 1.234
        Node{ .statement = .{ .node = 8, .tab = 2 } },
        Node{ .prefix = .{ .operator = Operator.minus, .node = 9 } },
        Node{ .atomic_token = 14 }, // 5.678
        // [10]:
        Node{ .statement = .{ .node = 11, .tab = 4 } },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 12 } },
        Node{ .atomic_token = 19 }, // Foe
        Node{ .statement = .{ .node = 15, .tab = 8 } },
        Node{ .atomic_token = 22 }, // Fum
        // [15]:
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 14 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        2,
        4,
        7,
        10,
        13,
    });
    try std.testing.expectEqual(Node.Statement{ .node = 1, .tab = 0 }, try parser.at(0));
    try std.testing.expectEqual(Node.Statement{ .node = 3, .tab = 4 }, try parser.at(1));
    try std.testing.expectEqual(Node.Statement{ .node = 5, .tab = 0 }, try parser.at(2));
    try std.testing.expectEqual(Node.Statement{ .node = 8, .tab = 2 }, try parser.at(3));
    try std.testing.expectEqual(Node.Statement{ .node = 11, .tab = 4 }, try parser.at(4));
    try std.testing.expectEqual(Node.Statement{ .node = 15, .tab = 8 }, try parser.at(5));
}

test "parser multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
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
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
    });
}

test "parser simple (and postfix) implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
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
        Node{ .atomic_token = 6 }, // Sci
        Node{ .atomic_token = 8 }, // Fi
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 5, .right = 6 } },
        Node{ .postfix = .{ .operator = Operator.increment, .node = 7 } },
        Node{ .statement = .{ .node = 15, .tab = 0 } },
        // [10]:
        Node{ .atomic_token = 13 }, // Kite
        Node{ .atomic_token = 15 }, // Sty
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 10, .right = 11 } },
        Node{ .atomic_token = 17 }, // Five
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 12, .right = 13 } },
        // [15]:
        Node{ .postfix = .{ .operator = Operator.not, .node = 14 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        4,
        9,
    });
}

test "parser complicated (and prefix) implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    // TODO:
    // try parser.tokenizer.file.lines.append(try SmallString.init("--Why Shy Spy"));
    // try parser.tokenizer.file.lines.append(try SmallString.init("!Chai Lie Fry"));
    // try parser.tokenizer.file.lines.append(try SmallString.init("!Knife Fly Nigh!"));
}

test "simple prefix/postfix operators with multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("++Theta * Beta"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Zeta * ++Woga"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Yodus-- * Spatula"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Wobdash * Flobsmash--"));

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
        Node{ .atomic_token = 10 }, // Zeta
        Node{ .prefix = .{ .operator = Operator.increment, .node = 8 } },
        Node{ .atomic_token = 16 }, // Woga
        Node{ .binary = .{ .operator = Operator.multiply, .left = 6, .right = 7 } },
        // [10]:
        Node{ .statement = .{ .node = 14, .tab = 0 } },
        Node{ .atomic_token = 19 }, // Yodus
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 11 } },
        Node{ .atomic_token = 25 }, // Spatula
        Node{ .binary = .{ .operator = Operator.multiply, .left = 12, .right = 13 } },
        // [15]:
        Node{ .statement = .{ .node = 18, .tab = 0 } },
        Node{ .atomic_token = 28 }, // Wobdash
        Node{ .atomic_token = 32 }, // Flobsmash
        Node{ .binary = .{ .operator = Operator.multiply, .left = 16, .right = 19 } },
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 17 } },
        // [20]:
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        5,
        10,
        15,
    });
}

test "complicated prefix/postfix operators with addition/multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Apple * !Berry Cantaloupe-- + 5"));
    try parser.tokenizer.file.lines.append(try SmallString.init("--Xeno Yak! + 3 * Zelda"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 9, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Apple
        Node{ .prefix = .{ .operator = Operator.not, .node = 7 } },
        Node{ .atomic_token = 7 }, // Berry
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 6 } },
        // [5]:
        Node{ .atomic_token = 9 }, // Cantaloupe
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 2, .right = 5 } },
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 3 } },
        Node{ .atomic_token = 15 }, // 5
        Node{ .binary = .{ .operator = Operator.plus, .left = 4, .right = 8 } }, // TODO: .right should be the `not` (node 2)
        // [10]:
        Node{ .statement = .{ .node = 17, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 15 } },
        Node{ .atomic_token = 20 }, // Xeno
        Node{ .atomic_token = 22 }, // Yak
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 12, .right = 13 } },
        // [15]:
        Node{ .postfix = .{ .operator = Operator.not, .node = 14 } },
        Node{ .atomic_token = 28 }, // 3
        Node{ .binary = .{ .operator = Operator.plus, .left = 11, .right = 19 } },
        Node{ .atomic_token = 32 }, // Zelda
        Node{ .binary = .{ .operator = Operator.multiply, .left = 16, .right = 18 } },
        // [20]:
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        10,
    });
}

test "nested prefix/postfix operators" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Abc Xyz-- !"));
    try parser.tokenizer.file.lines.append(try SmallString.init("! ++Def Uvw"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Yammer * Zen++ !"));
    try parser.tokenizer.file.lines.append(try SmallString.init("! --Oh Great * Hessian"));

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
        Node{ .atomic_token = 14 }, // Def
        // [10]:
        Node{ .atomic_token = 16 }, // Uvw
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 9, .right = 10 } },
        Node{ .statement = .{ .node = 15, .tab = 0 } },
        Node{ .atomic_token = 19 }, // Yammer
        Node{ .atomic_token = 23 }, // Zen
        // [15]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 13, .right = 17 } },
        Node{ .postfix = .{ .operator = Operator.increment, .node = 14 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 16 } },
        Node{ .statement = .{ .node = 25, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 20 } },
        // [20]:
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 23 } },
        Node{ .atomic_token = 34 }, // Oh
        Node{ .atomic_token = 36 }, // Great
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 21, .right = 22 } },
        Node{ .atomic_token = 40 }, // Hessian
        // [25]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 19, .right = 24 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        6,
        12,
        18,
    });
}

test "order of operations with addition and multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
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
        Node{ .atomic_token = 12 }, // Panko
        Node{ .atomic_token = 16 }, // K_panko
        Node{ .binary = .{ .operator = Operator.plus, .left = 7, .right = 11 } },
        // [10]:
        Node{ .atomic_token = 20 }, // 1000
        Node{ .binary = .{ .operator = Operator.multiply, .left = 8, .right = 10 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        6,
    });
}
