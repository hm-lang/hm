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
        _ = try self.justAppendNode(.end);
    }

    fn appendNextEnclosed(self: *Self, tab: u16, open: Open) ParserError!NodeIndex {
        // So that we go roughly in order, append the enclosed first, then child nodes.
        // but we don't know what it is yet, so just make a placeholder.
        const enclosed_node_index = try self.justAppendNode(.end);

        const enclosed_node = try if (open.isQuote())
            self.getNextEnclosedQuote(tab, open, enclosed_node_index)
        else
            self.getNextEnclosedBlock(tab, open, enclosed_node_index);

        self.nodes.set(enclosed_node_index, .{ .enclosed = enclosed_node }) catch unreachable;

        return enclosed_node_index;
    }

    fn getNextEnclosedBlock(self: *Self, tab: u16, open: Open, enclosed_node_index: NodeIndex) ParserError!Node.Enclosed {
        _ = enclosed_node_index;
        var enclosed_start_index: usize = 0;
        var previous_statement_index: usize = 0;
        var until_triggered = false;
        while (self.getSameBlockNextNodeIndex(tab)) |start_parsing_index| {
            self.farthest_token_index = start_parsing_index;
            const statement_result = self.appendNextStatement(tab, Until.closing(open), .only_try) catch {
                // There wasn't another statement here.
                // We don't update any previous statements here because we didn't successfully add one here.
                // This can be for multiple reasons, including good ones (e.g., trailing commas or declarations).
                break;
            };
            const current_statement_index = statement_result.node;
            until_triggered = statement_result.until_triggered;
            if (enclosed_start_index == 0) {
                enclosed_start_index = current_statement_index;
            } else {
                self.nodes.items()[previous_statement_index].setStatementNext(current_statement_index) catch {
                    return ParserError.broken_invariant;
                };
            }
            previous_statement_index = current_statement_index;
            if (until_triggered) {
                break;
            }
        }
        if (!until_triggered and open != .none) {
            try self.consumeCloseMatching(open);
        }

        // TODO: add tab to `Node.Statement` so that we can tell if it's an indented block
        //      by looking at the first statement's tab.
        return .{
            .tab = tab,
            .open = open,
            .start = enclosed_start_index,
        };
    }

    fn consumeCloseMatching(self: *Self, open: Open) ParserError!void {
        errdefer {
            self.addTokenizerError(Token.InvalidType.expected_close(open).error_message());
        }
        while (true) switch (try self.peekToken()) {
            .spacing => self.farthest_token_index += 1,
            .close => |close| {
                if (close == open) {
                    self.farthest_token_index += 1;
                    return;
                } else {
                    return ParserError.syntax;
                }
            },
            else => return ParserError.syntax,
        };
    }

    fn getNextEnclosedQuote(self: *Self, tab: u16, open: Open, enclosed_node_index: NodeIndex) ParserError!Node.Enclosed {
        _ = self;
        _ = tab;
        _ = open;
        _ = enclosed_node_index;
        return ParserError.unimplemented;
    }

    fn appendNextStatement(self: *Self, tab: u16, until: Until, or_else: OrElse) ParserError!NodeResult {
        // To make nodes mostly go in order, append the node first.
        const statement_index = try self.justAppendNode(.end);
        errdefer {
            // A downside of going in order is that we need a bit of cleanup.
            // Only clean up if we don't think it'll wreck any other nodes.
            if (self.nodes.count() == statement_index + 1) {
                _ = self.nodes.remove(statement_index);
            }
        }

        const result = try self.appendNextExpression(tab, until, or_else);

        self.nodes.set(statement_index, Node{ .statement = .{
            .node = result.node,
        } }) catch unreachable;

        return result.withNode(statement_index);
    }

    /// Supports starting with spacing *or not* (e.g., for the start of a statement
    /// where we don't want to check the indent yet).
    fn appendNextExpression(self: *Self, tab: u16, until: Until, or_else: OrElse) ParserError!NodeResult {
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
            const result = try self.seekNextOperation(tab, until);
            const operation = result.operation;
            switch (operation.operator) {
                .none => return result.toNode(hierarchy.inBounds(0)),
                .comma => {
                    // Commas are so low in priority that we split off statements
                    // so that we can go roughly in left-to-right order without
                    // depth-first-searching for the first left node.
                    return result.toNode(hierarchy.inBounds(0));
                },
                else => {},
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
                            return NodeResult.triggered(hierarchy.inBounds(0));
                        }
                        self.farthest_token_index -= 1;
                    },
                    .spacing => {
                        // The only way we can get here is if we newline'd it;
                        // keep the farthest_token_index steady.
                    },
                    .file_end => return NodeResult.notTriggered(hierarchy.inBounds(0)),
                    else => {
                        self.farthest_token_index -= 1;
                    },
                }
            }
        }
    }

    // Returns the next postfix or infix operation.
    // Prefix operations are taken care of inside of `appendNextStandaloneExpression`.
    fn seekNextOperation(self: *Self, tab: u16, until: Until) ParserError!OperationResult {
        const restore_index = self.farthest_token_index;

        var operation_tabbed = self.getSameStatementNextTabbed(tab) orelse {
            return OperationResult.notTriggered(.{ .operator = .none }, tab);
        };
        self.farthest_token_index = operation_tabbed.start_parsing_index;

        var operation: Operation = switch (try self.peekToken()) {
            .operator => |operator| blk: {
                if (operator.isInfixable()) {
                    self.farthest_token_index += 1;
                    if (operation_tabbed.tab > tab and (try self.peekToken()).isAbsoluteSpacing(tab + 8)) {
                        // This is a line continuation, e.g.,
                        //&|    Some_variable
                        //&|        +   Some_other_variable1
                        //&|        -   Some_other_variable2
                        operation_tabbed.tab = tab;
                    }
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
                    break :blk .{ .operator = .access, .type = .infix };
                }
            },
            .close => |close| blk: {
                if (until.shouldBreakAtClose(close)) {
                    self.farthest_token_index += 1;
                    return operation_tabbed.toTriggeredOperation(.{ .operator = .none });
                }
                // Same as the `else` block below:
                self.farthest_token_index -= 1;
                break :blk .{ .operator = .access, .type = .infix };
            },
            else => blk: {
                // We encountered another realizable token, back up so that
                // we maintain the invariant that there's a space before the next real element.
                self.farthest_token_index -= 1;
                break :blk .{ .operator = .access, .type = .infix };
            },
        };

        if (operation_tabbed.tab == tab + 4) {
            // This is an indented operation, and we didn't see another indent past it.
            //&|    Some_expression
            //&|        +enclosed
            // this is probably a syntax error, but we'll parse it as `Some_expression` `access` `+enclosed`
            self.farthest_token_index = restore_index;
            operation = .{ .operator = .indent, .type = .infix };
            self.farthest_token_index = restore_index;
        }

        if (until.shouldBreakBeforeOperation(operation)) {
            self.farthest_token_index = restore_index;
            return operation_tabbed.toTriggeredOperation(.{ .operator = .none });
        }
        return operation_tabbed.toNotTriggeredOperation(operation);
    }

    /// Adds an atom with possible prefix (but NOT postfix) operators.
    /// Includes things like `1.234`, `My_variable`, `+4.56`, `-7.89`,
    /// `++Index` or `!Countdown` as well.  For member access like
    /// `First_identifier Second_identifier`, just grab the first one.
    /// NOTE: do NOT add the returned index into `hierarchy`, we'll do that for you.
    /// Supports starting with spacing *or not* (e.g., for the start of a statement).
    fn appendNextStandaloneExpression(self: *Self, hierarchy: *OwnedNodeIndices, tab: u16, or_else: OrElse) ParserError!NodeIndex {
        const next_tabbed = self.getSameStatementNextTabbed(tab) orelse {
            const had_expression_token = false;
            self.assertSyntax(had_expression_token, or_else.map(expected_spacing)) catch {};
            return ParserError.syntax;
        };
        common.debugPrint("was at tab {d} -> {d}\n", .{ tab, next_tabbed.tab });
        self.debugTokens();
        self.farthest_token_index = next_tabbed.start_parsing_index;
        if (next_tabbed.tab > tab) {
            const enclosed_index = try self.appendNextEnclosed(next_tabbed.tab, .none);
            hierarchy.append(enclosed_index) catch return ParserError.out_of_memory;
            return enclosed_index;
        }

        switch (try self.peekToken()) {
            .starts_upper, .number => {
                const atomic_index = try self.justAppendNode(Node{
                    .atomic_token = self.farthest_token_index,
                    // TODO: .tab = try self.tokenizerTab(),
                });
                self.farthest_token_index += 1;

                hierarchy.append(atomic_index) catch return ParserError.out_of_memory;
                return atomic_index;
            },
            .starts_lower => {
                const callable_index = try self.justAppendNode(Node{
                    .callable_token = self.farthest_token_index,
                    // TODO: .tab = try self.tokenizerTab(),
                });
                self.farthest_token_index += 1;

                hierarchy.append(callable_index) catch return ParserError.out_of_memory;
                return callable_index;
            },
            .open => |open| {
                self.farthest_token_index += 1;
                const enclosed_index = try self.appendNextEnclosed(tab, open);
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
                // We're assuming that prefix operators should not break at an operator using an `Until` here.
                // We need every operation *stronger* than this prefix to be attached to this prefix.
                // TODO: add `tab` to the StatementNode so we can see if it's been indented
                const inner_result = try self.appendNextExpression(tab, Until.prefix_strength_wins(operator), or_else);
                switch (self.nodes.items()[prefix_index]) {
                    // restore the invariant:
                    .prefix => |*prefix| {
                        prefix.node = inner_result.node;
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

    /// For inside a block, the next statement index to continue with
    fn getSameBlockNextNodeIndex(self: *Self, tab: u16) ?NodeIndex {
        // TODO: ignore comments as well
        switch (self.peekToken() catch return null) {
            .file_end => return null,
            //&|MyValue:
            //&|        +SomeValue  # keep prefix operators at tab indent
            //&|    -   CoolStuff   # infix operator should be ignored
            .spacing => |spacing| if (spacing.getNewlineTab()) |newline_tab| {
                if (newline_tab % 4 != 0) {
                    self.addTokenizerError(expected_four_space_indents);
                    return null;
                }
                if (newline_tab < tab) {
                    return null;
                } else {
                    return self.farthest_token_index + 1;
                }
            } else if (spacing.absolute < tab) {
                return null;
            } else {
                // Not a newline, just continuing in the same statement
                return self.farthest_token_index + 1;
            },
            // Assume that anything else is already at the correct tab.
            else => return self.farthest_token_index,
        }
    }

    /// For inside a statement, the next index that we should continue with.
    fn getSameStatementNextTabbed(self: *Self, tab: u16) ?Tabbed {
        return self.getSameStatementNextTabbedAt(self.farthest_token_index, tab);
    }

    fn getSameStatementNextTabbedAt(self: *Self, token_index: TokenIndex, tab: u16) ?Tabbed {
        // TODO: ignore comments
        switch (self.tokenAt(token_index) catch return null) {
            .file_end => return null,
            .spacing => |spacing| if (spacing.getNewlineTab()) |newline_tab| {
                if (newline_tab % 4 != 0) {
                    self.addTokenizerError(expected_four_space_indents);
                    return null;
                }
                if (newline_tab > tab) {
                    return .{
                        .start_parsing_index = token_index + 1,
                        // Check if we're just a line continuation:
                        .tab = if (newline_tab >= tab + 8) tab else newline_tab,
                    };
                }
                if (newline_tab < tab) {
                    // Going lower in indent should not continue here.
                    return null;
                }
                // handle newlines with no indent especially below.
            } else {
                return .{
                    .start_parsing_index = token_index + 1,
                    .tab = if (self.getHorstmannTabAt(token_index, tab)) |h_tab| h_tab else tab,
                };
            },
            else => {
                return .{
                    .start_parsing_index = token_index,
                    .tab = if (self.getHorstmannTabAt(token_index - 1, tab)) |h_tab| h_tab else tab,
                };
            },
        }
        // newlines are special due to Horstmann braces.
        if (self.isHorstmannTabbedGoingForward(token_index + 1, tab)) {
            common.debugPrint("found horstmann indent\n", .{});
            return .{ .start_parsing_index = token_index + 1, .tab = tab };
        } else {
            return null;
        }
    }

    fn isHorstmannTabbedGoingForward(self: *Self, starting_token_index: usize, tab: u16) bool {
        common.debugPrint("ok looking for a horstmann indent at {d}\n", .{starting_token_index});
        var token_index = starting_token_index;
        // TODO: we probably can relax the 3 parentheses limit but we need to bump the tab.
        //&|my_function():
        //&|    return
        //&|    [[[[    my_indent1
        //&|            my_indent2
        //&|    ]]]]
        for (0..3) |closes| {
            common.debugPrint("ok looking for a horstmann indent ", self.peekToken() catch .file_end);
            _ = closes;
            if (!(self.tokenAt(token_index) catch return false).isMirrorOpen()) {
                return false;
            }
            common.debugPrint("ok checking space for a horstmann indent ", self.peekToken() catch .file_end);
            token_index += 1;
            switch (self.tokenAt(token_index) catch return false) {
                .file_end => return false,
                .spacing => |spacing| if (spacing.isNewline()) {
                    // we don't want to support horstmann like this:
                    //&|    my_function(): array[array[int]]
                    //&|        return
                    //&|        [
                    //&|        [   5, 6, 7]
                    //&|        [   8
                    //&|            9
                    //&|            10
                    //&|        ]
                    //&|        ]
                    // the correct syntax would be something like this:
                    //&|    my_function(): array[array[int]]
                    //&|        return
                    //&|        [   [5, 6, 7]
                    //&|            [   8
                    //&|                9
                    //&|                10
                    //&|            ]
                    //&|        ]
                    // so if we encounter a newline just bail:
                    // TODO: we probably can support this, although we should format.
                    //&|    my_function(): array[array[int]]
                    //&|        return
                    //&|        [
                    //&|        ]
                    return false;
                } else if (spacing.relative > 0 and spacing.absolute % 4 == 0) {
                    // we do want to support this, however:
                    //&|    my_function(): array[array[int]]
                    //&|        return
                    //&|        [[  5
                    //&|            6
                    //&|            7
                    //&|        ]]
                    // but only up to three braces, e.g.,
                    //&|        [{( 5
                    //&|        )}]
                    // because otherwise we won't be able to trigger on `spacing.relative > 0`.
                    if (spacing.absolute >= tab + 4) {
                        // line continuation in some form.  we'll let the compiler figure
                        // out the true tab later.
                        return true;
                    }
                } else {
                    return false;
                },
                else => return false,
            }
            token_index += 1;
        }
        return false;
    }

    /// This returns the Horstmann tab for this current spacing if there were
    /// parentheses/braces/brackets before it with the correct syntax.
    fn getHorstmannTabAt(self: *Self, token_index: TokenIndex, tab: u16) ?u16 {
        switch (self.tokenAt(token_index) catch return null) {
            .spacing => |spacing| {
                if (spacing.relative > 0 and spacing.absolute == tab + 4 and
                    (token_index == 0 or (self.tokenAt(token_index - 1) catch unreachable).isMirrorOpen()))
                {
                    return spacing.absolute;
                }
            },
            else => {},
        }
        return null;
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

    pub fn debugTokens(self: *Self) void {
        common.debugPrint("Tokens: [\n", .{});
        const start = common.back(self.farthest_token_index, 5) orelse 0;
        for (start..self.farthest_token_index + 1) |token_index| {
            common.debugPrint(" [{d}]:", .{token_index});
            common.debugPrint(" ", self.tokenAt(token_index) catch .file_end);
        }
        common.debugPrint("]\n", .{});
    }

    fn nodeInBounds(self: *Self, index: usize) *Node {
        return &self.nodes.items()[index];
    }

    const Open = Token.Open;
    const Close = Token.Close;
    const Self = @This();
};

const NodeResult = struct {
    node: NodeIndex,
    until_triggered: bool,

    fn triggered(node: NodeIndex) Self {
        return .{ .node = node, .until_triggered = true };
    }

    fn notTriggered(node: NodeIndex) Self {
        return .{ .node = node, .until_triggered = false };
    }

    fn withNode(self: Self, new_node: NodeIndex) Self {
        return .{ .node = new_node, .until_triggered = self.until_triggered };
    }

    const Self = @This();
};

const OperationResult = struct {
    operation: Operation,
    tab: u16,
    until_triggered: bool,

    fn triggered(operation: Operation, tab: u16) Self {
        return .{ .operation = operation, .tab = tab, .until_triggered = true };
    }

    fn notTriggered(operation: Operation, tab: u16) Self {
        return .{ .operation = operation, .tab = tab, .until_triggered = false };
    }

    fn toNode(self: Self, node: NodeIndex) NodeResult {
        return .{ .node = node, .until_triggered = self.until_triggered };
    }

    const Self = @This();
};

const Tabbed = struct {
    tab: u16,
    start_parsing_index: NodeIndex = 0,

    fn toTriggeredOperation(self: Self, operation: Operation) OperationResult {
        return OperationResult.triggered(operation, self.tab);
    }

    fn toNotTriggeredOperation(self: Self, operation: Operation) OperationResult {
        return OperationResult.notTriggered(operation, self.tab);
    }

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        try writer.print("Tabbed{{ .tab = {d}, .start_parsing_index = {d} }}", .{
            self.tab,
            self.start_parsing_index,
        });
    }

    const Self = @This();
};

const expected_spacing = "expected spacing between each identifier";
const expected_four_space_indents = "indents should be 4-spaces wide";

// General test structure:
// Organize by topic but within each topic put easier tests near the end.
// This makes easy tests fail last, and they are easier to debug
// if they are at the end of the input.

test "parser indent nesting" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 30, .next = 0 } }, // first (only) statement in root block
        Node{ .callable_token = 1 }, // greet_thee
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 0 } }, // ()
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } }, // greet_thee ()
        // [5]:
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 6 } }, // {...}
        Node{ .statement = .{ .node = 7, .next = 0 } }, // outer indent statement
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 8 } }, // outer indent
        Node{ .statement = .{ .node = 17, .next = 18 } }, // print statement
        Node{ .callable_token = 11 }, // print
        // [10]:
        Node{ .enclosed = .{ .open = .paren, .tab = 4, .start = 11 } }, // (...) inside print
        Node{ .statement = .{ .node = 12, .next = 0 } }, // statement inside print
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 13 } }, // indent inside print
        Node{ .statement = .{ .node = 14, .next = 15 } }, // first statement in print indent
        Node{ .atomic_token = 15 }, // 99731
        // [15]:
        Node{ .statement = .{ .node = 16, .next = 0 } }, // second statement in print indent
        Node{ .atomic_token = 17 }, // World
        Node{ .binary = .{ .operator = Operator.access, .left = 9, .right = 10 } }, // print (...)
        Node{ .statement = .{ .node = 29, .next = 0 } }, // wow54 statement
        Node{ .callable_token = 21 }, // wow54
        // [20]:
        Node{ .enclosed = .{ .open = .paren, .tab = 4, .start = 21 } }, // wow54 paren
        Node{ .statement = .{ .node = 22, .next = 0 } }, // inside wow54(...) statement
        Node{ .enclosed = .{ .open = .bracket, .tab = 4, .start = 23 } }, // [...]
        Node{ .statement = .{ .node = 24, .next = 0 } }, // inside wow54([...]) statement
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 25 } }, // indent inside ([...])
        // [25]:
        Node{ .statement = .{ .node = 26, .next = 27 } }, // first statement inside ([...])
        Node{ .atomic_token = 27 }, // 57973
        Node{ .statement = .{ .node = 28, .next = 0 } }, // second statement inside ([...])
        Node{ .atomic_token = 29 }, // 67974
        Node{ .binary = .{ .operator = Operator.access, .left = 19, .right = 20 } }, // wow (...)
        // [30]:
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 4, .right = 5 } }, // :
        .end,
    };
    {
        // One-true-brace
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
            common.debugPrint("# nodes:\n", parser.nodes);
        }
        const file_slice = [_][]const u8{
            "greet_thee(): {",
            "    print(",
            "        99731",
            "        World",
            "    )",
            "    wow54([",
            "        57973",
            "        67974",
            "    ])",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        // Horstmann
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
            common.debugPrint("# nodes:\n", parser.nodes);
        }
        const file_slice = [_][]const u8{
            "greet_thee():",
            "{   print",
            "    (   99731",
            "        World",
            "    )",
            "    wow54",
            "    ([  57973",
            "        67974",
            "    ])",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parser simple bracket indent" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 8, .next = 0 } },
        Node{ .atomic_token = 1 }, // Bart4
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 5, .next = 0 } },
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } },
        Node{ .statement = .{ .node = 7, .next = 0 } },
        Node{ .atomic_token = 5 }, // Bented4
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        .end,
    };
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart4",
            "[   Bented4",
            "]",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart4[",
            "    Bented4",
            "]",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parser simple paren indent" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 9, .next = 0 } },
        Node{ .atomic_token = 1 }, // Bart3
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 5, .next = 0 } },
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } },
        Node{ .statement = .{ .node = 7, .next = 0 } },
        Node{ .prefix = .{ .operator = Operator.plus, .node = 8 } },
        Node{ .atomic_token = 7 }, // Bented3
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        // [10]:
        .end,
    };
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart3",
            "(   +Bented3",
            ")",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart3(",
            "    +Bented3",
            ")",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

// TODO: implicit blocks
test "parser simple explicit brace block" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 10, .next = 0 } }, // root block first (only) statement
        Node{ .atomic_token = 1 }, // Bart5
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 4 } }, // {...}
        Node{ .statement = .{ .node = 5, .next = 0 } }, // internal statement
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } }, // indent
        Node{ .statement = .{ .node = 7, .next = 8 } }, // Bented51 statement
        Node{ .atomic_token = 7 }, // Bented51
        Node{ .statement = .{ .node = 9, .next = 0 } }, // Bented52 statement
        Node{ .atomic_token = 9 }, // Bented52
        // [10]:
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 3 } }, // :
        .end,
    };
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart5:",
            "{   Bented51",
            "    Bented52",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart5: {",
            "    Bented51",
            "    Bented52",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

// TODO: add `SomeCondition { some_bracket_logic() }`
//      braces need to be lower priority than `access`.
//      same for `open = .none` blocks.
//&|if MyCondition
//&|    do_something()

// TODO: test to error out on spacing indented to +8 or more at start of file
// TODO: test to see what happens when indented to +4 at start of file

//test "parsing quotes" {
//    {
//    var parser: Parser = .{};
//    defer parser.deinit();
//    errdefer {
//        common.debugPrint("# file:\n", parser.tokenizer.file);
//    }
//    const file_slice = [_][]const u8{
//        "'hi quote'",
//    };
//    try parser.tokenizer.file.appendSlice(&file_slice);
//
//    try parser.complete();
//
//    try parser.nodes.expectEqualsSlice(&[_]Node{
//        .end,
//    });
//    // No tampering done with the file, i.e., no errors.
//    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
//    }
//
//}

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
        "    -5.678",
        "    $$$Foe",
        "        Fum--",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 6, .next = 7 } }, // statement of 3.456 + indent
        Node{ .atomic_token = 1 }, // 3.456
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } }, // indent with hello_you
        Node{ .statement = .{ .node = 5, .next = 0 } }, // hello_you statement
        // [5]:
        Node{ .callable_token = 3 }, // hello_you
        Node{ .binary = .{ .operator = Operator.indent, .left = 2, .right = 3 } }, // 3.456 indent
        Node{ .statement = .{ .node = 22, .next = 0 } }, // statement of +1.234 + indent
        Node{ .prefix = .{ .operator = Operator.plus, .node = 9 } }, // +1.234
        Node{ .atomic_token = 7 }, // 1.234
        // [10]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 11 } }, // block with -5.678 starting
        Node{ .statement = .{ .node = 12, .next = 14 } }, // -5.678 statement
        Node{ .prefix = .{ .operator = Operator.minus, .node = 13 } }, // -5.678
        Node{ .atomic_token = 11 }, // 5.678
        Node{ .statement = .{ .node = 21, .next = 0 } }, // $$$Foe indent statement
        // [15]:
        Node{ .prefix = .{ .operator = Operator.lambda3, .node = 16 } }, // $$$Foe
        Node{ .atomic_token = 15 }, // Foe
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 18 } },
        Node{ .statement = .{ .node = 20, .next = 0 } },
        Node{ .atomic_token = 17 }, // Fum
        // [20]:
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 19 } },
        Node{ .binary = .{ .operator = Operator.indent, .left = 15, .right = 17 } },
        Node{ .binary = .{ .operator = Operator.indent, .left = 8, .right = 10 } },
        .end,
    });
    // No tampering done with the file, i.e., no errors.
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "parser implied blocks" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Start4",
            "    Indented4",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // Start4
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 0 } },
            // [5]:
            Node{ .atomic_token = 3 }, // Indented4
            Node{ .binary = .{ .operator = Operator.indent, .left = 2, .right = 3 } },
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Start3",
            "    +Indented3",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 7, .next = 0 } },
            Node{ .atomic_token = 1 }, // Start3
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 0 } },
            // [5]:
            Node{ .prefix = .{ .operator = Operator.plus, .node = 6 } },
            Node{ .atomic_token = 5 }, // Indented3
            Node{ .binary = .{ .operator = Operator.indent, .left = 2, .right = 3 } },
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Start5:",
            "    Indented51",
            "    Indented52",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 8, .next = 0 } },
            Node{ .atomic_token = 1 }, // Start5
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 6 } },
            // [5]:
            Node{ .atomic_token = 5 }, // Indented51
            Node{ .statement = .{ .node = 7, .next = 0 } },
            Node{ .atomic_token = 7 }, // Indented52
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 3 } },
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parser line continuations" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "Random_id3",
        "    +   Other_id4",
        "    -   Other_id5",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 6, .next = 0 } },
        Node{ .atomic_token = 1 }, // Random_id3
        Node{ .atomic_token = 5 }, // Other_id4
        Node{ .binary = .{ .operator = Operator.plus, .left = 2, .right = 3 } },
        // [5]:
        Node{ .atomic_token = 9 }, // Other_id5
        Node{ .binary = .{ .operator = Operator.minus, .left = 4, .right = 5 } },
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
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 4, .next = 0 } },
        Node{ .atomic_token = 1 }, // Wompus
        Node{ .atomic_token = 5 }, // 3.14
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 3 } },
        // [5]:
        .end,
    });
}

test "parser simple (and postfix) implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "Pi Sky",
        "Sci Fi++",
        "Kite Sty Five!",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 4, .next = 5 } },
        Node{ .atomic_token = 1 }, // Pi
        Node{ .atomic_token = 3 }, // Sky
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        // [5]:
        Node{ .statement = .{ .node = 9, .next = 10 } },
        Node{ .atomic_token = 5 }, // Sci
        Node{ .atomic_token = 7 }, // Fi
        Node{ .binary = .{ .operator = Operator.access, .left = 6, .right = 7 } },
        Node{ .postfix = .{ .operator = Operator.increment, .node = 8 } },
        // [10]:
        Node{ .statement = .{ .node = 16, .next = 0 } },
        Node{ .atomic_token = 11 }, // Kite
        Node{ .atomic_token = 13 }, // Sty
        Node{ .binary = .{ .operator = Operator.access, .left = 11, .right = 12 } },
        Node{ .atomic_token = 15 }, // Five
        // [15]:
        Node{ .binary = .{ .operator = Operator.access, .left = 13, .right = 14 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 15 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "parser complicated (and prefix) implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "--Why Shy Spy",
        "!Chai Lie Fry",
        "!Knife Fly Nigh!",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 2, .next = 8 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 7 } },
        Node{ .atomic_token = 3 }, // Why
        Node{ .atomic_token = 5 }, // Shy
        // [5]:
        Node{ .binary = .{ .operator = Operator.access, .left = 3, .right = 4 } },
        Node{ .atomic_token = 7 }, // Spy
        Node{ .binary = .{ .operator = Operator.access, .left = 5, .right = 6 } },
        Node{ .statement = .{ .node = 9, .next = 15 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 14 } },
        // [10]:
        Node{ .atomic_token = 11 }, // Chai
        Node{ .atomic_token = 13 }, // Lie
        Node{ .binary = .{ .operator = Operator.access, .left = 10, .right = 11 } },
        Node{ .atomic_token = 15 }, // Fry
        Node{ .binary = .{ .operator = Operator.access, .left = 12, .right = 13 } },
        // [15]:
        Node{ .statement = .{ .node = 16, .next = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 22 } },
        Node{ .atomic_token = 19 }, // Knife
        Node{ .atomic_token = 21 }, // Fly
        Node{ .binary = .{ .operator = Operator.access, .left = 17, .right = 18 } },
        // [20]:
        Node{ .atomic_token = 23 }, // Nigh
        Node{ .binary = .{ .operator = Operator.access, .left = 19, .right = 20 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 21 } },
        .end,
    });
    // No tampering done with the file, i.e., no errors.
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
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
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 5, .next = 6 } },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 3 } },
        Node{ .atomic_token = 3 }, // Theta
        Node{ .atomic_token = 7 }, // Beta
        // [5]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 4 } },
        Node{ .statement = .{ .node = 10, .next = 11 } },
        Node{ .atomic_token = 9 }, // Zeta
        Node{ .prefix = .{ .operator = Operator.increment, .node = 9 } },
        Node{ .atomic_token = 15 }, // Woga
        // [10]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 7, .right = 8 } },
        Node{ .statement = .{ .node = 15, .next = 16 } },
        Node{ .atomic_token = 17 }, // Yodus
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 12 } },
        Node{ .atomic_token = 23 }, // Spatula
        // [15]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 13, .right = 14 } },
        Node{ .statement = .{ .node = 19, .next = 0 } },
        Node{ .atomic_token = 25 }, // Wobdash
        Node{ .atomic_token = 29 }, // Flobsmash
        Node{ .binary = .{ .operator = Operator.multiply, .left = 17, .right = 20 } },
        // [20]:
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 18 } },
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
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 10, .next = 11 } },
        Node{ .atomic_token = 1 }, // Apple
        Node{ .prefix = .{ .operator = Operator.not, .node = 7 } },
        Node{ .atomic_token = 7 }, // Berry
        // [5]:
        Node{ .atomic_token = 9 }, // Cantaloupe
        Node{ .binary = .{ .operator = Operator.access, .left = 4, .right = 5 } },
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 6 } },
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 3 } },
        Node{ .atomic_token = 15 }, // 500
        // [10]:
        Node{ .binary = .{ .operator = Operator.plus, .left = 8, .right = 9 } },
        Node{ .statement = .{ .node = 18, .next = 0 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 15 } },
        Node{ .atomic_token = 19 }, // Xeno
        Node{ .atomic_token = 21 }, // Yak
        // [15]:
        Node{ .binary = .{ .operator = Operator.access, .left = 13, .right = 14 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 12 } },
        Node{ .atomic_token = 27 }, // 3000
        Node{ .binary = .{ .operator = Operator.minus, .left = 16, .right = 20 } },
        Node{ .atomic_token = 31 }, // Zelda
        // [20]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 17, .right = 19 } },
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
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 6, .next = 7 } },
        Node{ .atomic_token = 1 }, // Abc
        Node{ .atomic_token = 3 }, // Xyz
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        // [5]:
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 4 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 5 } },
        Node{ .statement = .{ .node = 8, .next = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 9 } },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 12 } },
        // [10]:
        Node{ .atomic_token = 13 }, // Def
        Node{ .atomic_token = 15 }, // Uvw
        Node{ .binary = .{ .operator = Operator.access, .left = 10, .right = 11 } },
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
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 5, .next = 8 } },
        Node{ .prefix = .{ .operator = Operator.lambda2, .node = 3 } },
        Node{ .atomic_token = 3 }, // Yammer
        Node{ .atomic_token = 7 }, // Zen
        // [5]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 7 } },
        Node{ .postfix = .{ .operator = Operator.increment, .node = 4 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 6 } },
        Node{ .statement = .{ .node = 16, .next = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 10 } },
        // [10]:
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 14 } },
        Node{ .prefix = .{ .operator = Operator.lambda1, .node = 12 } },
        Node{ .atomic_token = 19 }, // Oh
        Node{ .atomic_token = 21 }, // Great
        Node{ .binary = .{ .operator = Operator.access, .left = 11, .right = 13 } },
        // [15]:
        Node{ .atomic_token = 25 }, // Hessian
        Node{ .binary = .{ .operator = Operator.multiply, .left = 9, .right = 15 } },
        .end,
    });
}

test "order of operations with addition and multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "Alpha * Gamma + Epsilon",
        "Panko + K_panko * 1000",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);
    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 6, .next = 7 } },
        Node{ .atomic_token = 1 }, // Alpha
        Node{ .atomic_token = 5 }, // Gamma
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 3 } },
        // [5]:
        Node{ .atomic_token = 9 }, // Epsilon
        Node{ .binary = .{ .operator = Operator.plus, .left = 4, .right = 5 } },
        Node{ .statement = .{ .node = 10, .next = 0 } },
        Node{ .atomic_token = 11 }, // Panko
        Node{ .atomic_token = 15 }, // K_panko
        // [10]:
        Node{ .binary = .{ .operator = Operator.plus, .left = 8, .right = 12 } },
        Node{ .atomic_token = 19 }, // 1000
        Node{ .binary = .{ .operator = Operator.multiply, .left = 9, .right = 11 } },
        .end,
    });

    // No tampering done with the file, i.e., no errors.
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
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
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 22, .next = 0 } },
        Node{ .callable_token = 1 }, // container54
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 7, .next = 8 } },
        // [5]:
        Node{ .callable_token = 5 }, // of
        Node{ .callable_token = 9 }, // i1234
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 5, .right = 6 } },
        Node{ .statement = .{ .node = 11, .next = 16 } },
        Node{ .callable_token = 13 }, // at
        // [10]:
        Node{ .callable_token = 17 }, // str5
        Node{ .binary = .{ .operator = Operator.declare_temporary, .left = 9, .right = 15 } },
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 13 } },
        Node{ .statement = .{ .node = 14, .next = 0 } },
        Node{ .callable_token = 21 }, // qusp
        // [15]:
        Node{ .binary = .{ .operator = Operator.access, .left = 10, .right = 12 } },
        Node{ .statement = .{ .node = 21, .next = 0 } },
        Node{ .callable_token = 27 }, // array
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 19 } },
        Node{ .statement = .{ .node = 20, .next = 0 } },
        // [20]:
        Node{ .callable_token = 31 }, // dongle
        Node{ .binary = .{ .operator = Operator.access, .left = 17, .right = 18 } },
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
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
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 14, .next = 0 } },
        Node{ .callable_token = 1 }, // superb
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 7, .next = 8 } },
        // [5]:
        Node{ .atomic_token = 5 }, // Brepus
        Node{ .atomic_token = 9 }, // 161
        Node{ .binary = .{ .operator = Operator.declare_temporary, .left = 5, .right = 6 } },
        Node{ .statement = .{ .node = 11, .next = 12 } },
        Node{ .atomic_token = 13 }, // Canyon
        // [10]:
        Node{ .atomic_token = 17 }, // Noynac
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 9, .right = 10 } },
        Node{ .statement = .{ .node = 13, .next = 0 } },
        Node{ .atomic_token = 21 }, // Candid
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        // [15]:
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
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 31, .next = 0 } },
        Node{ .callable_token = 1 }, // fungus
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 7, .next = 12 } },
        // [5]:
        Node{ .callable_token = 5 }, // type1
        Node{ .callable_token = 9 }, // t_array
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 5, .right = 11 } },
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 9 } },
        Node{ .statement = .{ .node = 10, .next = 0 } },
        // [10]:
        Node{ .callable_token = 13 }, // str7
        Node{ .binary = .{ .operator = Operator.access, .left = 6, .right = 8 } },
        Node{ .statement = .{ .node = 13, .next = 14 } },
        Node{ .callable_token = 19 }, // type2
        Node{ .statement = .{ .node = 17, .next = 0 } },
        // [15]:
        Node{ .callable_token = 23 }, // type3
        Node{ .callable_token = 27 }, // i64
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 15, .right = 16 } },
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 20 } },
        // [20]:
        Node{ .statement = .{ .node = 23, .next = 24 } },
        Node{ .atomic_token = 33 }, // Life
        Node{ .atomic_token = 37 }, // 17
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 21, .right = 22 } },
        Node{ .statement = .{ .node = 26, .next = 27 } },
        // [25]:
        Node{ .atomic_token = 41 }, // Cardio
        Node{ .postfix = .{ .operator = Operator.not, .node = 25 } },
        Node{ .statement = .{ .node = 30, .next = 0 } },
        Node{ .atomic_token = 47 }, // Fritz
        Node{ .callable_token = 51 }, // foo_fritz
        // [30]:
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 28, .right = 29 } },
        Node{ .binary = .{ .operator = Operator.access, .left = 18, .right = 19 } },
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 3, .next = 4 } },
            Node{ .atomic_token = 1 }, // Esper
            Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 2 } },
            Node{ .statement = .{ .node = 6, .next = 7 } },
            // [5]:
            Node{ .atomic_token = 5 }, // Jesper
            Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 5 } },
            Node{ .statement = .{ .node = 9, .next = 0 } },
            Node{ .atomic_token = 9 }, // Esperk
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 8 } },
            // [10]:
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 2, .next = 6 } },
            Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 5, .next = 0 } },
            Node{ .atomic_token = 3 }, // Jarok
            // [5]:
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 4 } },
            Node{ .statement = .{ .node = 7, .next = 11 } },
            Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 8 } },
            Node{ .statement = .{ .node = 10, .next = 0 } },
            Node{ .atomic_token = 11 }, // Turmeric
            // [10]:
            Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 9 } },
            Node{ .statement = .{ .node = 12, .next = 0 } },
            Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 13 } },
            Node{ .statement = .{ .node = 15, .next = 0 } },
            Node{ .atomic_token = 19 }, // Quinine
            // [15]:
            Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 14 } },
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
        try parser.tokenizer.file.lines.append(try SmallString.init("funE1(F2.,G3;,H4:):"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 14, .next = 0 } },
            Node{ .callable_token = 1 }, // funE1
            Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
            Node{ .statement = .{ .node = 6, .next = 7 } },
            // [5]:
            Node{ .atomic_token = 5 }, // F2
            Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 5 } },
            Node{ .statement = .{ .node = 9, .next = 10 } },
            Node{ .atomic_token = 11 }, // G3
            Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 8 } },
            // [10]:
            Node{ .statement = .{ .node = 12, .next = 0 } },
            Node{ .atomic_token = 17 }, // H4
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 11 } },
            Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 13 } },
            // [15]:
            .end,
        });
        // No errors in attempts to parse a RHS expression for the infix operators.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "funE1(F2.,G3;,H4:):",
        });
    }
}

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
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 7, .next = 8 } },
        Node{ .atomic_token = 1 }, // Set
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 5, .next = 0 } },
        // [5]:
        Node{ .atomic_token = 5 }, // 543
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 6 } },
        Node{ .statement = .{ .node = 22, .next = 23 } },
        Node{ .atomic_token = 11 }, // Array
        // [10]:
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 11 } },
        Node{ .statement = .{ .node = 12, .next = 0 } },
        Node{ .callable_token = 15 }, // element_type
        Node{ .binary = .{ .operator = Operator.access, .left = 9, .right = 10 } },
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 15 } },
        // [15]:
        Node{ .statement = .{ .node = 16, .next = 17 } },
        Node{ .atomic_token = 21 }, // 1
        Node{ .statement = .{ .node = 18, .next = 19 } },
        Node{ .atomic_token = 25 }, // 2
        Node{ .statement = .{ .node = 20, .next = 0 } },
        // [20]:
        Node{ .atomic_token = 29 }, // 3
        Node{ .binary = .{ .operator = Operator.access, .left = 13, .right = 14 } },
        Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 21 } },
        Node{ .statement = .{ .node = 33, .next = 0 } },
        Node{ .atomic_token = 35 }, // Lot
        // [25]:
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 26 } },
        Node{ .statement = .{ .node = 27, .next = 28 } },
        Node{ .callable_token = 39 }, // inner_type
        Node{ .statement = .{ .node = 31, .next = 0 } },
        Node{ .callable_token = 43 }, // at
        // [30]:
        Node{ .callable_token = 47 }, // index4
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 29, .right = 30 } },
        Node{ .binary = .{ .operator = Operator.access, .left = 24, .right = 25 } },
        Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 32 } },
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 2, .next = 0 } },
            Node{ .prefix = .{ .operator = Operator.plus, .node = 3 } },
            Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 6 } },
            // [5]:
            Node{ .atomic_token = 5 }, // Wow
            Node{ .statement = .{ .node = 7, .next = 0 } },
            Node{ .atomic_token = 9 }, // Great
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 9, .next = 0 } },
            Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 4, .next = 5 } },
            Node{ .callable_token = 3 }, // wow
            // [5]:
            Node{ .statement = .{ .node = 6, .next = 7 } },
            Node{ .callable_token = 7 }, // jam
            Node{ .statement = .{ .node = 8, .next = 0 } },
            Node{ .callable_token = 11 }, // time
            Node{ .postfix = .{ .operator = Operator.not, .node = 2 } },
            // [10]:
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 14, .next = 0 } },
            Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 6, .next = 7 } },
            Node{ .atomic_token = 3 }, // Boo
            // [5]:
            Node{ .atomic_token = 7 }, //33
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 4, .right = 5 } },
            Node{ .statement = .{ .node = 10, .next = 0 } },
            Node{ .callable_token = 11 }, // hoo
            Node{ .atomic_token = 15 }, // 123
            // [10]:
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 8, .right = 12 } },
            Node{ .atomic_token = 19 }, // 44
            Node{ .binary = .{ .operator = Operator.plus, .left = 9, .right = 11 } },
            Node{ .atomic_token = 25 }, // 57
            Node{ .binary = .{ .operator = Operator.minus, .left = 2, .right = 13 } },
            // [15]:
            .end,
        });
        // Attempts to parse generics or arguments for `callable` don't add errors:
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "{Boo: 33, hoo: 123 + 44}-57",
        });
    }
}

// TODO
//test "mixed commas and newlines" {
//    const expected_nodes = [_]Node{
//        // [0]:
//        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
//        Node{ .statement = .{ .node = 20, .next = 0 } },
//        Node{ .callable_token = 1 }, // goober
//        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 0 } }, // ()
//        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } }, // goober()
//        // [5]:
//        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 6 } }, // { ... }
//        Node{ .statement = .{ .node = 7, .next = 0 } }, // interior of {...}
//        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 8 } }, // interior is indented
//        Node{ .statement = .{ .node = 9, .next = 10 } }, // first statement
//        Node{ .atomic_token = 11 }, // 405
//        // [10]:
//        Node{ .statement = .{ .node = 11, .next = 12 } }, // second statement
//        Node{ .atomic_token = 15 }, // 406
//        Node{ .statement = .{ .node = 13, .next = 14 } },
//        Node{ .atomic_token = 17 }, // 407
//        Node{ .statement = .{ .node = 15, .next = 16 } },
//        // [15]:
//        Node{ .atomic_token = 19 }, // 408
//        Node{ .statement = .{ .node = 17, .next = 18 } },
//        Node{ .atomic_token = 23 }, // 409
//        Node{ .statement = .{ .node = 19, .next = 0 } },
//        Node{ .atomic_token = 27 }, // 510
//        // [20]:
//        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 4, .right = 5 } },
//        .end,
//    };
//    {
//        var parser: Parser = .{};
//        defer parser.deinit();
//        errdefer {
//            common.debugPrint("# file:\n", parser.tokenizer.file);
//        }
//        const file_slice = [_][]const u8{
//            "goober(): {",
//            "    405, 406",
//            "    407",
//            "    408, 409,",
//            "    510,",
//            "}",
//        };
//        try parser.tokenizer.file.appendSlice(&file_slice);
//
//        try parser.complete();
//
//        try parser.nodes.expectEqualsSlice(&expected_nodes);
//        // No tampering done with the file, i.e., no errors.
//        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
//    }
//    {
//        var parser: Parser = .{};
//        defer parser.deinit();
//        errdefer {
//            common.debugPrint("# file:\n", parser.tokenizer.file);
//        }
//        const file_slice = [_][]const u8{
//            "goober():",
//            "{   405, 406",
//            "    407",
//            "    408, 409,",
//            "    510,",
//            "}",
//        };
//        try parser.tokenizer.file.appendSlice(&file_slice);
//
//        try parser.complete();
//
//        try parser.nodes.expectEqualsSlice(&expected_nodes);
//        // No tampering done with the file, i.e., no errors.
//        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
//    }
//}

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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 2, .next = 0 } },
            Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 4, .next = 5 } },
            Node{ .atomic_token = 3 }, // C0
            // [5]:
            Node{ .statement = .{ .node = 6, .next = 7 } },
            Node{ .atomic_token = 7 }, // C1
            Node{ .statement = .{ .node = 8, .next = 0 } },
            Node{ .atomic_token = 11 }, // C2
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 8, .next = 0 } },
            Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 4, .next = 5 } },
            Node{ .atomic_token = 3 }, // Bk0
            // [5]:
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 7 }, // Bk1
            Node{ .atomic_token = 15 }, // 761
            Node{ .binary = .{ .operator = Operator.minus, .left = 2, .right = 7 } },
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 3 }, // Bc0
            // [5]:
            Node{ .atomic_token = 11 }, // Abcdef
            Node{ .binary = .{ .operator = Operator.plus, .left = 2, .right = 5 } },
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // Whatever
            Node{ .callable_token = 5 }, // type1
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 3 } },
            // [5]:
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // Writable_whatever
            Node{ .callable_token = 5 }, // type2
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 2, .right = 3 } },
            // [5]:
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // Declassign
            Node{ .callable_token = 5 }, // type_assign1
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 3 } },
            // [5]:
            Node{ .atomic_token = 9 }, // 12345
            Node{ .binary = .{ .operator = Operator.assign, .left = 4, .right = 5 } },
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // Oh_writable
            Node{ .callable_token = 5 }, // type_assign2
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 2, .right = 3 } },
            // [5]:
            Node{ .atomic_token = 9 },
            Node{ .binary = .{ .operator = Operator.assign, .left = 4, .right = 5 } },
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // D1
            Node{ .atomic_token = 5 }, // D2
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 6 } },
            // [5]:
            Node{ .atomic_token = 9 }, // D3
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 3, .right = 5 } },
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // X3
            Node{ .atomic_token = 5 }, // Y4
            Node{ .binary = .{ .operator = Operator.assign, .left = 2, .right = 6 } },
            // [5]:
            Node{ .atomic_token = 9 }, // 750
            Node{ .binary = .{ .operator = Operator.assign, .left = 3, .right = 5 } },
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
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // VarQ
            Node{ .callable_token = 5 }, // i32
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 2, .right = 3 } },
            // [5]:
            Node{ .atomic_token = 9 }, // Qu16
            Node{ .binary = .{ .operator = Operator.assign, .left = 4, .right = 8 } },
            Node{ .atomic_token = 13 }, // VarU
            Node{ .binary = .{ .operator = Operator.assign, .left = 5, .right = 12 } },
            Node{ .callable_token = 17 }, // i16
            // [10]:
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 7, .right = 9 } },
            Node{ .atomic_token = 21 }, // 750
            Node{ .binary = .{ .operator = Operator.assign, .left = 10, .right = 11 } },
            .end,
        });
    }
}

// TODO: error tests, e.g., "cannot postfix this"
