const common = @import("common.zig");
const Operator = @import("operator.zig").Operator;
const Operation = Operator.Operation;
const OrElse = common.OrElse;
const OwnedList = @import("owned_list.zig").OwnedList;
const SmallString = @import("string.zig").Small;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Token = @import("token.zig").Token;
const Node = @import("node.zig").Node;
const TokenIndex = Node.TokenIndex;
const NodeIndex = Node.Index;
const Until = @import("until.zig").Until;

const std = @import("std");

const OwnedNodes = OwnedList(Node);
const OwnedNodeIndices = OwnedList(NodeIndex);

const ParserError = error{
    out_of_memory,
    out_of_statements,
    broken_invariant,
    // TODO: should all syntax errors panic?  we probably can
    // switch the syntax errors that shouldn't panic to something else (e.g., out_of_statements)
    syntax,
    syntax_panic, // break immediately everywhere
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

        // So that `node_index == 0` appears to be invalid, the
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
        while (try self.getSameBlockNextNodeIndex(tab, false)) |start_parsing_index| {
            self.farthest_token_index = start_parsing_index;
            const statement_result = self.appendNextStatement(tab, Until.closing(open), .only_try) catch |the_error| {
                if (the_error == ParserError.syntax_panic) {
                    return the_error;
                }
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
            if (or_else.isOnlyTry() and self.nodes.count() == statement_index + 1) {
                _ = self.nodes.remove(statement_index);
            }
        }

        const result = try self.appendNextExpression(tab, until, or_else);

        self.nodes.set(statement_index, Node{ .statement = .{
            .node = result.node,
        } }) catch unreachable;

        return result.withNode(statement_index);
    }

    /// Appends the next expression and requires that the `until` is triggered.
    fn appendTriggeredExpression(self: *Self, tab: u16, until: Until) ParserError!NodeIndex {
        const result = try self.appendNextExpression(tab, until, .only_try);
        common.debugPrint("tried to get an expression, was it triggered {s}?\n", .{common.boolSlice(result.until_triggered)});
        self.debugTokens();
        if (result.until_triggered) {
            return result.node;
        }
        return ParserError.syntax_panic;
    }

    /// Supports starting with spacing *or not* (e.g., for the start of a statement
    /// where we don't want to check the indent yet).
    fn appendNextExpression(self: *Self, tab: u16, until: Until, or_else: OrElse) ParserError!NodeResult {
        errdefer {
            if (or_else.be_noisy()) |error_message| {
                self.addTokenizerError(error_message);
            }
        }
        common.debugPrint("seeking next expression ", until);
        self.debugTokens();
        // The current left node which can interact with the next operation,
        // is the last element of `hierarchy`, with nesting all the way up
        // to the "root node" (`hierarchy.inBounds(0)`) which should be returned.
        var hierarchy = OwnedNodeIndices.init();
        defer hierarchy.deinit();

        const standalone = try self.appendNextStandaloneExpression(&hierarchy, tab, until, or_else);
        if (standalone.until_triggered) {
            common.debugPrint("got standalone triggered {d}\n", .{standalone.node});
            self.debugTokens();
            return NodeResult.triggered(hierarchy.inBounds(0));
        }

        while (true) {
            const result = try self.seekNextOperation(tab, until);
            const operation = result.operation;
            switch (operation.operator) {
                .op_none => return result.toNode(hierarchy.inBounds(0)),
                .op_comma => {
                    // Commas are so low in priority that we split off statements
                    // so that we can go roughly in left-to-right order without
                    // depth-first-searching for the first left node.
                    return result.toNode(hierarchy.inBounds(0));
                },
                else => {},
            }
            if (operation.type == .postfix) {
                try self.appendPostfixOperation(&hierarchy, operation);
            } else if (self.appendNextStandaloneExpression(&hierarchy, tab, until, .only_try)) |right| {
                try self.appendInfixOperation(&hierarchy, operation, right.node);
                if (right.until_triggered) {
                    return NodeResult.triggered(hierarchy.inBounds(0));
                }
            } else |error_getting_right_hand_expression| {
                if (error_getting_right_hand_expression == ParserError.syntax_panic) {
                    return error_getting_right_hand_expression;
                }
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

    /// Adds an atom with possible prefix (but NOT postfix) operators.
    /// Includes things like `1.234`, `My_variable`, `+4.56`, `-7.89`,
    /// `++Index` or `!Countdown` as well.  For member access like
    /// `First_identifier Second_identifier`, just grab the first one.
    /// NOTE: do NOT add the returned index into `hierarchy`, we'll do that for you.
    /// Supports starting with spacing *or not* (e.g., for the start of a statement).
    fn appendNextStandaloneExpression(self: *Self, hierarchy: *OwnedNodeIndices, tab: u16, until: Until, or_else: OrElse) ParserError!NodeResult {
        common.debugPrint("--seeking next standalone expression ", until);
        self.debugTokens();
        self.debugHierarchy(hierarchy);
        const next_tabbed = self.getSameStatementNextTabbed(tab) orelse {
            const had_expression_token = false;
            self.assertSyntax(had_expression_token, or_else.map(expected_spacing)) catch {};
            // TODO: maybe return .out_of_statements
            return ParserError.syntax;
        };
        self.farthest_token_index = next_tabbed.start_parsing_index;
        if (next_tabbed.tab > tab) {
            const enclosed_index = try self.appendNextEnclosed(next_tabbed.tab, .none);
            hierarchy.append(enclosed_index) catch return ParserError.out_of_memory;
            // TODO: this until_triggered doesn't seem to do anything, maybe it's not necessary.
            return .{ .node = enclosed_index, .until_triggered = until.shouldBreakAtDeindent() };
        }

        const next_index = switch (try self.peekToken()) {
            .starts_upper, .number => blk: {
                const atomic_index = try self.justAppendNode(Node{
                    .atomic_token = self.farthest_token_index,
                });
                self.farthest_token_index += 1;

                hierarchy.append(atomic_index) catch return ParserError.out_of_memory;
                break :blk atomic_index;
            },
            .starts_lower => blk: {
                const callable_index = try self.justAppendNode(Node{
                    .callable_token = self.farthest_token_index,
                });
                self.farthest_token_index += 1;

                hierarchy.append(callable_index) catch return ParserError.out_of_memory;
                break :blk callable_index;
            },
            .open => |open| blk: {
                self.farthest_token_index += 1;
                const enclosed_index = try self.appendNextEnclosed(tab, open);
                hierarchy.append(enclosed_index) catch return ParserError.out_of_memory;
                if (open == .brace and until.shouldBreakAtDeindent()) {
                    return NodeResult.triggered(enclosed_index);
                }
                break :blk enclosed_index;
            },
            .operator => |operator| blk: {
                if (!operator.isPrefixable()) {
                    if (or_else.be_noisy()) |_| {
                        self.addTokenizerError("not a prefix operator");
                    }
                    return ParserError.syntax;
                }
                common.debugPrint("\ngot prefix operator ", operator);
                self.debugTokens();
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
                const inner_result = try self.appendNextExpression(tab, Until.operatorWeakerThan(operator), or_else);
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
                common.debugPrint("\nafter getting prefix operator ", operator);
                self.debugTokens();
                self.debugHierarchy(hierarchy);
                break :blk prefix_index;
            },
            .keyword => |keyword| blk: {
                const node_index = switch (keyword) {
                    .kw_if => try self.appendConditionAndBlocks(Node.Conditional, tab, expected_if_condition_and_block),
                    .kw_while => try self.appendConditionAndBlocks(Node.WhileLoop, tab, expected_while_condition_and_block),
                    // TODO: verify that each statement inside the `what` block is correct (e.g.,
                    // an identifier or number followed by another statement -- e.g., A, B -- or an indent -- A {...})
                    // We should probably create a new node for this, like `what_case`.
                    .kw_what => try self.appendConditionAndFirstBlock(Node.What, tab, expected_what_condition_and_block),
                    .kw_else, .kw_elif => {
                        self.addTokenizerError("need `if` before `else` or `elif`");
                        return ParserError.syntax_panic;
                    },
                    else => return ParserError.unimplemented,
                };
                hierarchy.append(node_index) catch return ParserError.out_of_memory;
                break :blk node_index;
            },
            else => {
                if (or_else.be_noisy()) |_| {
                    self.addTokenizerError("expected an expression");
                }
                return ParserError.syntax;
            },
        };
        return NodeResult.notTriggered(next_index);
    }

    // Returns the next postfix or infix operation.
    // Prefix operations are taken care of inside of `appendNextStandaloneExpression`.
    fn seekNextOperation(self: *Self, tab: u16, until: Until) ParserError!OperationResult {
        const restore_index = self.farthest_token_index;
        common.debugPrint("--seeking next operation ", until);
        self.debugTokens();

        var operation_tabbed = self.getSameStatementNextTabbed(tab) orelse {
            common.debugPrint("wanted next operation but got deindent\n", .{});
            self.debugTokens();
            return OperationResult{
                .operation = .{ .operator = .op_none },
                .tab = tab,
                // The only reason we'd get here is if we de-indented.
                // TODO: we could trigger this at file_end; see if this breaks anything...
                .until_triggered = until.shouldBreakAtDeindent(),
            };
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
                    self.farthest_token_index -= 1;
                    // Pretend that we have an operator before this prefix.
                    break :blk .{ .operator = .op_access, .type = .infix };
                }
            },
            .close => |close| blk: {
                if (until.shouldBreakAtClose(close)) {
                    self.farthest_token_index += 1;
                    return operation_tabbed.toTriggeredOperation(.{ .operator = .op_none });
                }
                // TODO: does this ever actually happen?  i expect
                // that we should always trigger when we find a close.
                // Same as the `else` block below:
                self.farthest_token_index -= 1;
                break :blk .{ .operator = .op_access, .type = .infix };
            },
            .keyword => blk: {
                self.farthest_token_index -= 1;
                break :blk .{ .operator = .op_none };
            },
            .open => |open| blk: {
                self.farthest_token_index -= 1;
                break :blk .{ .operator = if (open == .brace) .op_indent else .op_access, .type = .infix };
            },
            else => blk: {
                // We encountered another realizable token, back up so that
                // we maintain the invariant that there's a space before the next real element.
                self.farthest_token_index -= 1;
                break :blk .{ .operator = .op_access, .type = .infix };
            },
        };

        if (operation_tabbed.tab == tab + 4) {
            // This is an indented operation, and we didn't see another indent past it.
            //&|    Some_expression
            //&|        +enclosed
            // this is probably a syntax error, but we'll parse it as `Some_expression` `access` `+enclosed`
            self.farthest_token_index = restore_index;
            operation = .{ .operator = .op_indent, .type = .infix };
        }

        self.debugTokens();
        if (until.shouldBreakBeforeOperation(operation)) {
            self.farthest_token_index = restore_index;
            common.debugPrint("breaking at operation ", operation);
            return operation_tabbed.toTriggeredOperation(.{ .operator = .op_none });
        }
        common.debugPrint("continuing with operation ", operation);
        return operation_tabbed.toNotTriggeredOperation(operation);
    }

    fn appendPostfixOperation(self: *Self, hierarchy: *OwnedNodeIndices, operation: Operation) ParserError!void {
        common.debugPrint("--appending postfix ", operation);
        self.debugTokens();
        self.debugHierarchy(hierarchy);
        const operation_precedence = operation.precedence(Operation.Compare.on_right);
        var hierarchy_index = hierarchy.count();
        var left_index: NodeIndex = 0;
        while (hierarchy_index > 0) {
            hierarchy_index -= 1;
            left_index = hierarchy.inBounds(hierarchy_index);
            const left_operation = self.nodeInBounds(left_index).operation();
            // higher precedence means higher priority.
            if (left_operation.isPostfix() or left_operation.precedence(Operation.Compare.on_left) >= operation_precedence) {
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
        common.debugPrint("--appending infix ", operation);
        self.debugTokens();
        self.debugHierarchy(hierarchy);

        const operation_precedence = operation.precedence(Operation.Compare.on_right);
        var hierarchy_index = hierarchy.count();
        var left_index: NodeIndex = 0;
        while (hierarchy_index > 0) {
            hierarchy_index -= 1;
            left_index = hierarchy.inBounds(hierarchy_index);
            const left_operation = self.nodeInBounds(left_index).operation();
            common.debugPrint(">>>checking hierarchy {d}, ", .{hierarchy_index});
            common.debugPrint("when appending infix ", operation);
            common.debugPrint(">>>checking left operation ", left_operation);
            const left_precedence = left_operation.precedence(Operation.Compare.on_left);
            // higher precedence means higher priority.
            if (left_operation.isPrefix() and left_precedence < operation_precedence) {
                // This can happen in situations like `5 + return 3 * 4`;
                // `return` is a very low priority operator.
                common.debugPrint(">>>left operator was prefix somehow ", operation);
                const next_index = try self.justAppendNode(.{ .binary = .{
                    .operator = operation.operator,
                    .left = hierarchy.inBounds(0),
                    .right = left_index,
                } });
                common.debugPrint("before putting new operation at {d}, hierarchy is ", .{next_index});
                self.debugHierarchy(hierarchy);
                hierarchy.clear();
                hierarchy.append(next_index) catch unreachable;
                common.debugPrint("after putting new operation at {d}, hierarchy is ", .{next_index});
                self.debugHierarchy(hierarchy);
                return;
            } else if (left_operation.isPostfix() or left_precedence >= operation_precedence) {
                common.debugPrint(">>>left precedence {d} was more important than new right precedence {d}, ", .{ left_precedence, operation_precedence });
                common.debugPrint("when appending infix ", operation);
                // `left` has higher priority; we should continue up the hierarchy
                // until we find the spot that this new operation should take.
                _ = hierarchy.pop();
            } else {
                // `operation` has higher priority, so we need to invert the nodes a bit.
                // break an invariant here:
                common.debugPrint(">>>left precedence {d} was less than new right precedence {d}, ", .{ left_precedence, operation_precedence });
                common.debugPrint("when appending infix ", operation);
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
                common.debugPrint("after putting new operation at {d}, hierarchy is ", .{next_index});
                self.debugHierarchy(hierarchy);
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

    fn appendConditionAndBlocks(self: *Self, comptime T: anytype, tab: u16, expected: []const u8) ParserError!NodeIndex {
        const conditional_index = try self.appendConditionAndFirstBlock(T, tab, expected);
        try self.maybeAppendConditionalContinuation(conditional_index, tab);
        return conditional_index;
    }

    fn appendConditionAndFirstBlock(self: *Self, comptime T: anytype, tab: u16, expected: []const u8) ParserError!NodeIndex {
        const keyword_index = self.farthest_token_index;
        self.farthest_token_index += 1;
        const conditional_result = self.appendNextExpression(tab, Until.nextBlockEnds(), .only_try) catch {
            self.tokenizer.addErrorAt(keyword_index, expected);
            return ParserError.syntax_panic;
        };
        const conditional_index = conditional_result.node;
        const binary = self.nodes.inBounds(conditional_index).getBinary() orelse {
            self.tokenizer.addErrorAt(keyword_index, expected);
            return ParserError.syntax_panic;
        };
        if (binary.operator != .op_indent) {
            self.tokenizer.addErrorAt(keyword_index, expected);
            return ParserError.syntax_panic;
        }
        self.nodes.items()[conditional_index] = T.withEvaluateAndBlock(binary.left, binary.right);
        return conditional_index;
    }

    fn maybeAppendConditionalContinuation(self: *Self, conditional_index: NodeIndex, tab: u16) ParserError!void {
        const start_parsing_index = try self.getSameBlockNextNodeIndex(tab, true) orelse {
            // Nothing further at this indent.
            return;
        };
        const keyword = (try self.tokenAt(start_parsing_index)).getKeyword() orelse {
            // Nothing further.
            return;
        };
        const keyword_index = start_parsing_index;
        const else_index: NodeIndex = switch (keyword) {
            .kw_elif => blk: {
                self.farthest_token_index = keyword_index;
                break :blk try self.appendConditionAndBlocks(Node.Conditional, tab, expected_elif_condition_and_block);
            },
            .kw_else => blk: {
                self.farthest_token_index = keyword_index + 1;
                common.debugPrint("looking at else block\n", .{});
                self.debugTokens();
                const result = self.appendNextExpression(tab, Until.nextBlockEnds(), .only_try) catch {
                    self.tokenizer.addErrorAt(keyword_index, expected_else_block);
                    return ParserError.syntax_panic;
                };
                if (!self.isBlockAtNode(result.node, tab)) {
                    self.tokenizer.addErrorAt(keyword_index, expected_else_block);
                    return ParserError.syntax_panic;
                }
                break :blk result.node;
            },
            else => return,
        };
        self.nodes.items()[conditional_index].setSecondBlock(else_index) catch {
            return ParserError.broken_invariant;
        };
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
    fn getSameBlockNextNodeIndex(self: *Self, tab: u16, require_same_indent: bool) ParserError!?NodeIndex {
        // TODO: ignore comments as well
        switch (self.peekToken() catch return null) {
            .file_end => return null,
            //&|MyValue:
            //&|        +SomeValue  # keep prefix operators at tab indent
            //&|    -   CoolStuff   # infix operator should be ignored
            .spacing => |spacing| if (spacing.getNewlineTab()) |newline_tab| {
                if (newline_tab == tab) {
                    return self.farthest_token_index + 1;
                }
                if (newline_tab % 4 != 0) {
                    self.addTokenizerError(expected_four_space_indents);
                    return ParserError.syntax;
                }
                if (require_same_indent or newline_tab < tab) {
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
            return .{ .start_parsing_index = token_index + 1, .tab = tab };
        } else {
            return null;
        }
    }

    fn isHorstmannTabbedGoingForward(self: *Self, starting_token_index: usize, tab: u16) bool {
        var token_index = starting_token_index;
        // TODO: we probably can relax the 3 parentheses limit but we need to bump the tab.
        //&|my_function():
        //&|    return
        //&|    [[[[    my_indent1
        //&|            my_indent2
        //&|    ]]]]
        for (0..3) |closes| {
            _ = closes;
            if (!(self.tokenAt(token_index) catch return false).isMirrorOpen()) {
                return false;
            }
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

    pub fn debug(self: *Self) void {
        common.debugPrint("# file:\n", self.tokenizer.file);
        common.debugPrint("# nodes:\n", self.nodes);
        self.debugTokens();
    }

    pub fn debugTokens(self: *Self) void {
        self.debugTokensIn(common.back(self.farthest_token_index, 3) orelse 0, self.farthest_token_index + 3);
    }

    pub fn debugHierarchy(self: *Self, hierarchy: *const OwnedNodeIndices) void {
        common.debugPrint("Hierarchy: [\n", .{});
        for (0..hierarchy.count()) |i| {
            const h = hierarchy.inBounds(i);
            common.debugPrint(" [{d}]: {d}", .{ i, h });
            common.debugPrint(" -> ", self.nodes.inBounds(h));
        }
        common.debugPrint("]\n", .{});
    }

    pub fn debugTokensIn(self: *Self, start_token_index: TokenIndex, end_token_index: TokenIndex) void {
        common.debugPrint("Tokens: [\n", .{});
        for (start_token_index..end_token_index + 1) |token_index| {
            if (token_index == self.farthest_token_index) {
                common.debugPrint("*[{d}]:", .{token_index});
            } else {
                common.debugPrint(" [{d}]:", .{token_index});
            }
            var should_break = false;
            common.debugPrint(" ", self.tokenAt(token_index) catch blk: {
                should_break = true;
                break :blk .file_end;
            });
            if (should_break) break;
        }
        common.debugPrint("]\n", .{});
    }

    fn nodeInBounds(self: *Self, index: NodeIndex) *Node {
        return &self.nodes.items()[index];
    }

    /// Returns true if `node` is an indented block.
    fn isBlockAtNode(self: *Self, node: NodeIndex, tab: u16) bool {
        // TODO: we'll eventually need to check for things like `if X $[...] else $(...)`,
        //      since $[] and $() are effectively indented.
        // TODO: do we want a dedicated operator like `if X |> [...] else |> (...)` for this?
        return switch (self.nodes.inBounds(node)) {
            .enclosed => |enclosed| switch (enclosed.open) {
                .none => enclosed.tab == tab + 4,
                .brace => true,
                else => false,
            },
            else => false,
        };
    }

    const Open = Token.Open;
    const Close = Token.Close;
    pub const Error = ParserError;
    const Self = @This();
};

const NodeResult = struct {
    node: NodeIndex,
    // TODO: rename to `triggered_until`
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

// TODO: errors should link to an hm-lang README page or issue tracker
const expected_spacing = "expected spacing between each identifier";
const expected_four_space_indents = "indents should be 4-spaces wide";
const expected_what_condition_and_block = "need expression for `what` or indented block after";
const expected_if_condition_and_block = "need condition for `if` or indented block after";
const expected_while_condition_and_block = "need condition for `while` or indented block after";
const expected_elif_condition_and_block = "need condition for `elif` or indented block after";
const expected_else_block = "need indented block after `else`";
