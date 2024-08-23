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
        hierarchy.append(try self.appendNextStandaloneExpression(tab)) catch return ParserError.out_of_memory;
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
                const right_index = try self.appendNextStandaloneExpression(tab);
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
        self.tokenizer.addErrorAt(self.farthest_token_index, "expected end of line");
        return ParserError.unimplemented;
    }

    /// Adds an atom with possible prefix (but NOT postfix) operators.
    /// Includes things like `1.234`, `My_variable`, `+4.56`, `-7.89`,
    /// `++Index` or `!Countdown` as well.  For member access like
    /// `First_identifier Second_identifier`, just grab the first one.
    // TODO: also include operations like `my_function(...)`
    fn appendNextStandaloneExpression(self: *Self, tab: u16) ParserError!NodeIndex {
        try self.assertAndConsumeNextTokenIf(.spacing, expected_spacing);

        switch (try self.peekToken(0)) {
            .starts_upper, .number => {
                const atomic_index = try self.justAppendNode(Node{
                    .atomic_token = self.farthest_token_index,
                });
                self.farthest_token_index += 1;

                return atomic_index;
            },
            .operator => |operator| {
                if (!operator.isPrefixable()) {
                    self.tokenizer.addErrorAt(self.farthest_token_index, "not a prefix operator");
                    return ParserError.syntax;
                }
                self.farthest_token_index += 1;
                // TODO: multiple prefix operators probably don't work here.
                return try self.justAppendNode(Node{ .prefix = .{
                    .operator = operator,
                    .node = try self.appendNextStandaloneExpression(tab),
                } });
            },
            else => {
                self.tokenizer.addErrorAt(self.farthest_token_index, "expected an expression");
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
            // lower precedence means higher priority.
            if (left.operation().precedence(Operation.Compare.on_left) <= operation_precedence) {
                // `left` has higher priority; we should continue up the hierarchy
                // until we find the spot that this new operation should take.
                _ = hierarchy.pop();
            } else {
                switch (left.*) {
                    .binary => |*left_binary| {
                        // `operation` has higher priority, so we need to invert the nodes a bit.
                        const boundary_index = left_binary.right;
                        left_binary.right = try self.justAppendNode(.{ .postfix = .{
                            .operator = operation.operator,
                            .node = boundary_index,
                        } });
                    },
                    else => return ParserError.unimplemented,
                }
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
            // lower precedence means higher priority.
            if (left.operation().precedence(Operation.Compare.on_left) <= operation_precedence) {
                // `left` has higher priority; we should continue up the hierarchy
                // until we find the spot that this new operation should take.
                _ = hierarchy.pop();
            } else {
                switch (left.*) {
                    .binary => |*left_binary| {
                        // `operation` has higher priority, so we need to invert the nodes a bit.
                        const boundary_index = left_binary.right;
                        left_binary.right = try self.justAppendNode(.{ .binary = .{
                            .operator = operation.operator,
                            .left = boundary_index,
                            .right = right_index,
                        } });
                    },
                    else => return ParserError.unimplemented,
                }
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
        self.tokenizer.addErrorAt(self.farthest_token_index, error_message);
        return ParserError.syntax;
    }

    const Self = @This();
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
        Node{ .statement = .{ .node = 6, .tab = 0 } },
        // [5]:
        Node{ .atomic_token = 9 }, // 1.234
        Node{ .prefix = .{ .operator = Operator.plus, .node = 5 } },
        Node{ .statement = .{ .node = 9, .tab = 2 } },
        Node{ .atomic_token = 14 }, // 5.678
        Node{ .prefix = .{ .operator = Operator.minus, .node = 8 } },
        // [10]:
        Node{ .statement = .{ .node = 12, .tab = 4 } },
        Node{ .atomic_token = 19 }, // Foe
        Node{ .prefix = .{ .operator = Operator.increment, .node = 11 } },
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
    try std.testing.expectEqual(Node.Statement{ .node = 6, .tab = 0 }, try parser.at(2));
    try std.testing.expectEqual(Node.Statement{ .node = 9, .tab = 2 }, try parser.at(3));
    try std.testing.expectEqual(Node.Statement{ .node = 12, .tab = 4 }, try parser.at(4));
}

test "parser multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Theta * 3.14"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        Node{ .statement = .{ .node = 3, .tab = 0 } },
        Node{ .atomic_token = 1 }, // "Theta"
        Node{ .atomic_token = 5 }, // "3.14"
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 2 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
    });
}

test "parser implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Pi Sky"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        Node{ .statement = .{ .node = 3, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Pi
        Node{ .atomic_token = 3 }, // Sky
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 1, .right = 2 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
    });
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
    // TODO: try parser.tokenizer.file.lines.append(try SmallString.init("Wobdash * Flobsmash--"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 4, .tab = 0 } },
        Node{ .atomic_token = 3 }, // Theta
        Node{ .prefix = .{ .operator = Operator.increment, .node = 1 } },
        Node{ .atomic_token = 7 }, // Beta
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 3 } },
        // [5]:
        Node{ .statement = .{ .node = 9, .tab = 0 } },
        Node{ .atomic_token = 10 }, // Zeta
        Node{ .atomic_token = 16 }, // Woga
        Node{ .prefix = .{ .operator = Operator.increment, .node = 7 } },
        Node{ .binary = .{ .operator = Operator.multiply, .left = 6, .right = 8 } },
        // [10]:
        Node{ .statement = .{ .node = 14, .tab = 0 } },
        Node{ .atomic_token = 19 }, // Yodus
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 11 } },
        Node{ .atomic_token = 25 }, // Spatula
        Node{ .binary = .{ .operator = Operator.multiply, .left = 12, .right = 13 } },
        // [15]:
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        5,
        10,
    });
}

test "complicated prefix/postfix operators with multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    // TODO: try parser.tokenizer.file.lines.append(try SmallString.init("Apple * Berry Cantaloupe--"));
    // TODO: try parser.tokenizer.file.lines.append(try SmallString.init("Apple * ++Berry Cantaloupe"));
    // TODO: try parser.tokenizer.file.lines.append(try SmallString.init("--Xeno Yak * Zelda"));
    // TODO: try parser.tokenizer.file.lines.append(try SmallString.init("Xeno Yak++ * Zelda"));
}

test "nested prefix/postfix operators" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    // TODO: try parser.tokenizer.file.lines.append(try SmallString.init("Berry Cantaloupe--!"));
    // TODO: try parser.tokenizer.file.lines.append(try SmallString.init("!++Berry Cantaloupe"));
    // TODO: try parser.tokenizer.file.lines.append(try SmallString.init("Yuck * Zoldy++!"));
    // TODO: try parser.tokenizer.file.lines.append(try SmallString.init("!--Xeno Yak * Zelda"));
}

test "order of operations with addition and multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    // TODO
    //try parser.tokenizer.file.lines.append(try SmallString.init("Theta * Beta + Zeta"));
    //try parser.tokenizer.file.lines.append(try SmallString.init("Panda + K_panda * 1000"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{});
}
