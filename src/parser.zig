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
        const left_index = try self.appendNextStandaloneExpression(0);
        const operator = try self.seekNextInfixOperator(tab);
        if (operator == .none) {
            self.assertAndConsumeNextTokenIf(.newline, "expected newline") catch {
                try self.assertAndConsumeNextTokenIf(.end, "expected EOF");
            };
            return left_index;
        }
        // Greedily parse everything else...
        const right_index = try self.appendNextExpression(tab);
        // But we may need to open up `right_node` and update its left-most connection
        // in case `operator` is stronger than any internal operations in `right_node`.
        const right_node = &self.nodes.array.items[right_index];
        return switch (right_node.*) {
            // e.g., `A * B` or `A - ++B`, simple cases where we don't have to worry about
            // operator precedence.
            .atomic_token, .prefix => try self.justAppendNode(Node{ .binary = .{
                .operator = operator,
                .left = left_index,
                .right = right_index,
            } }),
            else => ParserError.unimplemented,
        };
    }

    fn seekNextInfixOperator(self: *Self, tab: u16) ParserError!Operator {
        switch (try self.peekToken(0)) {
            // This was the last atom in the row.
            .newline => {
                // TODO: check if we had a double-indent on the next line and continue parsing if so.
                _ = tab;
                return .none;
            },
            .end => return .none,
            else => try self.assertAndConsumeNextTokenIf(.spacing, expected_spacing),
        }

        switch (try self.peekToken(0)) {
            .operator => |operator| {
                if (operator.isInfixable()) {
                    self.farthest_token_index += 1;
                    return operator;
                } else if (operator.isPrefixable()) {
                    // Back up so that the next standalone expression starts at the
                    // spacing before this prefix:
                    self.farthest_token_index -= 1;
                    return Operator.implicit_member_access;
                } else {
                    // If this operator was on the same line, we would have grabbed it with
                    // `appendNextStandaloneExpression`.  If we're here, that means it came
                    // on a second line.
                    self.tokenizer.addErrorAt(self.farthest_token_index, "use postfix operator on same line");
                    return ParserError.syntax;
                }
            },
            else => {
                // We encountered another realizable token, back up so that
                // we maintain the invariant that there's a space before the next real element.
                self.farthest_token_index -= 1;
                return Operator.implicit_member_access;
            },
        }

        (try self.peekToken(0)).printLine(common.debugStderr) catch {};
        self.tokenizer.addErrorAt(self.farthest_token_index, "expected end of line");
        return ParserError.unimplemented;
    }

    /// Adds an atom with possible prefix/postfix operators.
    /// Includes things like `1.234`, `My_variable`, `+4.56`, `-7.89`,
    /// `++Index` or `Countdown--` as well.  For member access like
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

                switch (try self.peekToken(0)) {
                    // TODO: in case of newline, try parsing the next row if at tab + 8
                    .newline, .end => return atomic_index,
                    else => try self.assertAndConsumeNextTokenIf(.spacing, expected_spacing),
                }

                // Check if there was a postfix operator.
                switch (try self.peekToken(0)) {
                    .operator => |operator| {
                        if (operator.isPostfixable()) {
                            self.farthest_token_index += 1;
                            return try self.justAppendNode(Node{ .prefix = .{
                                .operator = operator,
                                .node = atomic_index,
                            } });
                        }
                    },
                    else => {},
                }
                // We actually want to back up so that we maintain the invariant
                // that there is spacing before each next real element.
                self.farthest_token_index -= 1;
                return atomic_index;
            },
            .operator => |operator| {
                if (!operator.isPrefixable()) {
                    self.tokenizer.addErrorAt(self.farthest_token_index, "not a prefix operator");
                    return ParserError.syntax;
                }
                self.farthest_token_index += 1;
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

    fn shouldOperateLeftToRight(left: Operation, right: Operation) bool {
        // lower precedence means higher priority.
        return left.precedence(Operation.Compare.on_left) <= right.precedence(Operation.Compare.on_right);
    }

    // TODO: maybe return operator
    //    fn maybeAppendInfix(self: *Self) ParserError!NodeIndex {
    //
    //    }

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
    try parser.tokenizer.file.lines.append(try SmallString.init("    Fum--"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        Node{ .statement = .{ .node = 1, .tab = 0 } },
        Node{ .atomic_token = 1 }, // points to token 1 which is 3.456
        Node{ .statement = .{ .node = 3, .tab = 4 } },
        Node{ .atomic_token = 4 }, // Hello_you
        Node{ .statement = .{ .node = 6, .tab = 0 } },
        Node{ .atomic_token = 9 }, // 1.234
        Node{ .prefix = .{ .operator = Operator.plus, .node = 5 } },
        Node{ .statement = .{ .node = 9, .tab = 2 } },
        Node{ .atomic_token = 14 }, // 5.678
        Node{ .prefix = .{ .operator = Operator.negative, .node = 8 } },
        Node{ .statement = .{ .node = 12, .tab = 4 } },
        Node{ .atomic_token = 19 }, // Foe
        Node{ .prefix = .{ .operator = Operator.increment, .node = 11 } },
        Node{ .statement = .{ .node = 15, .tab = 4 } },
        Node{ .atomic_token = 22 }, // Fum
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 14 } },
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
    try parser.tokenizer.file.lines.append(try SmallString.init("Theta Beta"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        Node{ .statement = .{ .node = 3, .tab = 0 } },
        Node{ .atomic_token = 1 }, // "Theta"
        Node{ .atomic_token = 3 }, // "Beta"
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 1, .right = 2 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
    });
}

test "prefix/postfix operators with multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("++Theta * Beta"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Zeta * ++Woga"));
    // TODO: try parser.tokenizer.file.lines.append(try SmallString.init("Wobdash * Flobsmash--"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        Node{ .statement = .{ .node = 4, .tab = 0 } },
        Node{ .atomic_token = 3 },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 1 } },
        Node{ .atomic_token = 7 },
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 3 } },
        Node{ .statement = .{ .node = 9, .tab = 0 } },
        Node{ .atomic_token = 10 },
        Node{ .atomic_token = 16 },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 7 } },
        Node{ .binary = .{ .operator = Operator.multiply, .left = 6, .right = 8 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        5,
    });
}
