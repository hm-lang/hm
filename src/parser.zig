const common = @import("common.zig");
const OwnedList = @import("owned_list.zig").OwnedList;
const SmallString = @import("string.zig").Small;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Token = @import("token.zig").Token;
const node_zig = @import("node.zig");

const Node = node_zig.Node;
const TokenIndex = node_zig.TokenIndex;
const NodeIndex = node_zig.NodeIndex;

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
        const index = self.appendNextStandaloneExpression(0);
        switch (try self.peekToken(0)) {
            // This was the last atom in the row.
            .newline => {
                // TODO: check if we had a double-indent on the next line and continue parsing if so.
                _ = tab;
                self.farthest_token_index += 1;
                return index;
            },
            else => {},
        }
        const stderr = std.io.getStdErr().writer();
        (try self.peekToken(0)).printLine(stderr) catch {};
        self.tokenizer.addErrorAt(self.farthest_token_index, "expected end of line");
        return ParserError.unimplemented;
    }

    /// Adds an atom with possible prefix/postfix operators.
    /// Includes things like `1.234`, `My_variable`, `+4.56`, `-7.89`,
    /// `++Index` or `Countdown--` as well.  For member access like
    /// `First_identifier Second_identifier`, just grab the first one.
    // TODO: also include operations like `my_function(...)`
    fn appendNextStandaloneExpression(self: *Self, left_node: NodeIndex) ParserError!NodeIndex {
        _ = left_node;
        // We expect spacing before each identifier.
        switch (try self.peekToken(0)) {
            .spacing => {},
            else => return ParserError.broken_invariant,
        }
        self.farthest_token_index += 1;

        switch (try self.peekToken(0)) {
            .starts_upper, .number => {
                const atomic_index = try self.justAppendNode(Node{
                    .atomic_token = self.farthest_token_index,
                });
                self.farthest_token_index += 1;

                switch (try self.peekToken(0)) {
                    .newline => return atomic_index,
                    .spacing => {}, // ignore spaces
                    else => return ParserError.broken_invariant,
                }

                // Check if there was a postfix operator.
                switch (try self.peekToken(1)) {
                    .operator => |operator| {
                        if (Token.isPostfixable(operator)) {
                            // TODO: precedence.  next node might take this prefix.
                            self.farthest_token_index += 2;
                            return try self.justAppendNode(Node{ .prefix = .{
                                .operator = operator,
                                .node = atomic_index,
                            } });
                        }
                    },
                    else => {},
                }

                return atomic_index;
            },
            .operator => |operator| {
                if (!Token.isPrefixable(operator)) {
                    self.tokenizer.addErrorAt(self.farthest_token_index, "not a prefix operator");
                    return ParserError.syntax;
                }
                self.farthest_token_index += 1;
                // TODO: we need order of operations
                return try self.justAppendNode(Node{ .prefix = .{
                    .operator = operator,
                    .node = try self.appendNextStandaloneExpression(0),
                } });
            },
            else => {
                self.tokenizer.addErrorAt(self.farthest_token_index, "expected an expression");
                return ParserError.syntax;
            },
        }
    }

    fn shouldInvertNodes(left: Node.Operation, right: Node.Operation) bool {
        // lower precedence means higher priority.
        return right.precedence(Node.Operation.Compare.on_right) < left.precedence(Node.Operation.Compare.on_left);
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

    const Self = @This();
};

test "parser simple expressions" {
    var parser: Parser = .{};
    defer parser.deinit();
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
        Node{ .prefix = .{ .operator = SmallString.as64("+"), .node = 5 } },
        Node{ .statement = .{ .node = 9, .tab = 2 } },
        Node{ .atomic_token = 14 }, // 5.678
        Node{ .prefix = .{ .operator = SmallString.as64("-"), .node = 8 } },
        Node{ .statement = .{ .node = 12, .tab = 4 } },
        Node{ .atomic_token = 19 }, // Foe
        Node{ .prefix = .{ .operator = SmallString.as64("++"), .node = 11 } },
        Node{ .statement = .{ .node = 15, .tab = 4 } },
        Node{ .atomic_token = 22 }, // Fum
        Node{ .prefix = .{ .operator = SmallString.as64("--"), .node = 14 } },
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
