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
        const tab = switch (try self.peekToken()) {
            .spacing => |spacing| spacing.absolute,
            .end => return ParserError.out_of_statements,
            else => return ParserError.broken_invariant,
        };
        self.farthest_token_index += 1;

        const node_index = try self.appendNextExpression();

        switch (try self.peekToken()) {
            .newline => {},
            else => {
                self.tokenizer.addErrorAt(self.farthest_token_index, "expected end of line");
                return ParserError.syntax;
            },
        }
        self.farthest_token_index += 1;

        return .{ .tab = tab, .node = node_index };
    }

    fn appendNextExpression(self: *Self) ParserError!NodeIndex {
        switch (try self.peekToken()) {
            .starts_upper, .number => {
                const index = try self.justAppendNode(Node{
                    .atomic_token = self.farthest_token_index,
                });
                self.farthest_token_index += 1;
                // TODO: check for a postfix operator or an infix (then continue appendNextExpression)
                return index;
            },
            .operator => |operator| {
                if (!Token.isPrefixable(operator)) {
                    self.tokenizer.addErrorAt(self.farthest_token_index, "not a prefix operator");
                    return ParserError.syntax;
                }
                self.farthest_token_index += 1;
                // TODO: we need order of operations
                const prefix_index = try self.justAppendNode(Node{
                    .prefix = .{ .operator = operator, .node = try self.appendNextExpression() },
                });
                return prefix_index;
            },
            else => {
                self.tokenizer.addErrorAt(self.farthest_token_index, "expected an expression");
                return ParserError.syntax;
            },
        }
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
        return self.tokenizer.at(self.farthest_token_index) catch {
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

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        Node{ .statement = .{ .node = 1, .tab = 0 } },
        Node{ .atomic_token = 1 },
        Node{ .statement = .{ .node = 3, .tab = 4 } },
        Node{ .atomic_token = 4 },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        2,
    });
    try std.testing.expectEqual(Node.Statement{ .node = 1, .tab = 0 }, try parser.at(0));
    try std.testing.expectEqual(Node.Statement{ .node = 3, .tab = 4 }, try parser.at(1));
}
