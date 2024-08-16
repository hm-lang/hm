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
        // Notice that we should get more Nodes that comprise our `Statement`
        // before we even load the statement, but that's ok because we will
        // just grab the `statement_node_index` and update `statement_indices`
        // only after a successful result.
        const statement_node_index = try self.justAppendNode(Node{
            .statement = try self.getNextStatement(),
        });
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

        const token_index = self.farthest_token_index;
        switch (try self.peekToken()) {
            .starts_upper, .number => {},
            else => {
                self.tokenizer.addErrorAt(token_index, "expected variable or number");
                return ParserError.syntax;
            },
        }
        self.farthest_token_index += 1;

        // TODO: `var node` and update based on operators.
        const node = try self.justAppendNode(Node{ .atomic_token = token_index });

        switch (try self.peekToken()) {
            .newline => {},
            else => {
                self.tokenizer.addErrorAt(token_index, "expected end of line");
                return ParserError.syntax;
            },
        }
        self.farthest_token_index += 1;

        return .{ .tab = tab, .node = node };
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
        Node{ .atomic_token = 1 },
        // TODO: i'm not a big fan of node == 0 being a valid value.
        // we probably want to append the statement first, then update node.
        Node{ .statement = .{ .node = 0, .tab = 0 } },
        Node{ .atomic_token = 4 },
        Node{ .statement = .{ .node = 2, .tab = 4 } },
    });

    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        1,
    });
}
