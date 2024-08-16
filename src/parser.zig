const common = @import("common.zig");
const OwnedList = @import("owned_list.zig").OwnedList;
const SmallString = @import("string.zig").Small;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Token = @import("token.zig").Token;
const node = @import("node.zig");

const Node = node.Node;
const TokenIndex = node.TokenIndex;
const NodeIndex = node.NodeIndex;

const OwnedNodes = OwnedList(Node);
const OwnedNodeIndices = OwnedList(NodeIndex);

const ParserError = error{
    out_of_memory,
    out_of_statements,
    broken_invariant,
};

pub const Parser = struct {
    // The parser will free this at the end.
    tokenizer: Tokenizer = .{},
    nodes: OwnedNodes = OwnedNodes.init(),
    statement_indices: OwnedNodeIndices = OwnedNodeIndices.init(),
    last_statement_index: usize = std.math.maxInt(usize),
    farthest_token_index: usize = 0,

    pub fn deinit(self: *Self) void {
        self.tokenizer.deinit();
        self.statement_indices.deinit();
        self.nodes.deinit();
    }

    pub fn at(self: *Self, statement_index: usize) ParserError!Node.Statement {
        if (statement_index > self.last_statement_index) {
            return ParserError.out_of_statements;
        }
        while (statement_index >= self.statement_indices.count()) {
            try self.addNextStatement();
            if (statement_index > self.last_statement_index) {
                return ParserError.out_of_statements;
            }
        }
        const node_index = self.statement_indices.at(statement_index) orelse return ParserError.broken_invariant;
        return switch (self.nodes.inBounds(node_index)) {
            .statement => |statement| statement,
            else => ParserError.broken_invariant,
        };
    }

    pub fn complete(self: *Self) ParserError!void {
        while (self.last_node_index >= self.nodes.count()) {
            try self.addNextStatement();
        }
    }

    fn addNextStatement(self: *Self) ParserError!void {
        const statement_index = self.nodes.count();
        // Insert the node which will become the statement first.
        // Otherwise we'd need to calculate `statement_index` *after* `getNextStatement()`,
        // since there will be additional nodes added here which comprise the statement.
        // Theoretically we also could keep track of statements in a separate array,
        // but we do this in case statements refer to other statements...
        try self.justAppendNode(Node.Statement{});
        self.statement_indices.append(statement_index) catch {
            return ParserError.out_of_memory;
        };
        self.nodes.items[statement_index] = Node{ .statement = try self.getNextStatement() };
    }

    fn getNextStatement(self: *Self) ParserError!Node.Statement {
        // TODO
    }

    fn justAppendNode(self: *Self, node: Node) ParserError!void {
        self.nodes.append(node) catch {
            return ParserError.out_of_memory;
        };
    }

    const Self = @This();
};
