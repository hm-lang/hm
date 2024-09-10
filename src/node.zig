const SmallString = @import("string.zig").Small;
const operator_zig = @import("operator.zig");
const Operator = operator_zig.Operator;
const Token = @import("token.zig").Token;
const common = @import("common.zig");

const std = @import("std");

pub const TokenIndex = usize;
pub const NodeIndex = usize;

const NodeTag = enum {
    /// includes things like blocks which don't have an explicit `open`.
    // TODO: add an `open` field into `block`.
    // the root block has `.tab = 0` and `.open = .brace`.
    enclosed,
    /// statements are essentially a singly-linked list of lines in a block.
    /// they can optionally include an indented block that immediately follows.
    /// since each newline corresponds to a comma operator, these used for commas
    /// as well.  commas are special because they are essentially lowest priority operators
    /// that create new statements in the current block, and we want to make blocks
    /// not need a depth-first search to find the first statement.  (LTR operators
    /// essentially stack the left operand, so if it was an operator it'd be, e.g.,
    /// `[Op: ',', Left: [Op: ',', Left: [Statement1], Right: [Statement2]], Right: [Statement3]]`
    /// where we want to make it like this instead:
    /// `[Node: [Statement1], Next: [Node: [Statement2], Next: [Statement3]]]`.)
    statement,
    // TODO: split into `identifier` and `number` nodes, probably...
    atomic_token,
    /// Might be a function, might be a type.
    callable_token,
    prefix,
    postfix,
    binary,
    interpolation,
    // TODO: rename to `file_end` or something
    end,
};

const NodeError = error{not_allowed};

pub const Node = union(NodeTag) {
    enclosed: EnclosedNode,
    statement: StatementNode,
    atomic_token: TokenIndex,
    callable_token: TokenIndex,
    prefix: PrefixNode,
    postfix: PostfixNode,
    binary: BinaryNode,
    string_interpolation: StringInterpolationNode,
    end: void,

    pub fn operation(self: Self) Operation {
        return switch (self) {
            .prefix => |prefix| prefix.operation(),
            .postfix => |postfix| postfix.operation(),
            .binary => |binary| binary.operation(),
            else => .{},
        };
    }

    /// Swaps out the current "right" operand with the new `NodeIndex`.
    /// Returns the old "right" operand.
    pub fn swapRight(self: *Self, new_index: NodeIndex) NodeError!NodeIndex {
        switch (self.*) {
            .binary => |*binary| {
                const old_index = binary.right;
                binary.right = new_index;
                return old_index;
            },
            .prefix => |*prefix| {
                const old_index = prefix.node;
                prefix.node = new_index;
                return old_index;
            },
            else => return NodeError.not_allowed,
        }
    }

    pub fn setStatementNext(self: *Self, next_index: NodeIndex) NodeError!void {
        switch (self.*) {
            .statement => |*statement| {
                statement.next = next_index;
            },
            else => return NodeError.not_allowed,
        }
    }

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        switch (self) {
            .enclosed => |enclosed| {
                try writer.print("Node{{ .enclosed = .{{ .open = .", .{});
                try enclosed.open.print(writer);
                try writer.print(", .inner_tab = {d}, .outer_tab = {d}, .start = {d} }} }}", .{
                    enclosed.inner_tab,
                    enclosed.outer_tab,
                    enclosed.start,
                });
            },
            .statement => |statement| {
                try writer.print("Node{{ .statement = .{{ .node = {d}, .next = {d} }} }}", .{ statement.node, statement.next });
            },
            .atomic_token => |token_index| {
                try writer.print("Node{{ .atomic_token = {d} }}", .{token_index});
            },
            .callable_token => |token_index| {
                try writer.print("Node{{ .callable_token = {d} }}", .{token_index});
            },
            .prefix => |prefix| {
                try writer.print("Node{{ .prefix = .{{ .operator = ", .{});
                try prefix.operator.print(writer);
                try writer.print(", .node = {d} }} }}", .{prefix.node});
            },
            .postfix => |postfix| {
                try writer.print("Node{{ .postfix = .{{ .operator = ", .{});
                try postfix.operator.print(writer);
                try writer.print(", .node = {d} }} }}", .{postfix.node});
            },
            .comma => |comma| {
                try writer.print("Node{{ .comma = .{{ .node = {d}, .next = {d} }} }}", .{ comma.node, comma.next });
            },
            .binary => |binary| {
                try writer.print("Node{{ .binary = .{{ .operator = ", .{});
                try binary.operator.print(writer);
                try writer.print(", .left = {d}, .right = {d} }} }}", .{ binary.left, binary.right });
            },
            .end => try writer.print(".end", .{}),
        }
    }

    pub fn equals(a: Self, b: Self) bool {
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        if (tag_a != tag_b) return false;

        const info = switch (@typeInfo(Self)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(Tag, field_info.name) == tag_a) {
                const SubField = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(SubField, "equals")) {
                    return @field(a, field_info.name).equals(@field(b, field_info.name));
                } else {
                    return @field(a, field_info.name) == @field(b, field_info.name);
                }
            }
        }
        return false;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        const stderr = common.debugStderr;
        errdefer {
            stderr.print("expected:\n", .{}) catch {};
            b.printLine(stderr) catch {};

            stderr.print("got:\n", .{}) catch {};
            a.printLine(stderr) catch {};
        }
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        try std.testing.expectEqual(tag_b, tag_a);

        const info = switch (@typeInfo(Self)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(Tag, field_info.name) == tag_a) {
                const SubField = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(SubField, "expectEquals")) {
                    try @field(a, field_info.name).expectEquals(@field(b, field_info.name));
                    return;
                } else {
                    try std.testing.expectEqual(@field(b, field_info.name), @field(a, field_info.name));
                    return;
                }
            }
        }
        try std.testing.expect(false);
    }

    pub fn expectNotEquals(a: Self, b: Self) !void {
        const stderr = common.debugStderr;
        errdefer {
            stderr.print("expected NOT this, but got it:\n", .{}) catch {};
            a.printLine(stderr) catch {};
        }
        try std.testing.expect(!a.equals(b));
    }

    pub const Tag = NodeTag;
    pub const Enclosed = EnclosedNode;
    pub const Statement = StatementNode;
    pub const Prefix = PrefixNode;
    pub const Postfix = PostfixNode;
    pub const Binary = BinaryNode;
    pub const StringInterpolation = StringInterpolationNode;

    pub const Operation = operator_zig.Operation;
    pub const Error = NodeError;
    const Self = @This();
};

const EnclosedNode = struct {
    open: Token.BlockOpen,
    inner_tab: u16 = 0,
    outer_tab: u16,
    start: NodeIndex = 0,

    pub fn equals(a: Self, b: Self) bool {
        return a.open == b.open and a.inner_tab == b.inner_tab and a.outer_tab == b.outer_tab and a.start == b.start;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

const StatementNode = struct {
    node: NodeIndex = 0,
    next: NodeIndex = 0,

    pub fn operation(self: Self) operator_zig.Operation {
        _ = self;
        // we probably should never ask for a `StatementNode`'s operation.
        return .{ .type = .infix, .operator = .comma };
    }

    pub fn equals(a: Self, b: Self) bool {
        return a.node == b.node and a.next == b.next;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

const BinaryNode = struct {
    operator: Operator = .none,
    tab: u16 = 0,
    left: NodeIndex = 0,
    right: NodeIndex = 0,

    pub fn operation(self: Self) operator_zig.Operation {
        return .{ .type = .infix, .operator = self.operator };
    }

    pub fn equals(a: Self, b: Self) bool {
        return a.operator == b.operator and a.tab == b.tab and a.left == b.left and a.right == b.right;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

const PrefixNode = struct {
    operator: Operator = .none,
    tab: u16 = 0,
    node: NodeIndex = 0,

    pub fn operation(self: Self) operator_zig.Operation {
        return .{ .type = .prefix, .operator = self.operator };
    }

    pub fn equals(a: Self, b: Self) bool {
        return a.operator == b.operator and a.tab == b.tab and a.node == b.node;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

const PostfixNode = struct {
    operator: Operator = .none,
    tab: u16 = 0,
    node: NodeIndex = 0,

    pub fn operation(self: Self) operator_zig.Operation {
        return .{ .type = .postfix, .operator = self.operator };
    }

    pub fn equals(a: Self, b: Self) bool {
        return a.operator == b.operator and a.tab == b.tab and a.node == b.node;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

const StringInterpolationNode = struct {
    open: Token.StringOpen,
    root: NodeIndex = 0,

    pub fn equals(a: Self, b: Self) bool {
        return a.open == b.open and a.root == b.root;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

test "node size" {
    // 4 u64s worth of data, seems like a lot...
    try std.testing.expectEqual(4 * 8, @sizeOf(Node));
}

test "node equality" {
    const end: Node = .end;
    try end.expectEquals(end);

    const postfix = Node{ .postfix = .{ .operator = .increment, .node = 123 } };
    try end.expectNotEquals(postfix);
    try postfix.expectEquals(postfix);
    try postfix.expectNotEquals(Node{ .postfix = .{ .operator = .increment, .node = 124 } });
    try postfix.expectNotEquals(Node{ .postfix = .{ .operator = .plus, .node = 123 } });

    const prefix = Node{ .prefix = .{ .operator = .decrement, .node = 123 } };
    try prefix.expectNotEquals(postfix);
    try prefix.expectEquals(prefix);
    try prefix.expectNotEquals(Node{ .prefix = .{ .operator = .decrement, .node = 124 } });
    try prefix.expectNotEquals(Node{ .prefix = .{ .operator = .minus, .node = 123 } });

    const binary = Node{ .binary = .{ .operator = .plus, .left = 5, .right = 7 } };
    try binary.expectNotEquals(postfix);
    try binary.expectEquals(binary);
    try binary.expectNotEquals(Node{ .binary = .{ .operator = .plus, .left = 6, .right = 7 } });
    try binary.expectNotEquals(Node{ .binary = .{ .operator = .plus, .left = 5, .right = 8 } });
    try binary.expectNotEquals(Node{ .binary = .{ .operator = .minus, .left = 5, .right = 7 } });
}
