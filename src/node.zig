const SmallString = @import("string.zig").Small;
const operator_zig = @import("operator.zig");
const Operator = operator_zig.Operator;
const common = @import("common.zig");

const std = @import("std");

pub const TokenIndex = usize;
pub const NodeIndex = usize;

const NodeTag = enum {
    statement,
    atomic_token,
    prefix,
    postfix,
    binary,
    end,
};

const NodeError = error{not_allowed};

pub const Node = union(NodeTag) {
    statement: Statement,
    atomic_token: TokenIndex,
    prefix: PrefixNode,
    postfix: PostfixNode,
    binary: BinaryNode,
    end: void,
    // TODO: function_call: {function_name: NodeIndex, first_argument: Argument}
    // TODO: Argument: {name: NodeIndex, value: NodeIndex, next_argument: NodeIndex}

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

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        switch (self) {
            .statement => |statement| {
                try writer.print("Node{{ .statement = .{{ .node = {d}, .tab = {d} }} }}", .{ statement.node, statement.tab });
            },
            .atomic_token => |token_index| {
                try writer.print("Node{{ .atomic_token = {d} }}", .{token_index});
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
    pub const Statement = StatementNode;
    pub const Binary = BinaryNode;
    pub const Prefix = PrefixNode;
    pub const Postfix = PostfixNode;
    pub const Operation = operator_zig.Operation;
    pub const Error = NodeError;
    const Self = @This();
};

const StatementNode = struct {
    node: NodeIndex = 0,
    tab: u16 = 0,

    pub fn equals(a: Self, b: Self) bool {
        return a.node == b.node and a.tab == b.tab;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

const BinaryNode = struct {
    operator: Operator = .none,
    left: NodeIndex = 0,
    right: NodeIndex = 0,

    pub fn operation(self: Self) operator_zig.Operation {
        return .{ .type = .infix, .operator = self.operator };
    }

    pub fn equals(a: Self, b: Self) bool {
        return a.operator == b.operator and a.left == b.left and a.right == b.right;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

const PrefixNode = struct {
    operator: Operator = .none,
    node: NodeIndex = 0,

    pub fn operation(self: Self) operator_zig.Operation {
        return .{ .type = .prefix, .operator = self.operator };
    }

    pub fn equals(a: Self, b: Self) bool {
        return a.operator == b.operator and a.node == b.node;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

const PostfixNode = struct {
    operator: Operator = .none,
    node: NodeIndex = 0,

    pub fn operation(self: Self) operator_zig.Operation {
        return .{ .type = .postfix, .operator = self.operator };
    }

    pub fn equals(a: Self, b: Self) bool {
        return a.operator == b.operator and a.node == b.node;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

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
