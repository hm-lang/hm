const SmallString = @import("string.zig").Small;

const std = @import("std");

const NodeTag = enum {
    invalid,
    atomic_token,
    prefix,
    postfix,
    binary,
};

pub const Node = union(NodeTag) {
    invalid: void,
    atomic_token: TokenIndex,
    prefix: PrefixNode,
    postfix: PostfixNode,
    binary: BinaryNode,
    // TODO: function_call: {function_name: NodeIndex, first_argument: Argument}
    // TODO: Argument: {name: NodeIndex, value: NodeIndex, next_argument: NodeIndex}

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        switch (self) {
            .invalid => try writer.print(".invalid", .{}),
            .atomic_token => |token_index| {
                try writer.print("Node{{ .atomic_token = {d} }}", .{token_index});
            },
            .prefix => |prefix| {
                try writer.print("Node{{ .prefix = .{{ .operator = SmallString.as64(\"", .{});
                try SmallString.init64(prefix.operator).print(writer);
                try writer.print("\"), .node = {d} }} }}", .{prefix.node});
            },
            .postfix => |postfix| {
                try writer.print("Node{{ .postfix = .{{ .operator = SmallString.as64(\"", .{});
                try SmallString.init64(postfix.operator).print(writer);
                try writer.print("\"), .node = {d} }} }}", .{postfix.node});
            },
            .binary => |binary| {
                try writer.print("Node{{ .binary = .{{ .operator = SmallString.as64(\"", .{});
                try SmallString.init64(binary.operator).print(writer);
                try writer.print("\"), .left = {d}, .right = {d} }} }}", .{binary.left, binary.right});
            },
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
        const stderr = std.io.getStdErr().writer();
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
        const stderr = std.io.getStdErr().writer();
        errdefer {
            stderr.print("expected NOT this, but got it:\n", .{}) catch {};
            a.printLine(stderr) catch {};
        }
        try std.testing.expect(!a.equals(b));
    }

    pub const Tag = NodeTag;
    pub const Binary = BinaryNode;
    pub const Prefix = PrefixNode;
    pub const Postfix = PostfixNode;
    const Self = @This();
};

const BinaryNode = struct {
    operator: u64 = 0,
    left: NodeIndex = 0,
    right: NodeIndex = 0,

    pub fn equals(a: Self, b: Self) bool {
        return a.operator == b.operator and a.left == b.left and a.right == b.right;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

const PrefixNode = struct {
    operator: u64 = 0,
    node: NodeIndex = 0,

    pub fn equals(a: Self, b: Self) bool {
        return a.operator == b.operator and a.node == b.node;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

const PostfixNode = struct {
    operator: u64 = 0,
    node: NodeIndex = 0,

    pub fn equals(a: Self, b: Self) bool {
        return a.operator == b.operator and a.node == b.node;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        try std.testing.expect(a.equals(b));
    }

    const Self = @This();
};

pub const TokenIndex = usize;
pub const NodeIndex = usize;

test "node equality" {
    const invalid: Node = .invalid;
    try invalid.expectEquals(invalid);

    const postfix = Node{ .postfix = .{ .operator = SmallString.as64("++"), .node = 123 } };
    try invalid.expectNotEquals(postfix);
    try postfix.expectEquals(postfix);
    try postfix.expectNotEquals(Node{ .postfix = .{ .operator = SmallString.as64("++"), .node = 124 } } );
    try postfix.expectNotEquals(Node{ .postfix = .{ .operator = SmallString.as64("+-"), .node = 123 } } );

    const prefix = Node{ .prefix = .{ .operator = SmallString.as64("++"), .node = 123 } };
    try prefix.expectNotEquals(postfix);
    try prefix.expectEquals(prefix);
    try prefix.expectNotEquals(Node{ .prefix = .{ .operator = SmallString.as64("++"), .node = 124 } } );
    try prefix.expectNotEquals(Node{ .prefix = .{ .operator = SmallString.as64("+-"), .node = 123 } } );

    const binary = Node{ .binary = .{ .operator = SmallString.as64("++"), .left = 5, .right = 7 } };
    try binary.expectNotEquals(postfix);
    try binary.expectEquals(binary);
    try binary.expectNotEquals(Node{ .binary = .{ .operator = SmallString.as64("++"), .left = 6, .right = 7 } } );
    try binary.expectNotEquals(Node{ .binary = .{ .operator = SmallString.as64("++"), .left = 5, .right = 8 } } );
    try binary.expectNotEquals(Node{ .binary = .{ .operator = SmallString.as64("-+"), .left = 5, .right = 7 } } );
}
