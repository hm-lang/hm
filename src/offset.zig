pub const Offset = struct {
    inner_value: usize = 0,

    pub fn token(token_index: usize) Self {
        return Self{ .inner_value = token_index * 2 };
    }

    pub fn node(node_index: usize) Self {
        return Self{ .inner_value = node_index * 2 + 1 };
    }

    pub fn tokenOffset(self: Self) ?usize {
        if (self.inner_value % 2 == 0) return self.inner_value / 2;
        return null;
    }

    pub fn nodeOffset(self: Self) ?usize {
        if (self.inner_value % 2 == 1) return self.inner_value / 2;
        return null;
    }

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        try if (self.inner_value % 2 == 0) {
            writer.print("Offset.token({d})", .{self.inner_value / 2});
        } else {
            writer.print("Offset.node({d})", .{self.inner_value / 2});
        };
    }

    pub fn equals(a: Self, b: Self) bool {
        return a.inner_value == b.inner_value;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        std.testing.expectEqual(b.inner_value, a.inner_value);
    }

    pub fn expectNotEquals(a: Self, b: Self) !void {
        const stderr = std.io.getStdErr().writer();
        errdefer {
            stderr.print("expected NOT this, but got it:\n", .{}) catch {};
            a.printLine(stderr) catch {};
        }
        try std.testing.expect(!a.equals(b));
    }

    const Self = @This();
};
