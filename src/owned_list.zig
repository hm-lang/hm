const common = @import("common.zig");

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const OwnedListError = error{
    out_of_bounds,
    out_of_memory,
};

pub fn OwnedList(comptime T: type) type {
    return struct {
        const Self = @This();

        array: std.ArrayListUnmanaged(T) = std.ArrayListUnmanaged(T){},

        pub inline fn init() Self {
            return .{};
        }

        // This will take ownership of the items.
        pub inline fn of(my_items: []const T) OwnedListError!Self {
            var result = Self{};
            try result.appendAll(my_items);
            return result;
        }

        pub inline fn deinit(self: *Self) void {
            if (std.meta.hasMethod(T, "deinit")) {
                self.clear();
            }
            self.array.deinit(common.allocator);
        }

        pub inline fn items(self: *const Self) []T {
            return self.array.items;
        }

        pub inline fn count(self: *const Self) usize {
            return self.array.items.len;
        }

        pub inline fn set(self: *Self, at_index: anytype, new_value: T) OwnedListError!void {
            const count_i64: i64 = @intCast(self.count());
            std.debug.assert(count_i64 >= 0);
            var index: i64 = @intCast(at_index);
            if (index < 0) {
                index += count_i64;
                if (index < 0) {
                    return OwnedListError.out_of_bounds;
                }
            } else if (index < count_i64) {
                // pass
            } else if (index == count_i64) {
                try self.append(new_value);
                return;
            } else {
                // TODO: try to resize and fill with defaults
                return OwnedListError.out_of_bounds;
            }
            self.array.items[@intCast(index)] = new_value;
        }

        /// If in bounds, returns non-null; if `at_index < 0` then will
        /// try to return from the end of the list.
        /// Returns a "reference" -- don't free it.
        pub inline fn at(self: *const Self, at_index: anytype) ?T {
            const count_i64: i64 = @intCast(self.count());
            std.debug.assert(count_i64 >= 0);
            var index: i64 = @intCast(at_index);
            if (index < 0) {
                index += count_i64;
                if (index < 0) {
                    return null;
                }
            } else if (index >= count_i64) {
                return null;
            }
            return self.array.items[@intCast(index)];
        }

        pub inline fn before(self: *const Self, index: usize) ?T {
            const before_index = common.before(index) orelse return null;
            return self.at(before_index);
        }

        /// Returns a "reference" -- don't free it.
        pub inline fn inBounds(self: *const Self, index: usize) T {
            std.debug.assert(index < self.count());
            return self.array.items[index];
        }

        pub inline fn remove(self: *Self, index: usize) ?T {
            return if (index < self.count())
                self.array.orderedRemove(index)
            else
                null;
        }

        pub inline fn pop(self: *Self) ?T {
            const last_index = common.before(self.count()) orelse return null;
            return self.array.orderedRemove(last_index);
        }

        /// This list will take ownership of `t`.
        pub inline fn append(self: *Self, t: T) OwnedListError!void {
            self.array.append(common.allocator, t) catch {
                return OwnedListError.out_of_memory;
            };
        }

        /// This list will take ownership of all `items`.
        pub inline fn appendAll(self: *Self, more_items: []const T) OwnedListError!void {
            self.array.appendSlice(common.allocator, more_items) catch {
                return OwnedListError.out_of_memory;
            };
        }

        pub inline fn insert(self: *Self, at_index: usize, item: T) OwnedListError!void {
            assert(at_index <= self.count());
            self.array.insert(common.allocator, at_index, item) catch {
                return OwnedListError.out_of_memory;
            };
        }

        pub inline fn clear(self: *Self) void {
            if (std.meta.hasMethod(T, "deinit")) {
                // TODO: for some reason, this doesn't work (we get a `const` cast problem;
                //      `t` appears to be a `*const T` instead of a `*T`).
                //while (self.array.popOrNull()) |*t| {
                //    t.deinit();
                //}
                while (true) {
                    var t: T = self.array.popOrNull() orelse break;
                    t.deinit();
                }
            } else {
                self.array.clearRetainingCapacity();
            }
        }

        pub inline fn expectEquals(self: Self, other: Self) !void {
            try self.expectEqualsSlice(other.items());
        }

        pub fn expectEqualsSlice(self: Self, other: []const T) !void {
            const stderr = std.io.getStdErr().writer();
            errdefer {
                stderr.print("expected:\n", .{}) catch {};
                common.printSliceLine(other, stderr) catch {};

                stderr.print("got:\n", .{}) catch {};
                self.printLine(stderr) catch {};
            }
            for (0..@min(self.count(), other.len)) |index| {
                const self_item = self.inBounds(index);
                const other_item = other[index];
                if (std.meta.hasMethod(T, "expectEquals")) {
                    self_item.expectEquals(other_item) catch |e| {
                        stderr.print("\nnot equal at index {d}\n\n", .{index}) catch {};
                        return e;
                    };
                } else {
                    try std.testing.expectEqual(other_item, self_item);
                }
            }
            try std.testing.expectEqual(other.len, self.count());
        }

        pub fn printLine(self: Self, writer: anytype) !void {
            try self.print(writer);
            try writer.print("\n", .{});
        }

        pub fn print(self: Self, writer: anytype) !void {
            try common.printSlice(self.items(), writer);
        }
    };
}

// TODO: tests

test "insert works at end" {
    var list = OwnedList(u32).init();
    defer list.deinit();

    try list.insert(0, 54);
    try list.insert(1, 55);
    try list.append(56);
    try list.insert(3, 57);

    try std.testing.expectEqualSlices(u32, list.items(), &[_]u32{ 54, 55, 56, 57 });
}

test "insert works at start" {
    var list = OwnedList(u8).init();
    defer list.deinit();

    try list.insert(0, 100);
    try list.insert(0, 101);
    try list.insert(0, 102);

    try std.testing.expectEqualSlices(u8, list.items(), &[_]u8{ 102, 101, 100 });
}

test "clear gets rid of everything" {
    const SmallString = @import("string.zig").Small;
    var list = OwnedList(SmallString).init();
    defer list.deinit();

    try list.append(try SmallString.init("over fourteen" ** 4));
    try list.append(try SmallString.init("definitely allocated" ** 10));
    try std.testing.expectEqual(2, list.count());

    list.clear();

    try std.testing.expectEqual(0, list.count());
}
