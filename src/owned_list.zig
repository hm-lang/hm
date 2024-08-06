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

        pub fn init() Self {
            return .{};
        }

        pub fn deinit(self: *Self) void {
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
            }
            self.array.deinit(common.allocator);
        }

        pub inline fn items(self: *const Self) []T {
            return self.array.items;
        }

        pub inline fn count(self: *const Self) usize {
            return self.array.items.len;
        }

        /// Returns a "reference" -- don't free it.
        pub inline fn at(self: *Self, at_index: i64) OwnedListError!T {
            const count_i64: i64 = @intCast(self.count());
            var index: i64 = at_index;
            if (index < 0) {
                index += count_i64;
                if (index < 0) {
                    return OwnedListError.out_of_bounds;
                }
            } else if (index >= count_i64) {
                return OwnedListError.out_of_bounds;
            }
            return self.array.items[@intCast(index)];
        }

        /// Returns a "reference" -- don't free it.
        pub inline fn inBounds(self: *Self, index: usize) T {
            assert(index < self.count());
            return self.array.items[index];
        }

        /// This list will take ownership of `t`.
        pub inline fn append(self: *Self, t: T) OwnedListError!void {
            self.array.append(common.allocator, t) catch {
                return OwnedListError.out_of_memory;
            };
        }

        pub inline fn insert(self: *Self, at_index: usize, item: T) OwnedListError!void {
            assert(at_index <= self.count());
            self.array.insert(common.allocator, at_index, item) catch {
                return OwnedListError.out_of_memory;
            };
        }

        // TODO: add an `expectEquals(slice: []T) !void` method
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
