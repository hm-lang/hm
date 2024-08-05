const common = @import("common.zig");

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const OwnedListError = error{
    OutOfBounds,
};

pub fn OwnedList(comptime T: type) type {
    return struct {
        const Self = @This();

        array: std.ArrayListUnmanaged(T) = std.ArrayListUnmanaged(T){},

        pub fn init() Self {
            return .{};
        }

        pub fn deinit(self: *Self) void {
            // TODO: for some reason, this doesn't work (we get a `const` cast problem;
            //      `t` appears to be a `*const T` instead of a `*T`).
            //while (self.array.popOrNull()) |*t| {
            //    t.deinit();
            //}
            while (true) {
                var t: T = self.array.popOrNull() orelse break;
                t.deinit();
            }
            self.array.deinit(common.allocator);
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
                    return OwnedListError.OutOfBounds;
                }
            } else if (index >= count_i64) {
                return OwnedListError.OutOfBounds;
            }
            return self.array.items[@intCast(index)];
        }

        /// Returns a "reference" -- don't free it.
        pub inline fn inBounds(self: *Self, index: usize) T {
            assert(index < self.count());
            return self.array.items[index];
        }

        /// This list will take ownership of `t`.
        pub inline fn append(self: *Self, t: T) Allocator.Error!void {
            try self.array.append(common.allocator, t);
        }
    };
}

// TODO: tests
