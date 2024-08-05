const common = @import("common.zig");

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

pub fn OwnedList(comptime T: type) type {
    return struct {
        const Self = @This();

        // TODO: switch to ArrayListAlignedUnmanaged since we have `common.allocator` to pull from.
        array: std.ArrayList(T) = std.ArrayList(T).init(common.allocator),

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
            self.array.deinit();
        }

        pub inline fn count(self: *const Self) usize {
            return self.array.items.len;
        }

        /// Returns a "reference" -- don't free it.
        pub inline fn at(self: *Self, index: anytype) ?T {
            if (index < 0) {
                index += self.count();
                if (index < 0) {
                    return null;
                }
            } else if (index >= self.count()) {
                return null;
            }
            return self.array.items[index];
        }

        /// Returns a "reference" -- don't free it.
        pub inline fn inBounds(self: *Self, index: usize) T {
            assert(index < self.count());
            return self.array.items[index];
        }

        /// This list will take ownership of `t`.
        pub inline fn append(self: *Self, t: T) Allocator.Error!void {
            try self.array.append(t);
        }
    };
}
