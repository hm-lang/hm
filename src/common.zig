const std = @import("std");
const builtin = @import("builtin");

pub const debug = builtin.mode == .Debug;
pub const testing = builtin.is_test;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub const allocator: std.mem.Allocator = if (testing)
    std.testing.allocator
else
    gpa.allocator();

pub fn swap(a: anytype, b: anytype) void {
    const c = a.*;
    a.* = b.*;
    b.* = c;
}

pub fn Range(comptime T: type) type {
    return struct {
        const Self = @This();

        /// `Range` starts at `start`.
        start: T,
        /// `Range` excludes `end`.
        end: T,

        pub fn of(the_start: anytype, the_end: anytype) Self {
            return .{ .start = @intCast(the_start), .end = @intCast(the_end) };
        }
    };
}
