const std = @import("std");
const builtin = @import("builtin");

pub const debug = builtin.mode == .Debug;
pub const testing = builtin.is_test;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub const allocator: std.mem.Allocator = if (testing)
    std.testing.allocator
else
    gpa.allocator();

pub const stdout = std.io.getStdOut().writer();
pub const stderr = std.io.getStdErr().writer();
// TODO: mock stdout/stderr for tests to ensure we're writing the right things.

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

/// Does a short version of `chars` for `buffer` in case `buffer` is smaller than `chars`.
/// Shortens e.g., 'my_string' to 'my_9ng' if `buffer` is 6 letters long, where 9
/// is the full length of the `chars` slice.
pub fn sign(buffer: []u8, chars: []const u8) !void {
    if (buffer.len == 0) {
        return;
    }
    if (buffer.len >= chars.len) {
        @memcpy(buffer[0..chars.len], chars);
        @memset(buffer[chars.len..], 0);
        return;
    }
    buffer[0] = chars[0];
    // We don't know how many digits `chars.len` is until we write it out.
    // (Although we could do a base-10 log.)
    const written_slice = try std.fmt.bufPrint(buffer[1..], "{d}", .{chars.len});
    // Where we want the number to show up.
    // Note that `written_slice.len` is always `< buffer.len`,
    // so this number is always >= 1.
    const desired_number_starting_index = (buffer.len + 1 - written_slice.len) / 2;
    const tail_letters_starting_index = desired_number_starting_index + written_slice.len;
    if (desired_number_starting_index > 1) {
        std.mem.copyBackwards(u8, buffer[desired_number_starting_index..tail_letters_starting_index], written_slice);
        @memcpy(buffer[1..desired_number_starting_index], chars[1..desired_number_starting_index]);
    }
    if (tail_letters_starting_index < buffer.len) {
        const tail_letters_count = buffer.len - tail_letters_starting_index;
        @memcpy(buffer[tail_letters_starting_index..], chars[chars.len - tail_letters_count ..]);
    }
}

test "sign works well for even-sized buffers" {
    var buffer = [_]u8{0} ** 8;
    try sign(&buffer, "hello");
    try std.testing.expectEqualStrings("hello\x00\x00\x00", &buffer);

    try sign(&buffer, "hi");
    try std.testing.expectEqualStrings("hi\x00\x00\x00\x00\x00\x00", &buffer);

    try sign(&buffer, "underme");
    try std.testing.expectEqualStrings("underme\x00", &buffer);

    try sign(&buffer, "equal_it");
    try std.testing.expectEqualStrings("equal_it", &buffer);

    try sign(&buffer, "just_over");
    try std.testing.expectEqualStrings("just9ver", &buffer);

    try sign(&buffer, "something_bigger");
    try std.testing.expectEqualStrings("som16ger", &buffer);

    try sign(&buffer, "wowza" ** 100);
    try std.testing.expectEqualStrings("wow500za", &buffer);

    try sign(&buffer, "cake" ** 1000);
    try std.testing.expectEqualStrings("ca4000ke", &buffer);

    try sign(&buffer, "big" ** 10000);
    try std.testing.expectEqualStrings("bi30000g", &buffer);
}

test "sign works well for odd-sized buffers" {
    var buffer = [_]u8{0} ** 7;
    try sign(&buffer, "");
    try std.testing.expectEqualStrings("\x00\x00\x00\x00\x00\x00\x00", &buffer);

    try sign(&buffer, "a");
    try std.testing.expectEqualStrings("a\x00\x00\x00\x00\x00\x00", &buffer);

    try sign(&buffer, "under@");
    try std.testing.expectEqualStrings("under@\x00", &buffer);

    try sign(&buffer, "equalit");
    try std.testing.expectEqualStrings("equalit", &buffer);

    try sign(&buffer, "justover");
    try std.testing.expectEqualStrings("jus8ver", &buffer);

    try sign(&buffer, "something_bigger");
    try std.testing.expectEqualStrings("som16er", &buffer);

    try sign(&buffer, "wowza" ** 100);
    try std.testing.expectEqualStrings("wo500za", &buffer);

    try sign(&buffer, "cake" ** 1000);
    try std.testing.expectEqualStrings("ca4000e", &buffer);

    try sign(&buffer, "big" ** 10000);
    try std.testing.expectEqualStrings("b30000g", &buffer);
}

test "sign works well for small even-sized buffers" {
    var buffer = [_]u8{0} ** 6;
    try sign(&buffer, "wowza" ** 100);
    try std.testing.expectEqualStrings("wo500a", &buffer);

    try sign(&buffer, "cake" ** 1000);
    try std.testing.expectEqualStrings("c4000e", &buffer);

    try sign(&buffer, "big" ** 10000);
    try std.testing.expectEqualStrings("b30000", &buffer);
}
