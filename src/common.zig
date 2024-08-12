const testing = @import("testing.zig");

const std = @import("std");
const builtin = @import("builtin");

pub const debug = builtin.mode == .Debug;
pub const in_test = builtin.is_test;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub const allocator: std.mem.Allocator = if (in_test)
    std.testing.allocator
else
    gpa.allocator();

pub const At = enum {
    start,
    end,
};

pub const Error = error{
    unknown,
};

// TODO: ideally we wouldn't create these in non-test environments.
//       probably the best we can do is minimize the buffers internally.
var stdout_data = testing.TestWriterData{};
var stderr_data = testing.TestWriterData{};

pub var stdout = if (in_test)
    testing.TestWriter.init(&stdout_data)
else
    std.io.getStdOut().writer();

pub var stderr = if (in_test)
    testing.TestWriter.init(&stderr_data)
else
    std.io.getStdErr().writer();

pub fn swap(a: anytype, b: anytype) void {
    const c = a.*;
    a.* = b.*;
    b.* = c;
}

pub fn Found(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Optional => |optional| optional.child,
        .ErrorUnion => |error_union| error_union.payload,
        else => @compileError("should use an `Optional` or `ErrorUnion` type inside `Found`"),
    };
}

pub inline fn assert(a: anytype) Found(@TypeOf(a)) {
    return switch (@typeInfo(@TypeOf(a))) {
        .Optional => if (a) |not_null| not_null else @panic("expected `assert` argument to be non-null"),
        .ErrorUnion => a catch @panic("expected `assert` argument to not be an error"),
        else => @compileError("should use an `Optional` or `ErrorUnion` type inside `assert`"),
    };
}

pub inline fn when(a: anytype, comptime predicate: fn (Found(@TypeOf(a))) bool) bool {
    return switch (@typeInfo(@TypeOf(a))) {
        .Optional => if (a) |not_null| predicate(not_null) else false,
        .ErrorUnion => {
            const not_error = a catch return false;
            return predicate(not_error);
        },
        else => @compileError("should use an `Optional` or `ErrorUnion` type inside `when`"),
    };
}

pub fn printSlice(slice: anytype, writer: anytype) !void {
    try writer.print("[", .{});
    for (slice) |item| {
        if (std.meta.hasMethod(@TypeOf(item), "print")) {
            try item.print(writer);
            try writer.print(", ", .{});
        } else {
            try writer.print("{}, ", .{item});
        }
    }
    try writer.print("]", .{});
}

pub fn printSliceLine(slice: anytype, writer: anytype) !void {
    try printSlice(slice, writer);
    try writer.print("\n", .{});
}

pub inline fn before(a: anytype) ?@TypeOf(a) {
    return back(a, 1);
}

// TODO: consider getting rid of these and using `count() -> i64` so that we
// can easily do `@max(0, count() - amount)`
pub inline fn back(start: anytype, amount: anytype) ?@TypeOf(start) {
    if (start >= amount) {
        return start - amount;
    }
    return null;
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

        pub inline fn count(self: Self) T {
            return self.end - self.start;
        }

        pub fn equals(self: Self, other: Self) bool {
            return self.start == other.start and self.end == other.end;
        }

        pub fn expectEquals(self: Self, other: Self) !void {
            try std.testing.expectEqual(other.start, self.start);
            try std.testing.expectEqual(other.end, self.end);
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

test "assert works with nullables" {
    var my_range: ?Range(i32) = null;
    my_range = .{ .start = 123, .end = 456 };
    try std.testing.expectEqual(123, assert(my_range).start);
}

test "assert works with error unions" {
    const Err = error{err};
    var my_range: Err!Range(i32) = Err.err;
    my_range = .{ .start = 123, .end = 456 };
    try std.testing.expectEqual(456, assert(my_range).end);
}

test "when works with nullables" {
    const Test = struct {
        fn smallRange(range: Range(i32)) bool {
            return range.end - range.start < 10;
        }
        fn bigRange(range: Range(i32)) bool {
            return range.end - range.start >= 10;
        }
        fn alwaysTrue(range: Range(i32)) bool {
            _ = range;
            return true;
        }
    };
    var my_range: ?Range(i32) = null;
    // with null:
    try std.testing.expectEqual(false, when(my_range, Test.alwaysTrue));

    // when not null...
    my_range = .{ .start = 123, .end = 456 };
    // ... but predicate is false:
    try std.testing.expectEqual(false, when(my_range, Test.smallRange));
    // ... and predicate is true:
    try std.testing.expectEqual(true, when(my_range, Test.bigRange));
}

test "when works with error unions" {
    const Test = struct {
        fn smallRange(range: Range(i32)) bool {
            return range.end - range.start < 10;
        }
        fn bigRange(range: Range(i32)) bool {
            return range.end - range.start >= 10;
        }
        fn alwaysTrue(range: Range(i32)) bool {
            _ = range;
            return true;
        }
    };
    const Err = error{err};
    var my_range: Err!Range(i32) = Err.err;
    // with an error:
    try std.testing.expectEqual(false, when(my_range, Test.alwaysTrue));

    // when not an error...
    my_range = .{ .start = 123, .end = 456 };
    // ... but predicate is false:
    try std.testing.expectEqual(false, when(my_range, Test.smallRange));
    // ... and predicate is true:
    try std.testing.expectEqual(true, when(my_range, Test.bigRange));
}
