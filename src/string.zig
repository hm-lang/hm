const Allocator = std.mem.Allocator;
const std = @import("std");
const testing = std.testing;

const StringError = error{
    StringTooLong,
};

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

// TODO: something like this would be cool
// const Pool = struct {
//     index_map: std.StringHashMap(usize),
//     array: std.ArrayList(u8),
// };

pub const Small = extern struct {
    size: u16 = 0,
    short: [6]u8 = undefined,
    remaining: extern union {
        if_small: [8]u8,
        pointer: *u8,
    } = undefined,

    fn get_medium_size() comptime_int {
        const small: Small = .{};
        const smallest_size = @sizeOf(@TypeOf(small.short));
        return smallest_size + @sizeOf(@TypeOf(small.remaining.if_small));
    }

    pub fn deinit(self: *Small) void {
        if (self.size <= get_medium_size()) {
            return;
        }
        allocator.free(self.buffer());
    }

    pub fn init(chars: []const u8) StringError!Small {
        _ = allocator;
        const u16_max = std.math.maxInt(u16);
        if (chars.len > u16_max) {
            return StringError.StringTooLong;
        }
        var string: Small = .{ .size = @intCast(chars.len) };
        const medium_size = comptime get_medium_size();
        if (chars.len > medium_size) {
            const heap = allocator.alloc(u8, chars.len) catch {
                // OutOfMemory
                std.debug.print("\ncouldn't allocate {d}-character string...\n", .{chars.len});
                return StringError.StringTooLong;
            };
            string.remaining.pointer = @ptrCast(heap.ptr);
            // TODO: write a short description (e.g., a11y) into `short`
        }
        @memcpy(string.buffer()[0..chars.len], chars);
        return string;
    }

    pub fn at(self: *const Small, index: anytype) u8 {
        if (index < 0) {
            index += self.size;
            if (index < 0) {
                return 0;
            }
        } else if (index >= self.size) {
            return 0;
        }
        return self.slice()[index];
    }

    /// Not public, should only be used when writing the first time (or deallocating).
    fn buffer(self: *Small) []u8 {
        const medium_size = comptime get_medium_size();
        if (self.size <= medium_size) {
            const full_small_buffer: *[medium_size]u8 = @ptrCast(&self.short[0]);
            return full_small_buffer[0..medium_size];
        } else {
            const full_buffer: *[std.math.maxInt(u16)]u8 = @ptrCast(self.remaining.pointer);
            return full_buffer[0..self.size];
        }
    }

    pub fn slice(self: *const Small) []const u8 {
        const medium_size = comptime get_medium_size();

        if (self.size <= medium_size) {
            const full_small_buffer: *const [medium_size]u8 = @ptrCast(&self.short[0]);
            return full_small_buffer[0..self.size];
        } else {
            const full_buffer: *const [std.math.maxInt(u16)]u8 = @ptrCast(self.remaining.pointer);
            return full_buffer[0..self.size];
        }
    }

    pub fn equals(self: *const Small, other: *const Small) bool {
        if (self.size != other.size) {
            return false;
        }
        return std.mem.eql(u8, self.slice(), other.slice());
    }
};

test "Small size is correct" {
    try testing.expectEqual(16, @sizeOf(Small));
    const small_string: Small = .{};
    try testing.expectEqual(2, @sizeOf(@TypeOf(small_string.size)));
    try testing.expectEqual(6, @sizeOf(@TypeOf(small_string.short)));
    try testing.expectEqual(8, @sizeOf(@TypeOf(small_string.remaining.if_small)));
    try testing.expectEqual(14, Small.get_medium_size());
    // TODO: check for `small_string.remaining.pointer` being at +8 from start of small_string
    // try testing.expectEqual(8, @typeInfo(@TypeOf(small_string.remaining.pointer)).Pointer.alignment);
}

test "equals works for small strings" {
    var empty_string: Small = .{};
    try testing.expectEqual(true, empty_string.equals(&try Small.init("")));

    var string1 = try Small.init("hi");
    defer string1.deinit();
    var string2 = try Small.init("hi");
    defer string2.deinit();
    try testing.expectEqual(true, string1.equals(&string2));
    try testing.expectEqualStrings(string1.slice(), "hi");

    var string3 = try Small.init("hI");
    defer string3.deinit();
    try testing.expectEqual(false, string1.equals(&string3));

    var string4 = try Small.init("hi this is going to be more than 16 characters");
    defer string4.deinit();
    try testing.expectEqual(false, string1.equals(&string4));
}

test "equals works for large strings" {
    var string1 = try Small.init("hello world this is over 16 characters");
    defer string1.deinit();
    var string2 = try Small.init("hello world this is over 16 characters");
    defer string2.deinit();
    try testing.expectEqual(true, string1.equals(&string2));
    try testing.expectEqualStrings(string1.slice(), "hello world this is over 16 characters");

    var string3 = try Small.init("hello world THIS is over 16 characters");
    defer string3.deinit();
    try testing.expectEqual(false, string1.equals(&string3));

    var string4 = try Small.init("hello");
    defer string4.deinit();
    try testing.expectEqual(false, string1.equals(&string4));
}
