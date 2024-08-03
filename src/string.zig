const Allocator = std.mem.Allocator;
const std = @import("std");
const testing = std.testing;

const StringError = error{
    StringTooLong,
};

pub const Small = extern struct {
    size: u16 = 0,
    small_start: [6]u8 = undefined,
    remaining: extern union {
        if_small: [8]u8,
        pointer: *u8,
    } = undefined,

    pub fn deinit(self: *Small) void {
        _ = self;
        // TODO
    }

    pub fn init(chars: []const u8, allocator: *Allocator) StringError!Small {
        _ = allocator;
        const u16_max = std.math.maxInt(u16);
        if (chars.len > u16_max) {
            return StringError.StringTooLong;
        }
        var string: Small = .{.size = @intCast(chars.len)};
        const smallest_size = comptime @sizeOf(@TypeOf(string.small_start));
        const medium_size = comptime smallest_size + @sizeOf(@TypeOf(string.remaining.if_small));
        if (chars.len > medium_size) {
            // TODO: try to allocate first
            return StringError.StringTooLong;
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

    pub fn buffer(self: *Small) []u8 {
        const smallest_size = comptime @sizeOf(@TypeOf(self.small_start));
        const medium_size = comptime smallest_size + @sizeOf(@TypeOf(self.remaining.if_small));

        if (self.size <= medium_size) {
            const full_small_buffer: *[medium_size]u8 = @ptrCast(&self.small_start[0]);
            return full_small_buffer[0..medium_size];
        }
        // TODO
        return &.{};
    }

    pub fn slice(self: *const Small) []const u8 {
        const smallest_size = comptime @sizeOf(@TypeOf(self.small_start));
        const medium_size = comptime smallest_size + @sizeOf(@TypeOf(self.remaining.if_small));

        if (self.size <= medium_size) {
            const full_small_buffer: *const [medium_size]u8 = @ptrCast(&self.small_start[0]);
            return full_small_buffer[0..self.size];
        }
        // TODO
        return &.{};
    }

    pub fn equals(self: *const Small, other: *const Small) bool {
        if (self.size != other.size) {
            return false;
        }
        const self_slice = self.slice();
        const other_slice = other.slice();
        for (0..self.size) |i| {
            if (self_slice[i] != other_slice[i]) {
                return false;
            }
        }
        return true;
    }
};

test "Small size is correct" {
    try testing.expectEqual(@sizeOf(Small), 16);
    const small_string: Small = .{};
    try testing.expectEqual(@sizeOf(@TypeOf(small_string.size)), 2);
    try testing.expectEqual(@sizeOf(@TypeOf(small_string.small_start)), 6);
    try testing.expectEqual(@sizeOf(@TypeOf(small_string.remaining.if_small)), 8);
    // TODO: check for `small_string.remaining.pointer` being at +8 from start of small_string
    // try testing.expectEqual(@typeInfo(@TypeOf(small_string.remaining.pointer)).Pointer.alignment, 8);
}

test "equals works" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var empty_string: Small = .{};
    try testing.expectEqual(true, empty_string.equals(&try Small.init("", &allocator)));

    var string1 = try Small.init("hi", &allocator);
    defer string1.deinit();
    var string2 = try Small.init("hi", &allocator);
    defer string2.deinit();
    try testing.expectEqual(true, string1.equals(&string2));
    try testing.expectEqualStrings(string1.slice(), "hi");

    var string3 = try Small.init("hI", &allocator);
    defer string3.deinit();
    try testing.expectEqual(false, string1.equals(&string3));
}
