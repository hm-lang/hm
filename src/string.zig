const std = @import("std");
const testing = std.testing;

const StringError = error{
    StringTooLong,
};

pub const Small = extern struct {
    size: u16 = 0,
    start: [6]u8 = undefined,
    remaining: extern union {
        if_small: [8]u8,
        pointer: *u8,
    } = undefined,

    pub fn init(chars: []const u8) StringError!Small {
        const u16_max: u16 = comptime -1;
        if (chars.len > u64(u16_max)) {
            return .StringTooLong;
        }
        const string = Small();
        const smallest_size = comptime @sizeOf(@TypeOf(string.start));
        const medium_size = comptime smallest_size + @sizeOf(@TypeOf(string.remaining.if_small));
        if (chars.len <= smallest_size) {
            std.mem.copy(u8, string.start, chars);
        } else if (chars.len <= medium_size) {
            std.mem.copy(u8, string.start, chars[0..6]);
            //std.mem.copy(u8, string.start, chars);
        } else {
            // TODO: allocate first
            return .StringTooLong;
        }
        string.size = chars.len;
        return string;
    }

    pub fn at(self: *Small, index: i64) u8 {
        _ = self;
        _ = index;
        // TODO
        return 0;
    }
};

test "Small size is correct" {
    try testing.expectEqual(@sizeOf(Small), 16);
    const small_string: Small = .{};
    try testing.expectEqual(@sizeOf(@TypeOf(small_string.size)), 2);
    try testing.expectEqual(@sizeOf(@TypeOf(small_string.start)), 6);
    try testing.expectEqual(@sizeOf(@TypeOf(small_string.remaining.if_small)), 8);
    // TODO: check for `small_string.remaining.pointer` being at +8 from start of small_string
    // try testing.expectEqual(@typeInfo(@TypeOf(small_string.remaining.pointer)).Pointer.alignment, 8);
}
