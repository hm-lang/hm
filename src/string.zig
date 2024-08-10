const common = @import("common.zig");

const std = @import("std");

const StringError = error{
    string_too_long,
    out_of_memory,
};

// TODO: something like this would be cool
// const Pool = struct {
//     index_map: std.StringHashMap(usize),
//     array: std.ArrayList(u8),
// };

pub const Small = extern struct {
    const Self = @This();

    pub const Error = StringError;
    pub const max_size: usize = std.math.maxInt(u16);

    /// Each `Small` will only ever get to 65535 as the max size, so
    /// 65534 will be the max (non-empty) `start` field and
    /// 65535 will be the max `end` field, so a `u16` type is ok here.
    pub const Range = common.Range(u16);
    pub inline fn range(start: anytype, end: anytype) Range {
        return .{ .start = @intCast(start), .end = @intCast(end) };
    }

    size: u16 = 0,
    short: [6]u8 = undefined,
    remaining: extern union {
        if_small: [8]u8,
        pointer: *u8,
    } = undefined,

    // TODO: rename to `get_no_alloc_size()`
    fn get_medium_size() comptime_int {
        const small: Small = .{};
        const smallest_size = @sizeOf(@TypeOf(small.short));
        return smallest_size + @sizeOf(@TypeOf(small.remaining.if_small));
    }

    /// Initializes a `Small` that is just on the stack (no allocations on the heap).
    /// For compile-time-known `chars` only.  For anything else, prefer `init` and
    /// just defer `deinit` to be safe.
    pub inline fn noAlloc(chars: anytype) Small {
        // We're expecting `chars` to be `*const [n:0]u8` with n <= get_medium_size()
        if (chars.len > comptime get_medium_size()) {
            @compileError(std.fmt.comptimePrint("Small.noAlloc must have {d} characters or less", .{get_medium_size()}));
        }
        return Small.init(chars) catch unreachable;
    }

    // Another initialization that doesn't require an allocation.
    pub fn init64(l64: u64) Small {
        var result: Small = .{ .size = 8 };
        var actual_size: u16 = 0;
        var remaining64 = l64;
        const write_buffer = result.buffer();
        while (remaining64 > 0) {
            write_buffer[actual_size] = @intCast(remaining64 & 255);
            remaining64 >>= 8;
            actual_size += 1;
        }
        result.size = actual_size;
        return result;
    }

    // TODO: we should see how flexible the allocation stuff is;
    // do we need the end of the range for the deallocation to work correctly?
    // if so, we could allow pop() from this string, etc.
    // Probably not something we want to support, though.
    /// Note this will not provide a signature.
    pub fn allocExactly(new_size: anytype) StringError!Small {
        if (new_size > max_size) {
            return StringError.string_too_long;
        }
        var string: Small = .{ .size = @intCast(new_size) };
        if (new_size > comptime get_medium_size()) {
            const heap = common.allocator.alloc(u8, new_size) catch {
                std.debug.print("couldn't allocate {d}-character string...\n", .{new_size});
                return StringError.out_of_memory;
            };
            string.remaining.pointer = @ptrCast(heap.ptr);
        }
        return string;
    }

    /// Signing is only useful for "large" strings.
    pub fn sign(self: *Small) void {
        if (self.size > comptime get_medium_size()) {
            const chars = self.slice();
            common.sign(&self.short, chars) catch {
                std.debug.print("shouldn't have had a problem signing {d} characters\n", .{chars.len});
            };
        }
    }

    pub fn deinit(self: *Small) void {
        if (self.size > get_medium_size()) {
            common.allocator.free(self.buffer());
        }
        self.size = 0;
    }

    pub inline fn init(chars: []const u8) StringError!Small {
        var string = try Small.allocExactly(chars.len);
        @memcpy(string.buffer(), chars);
        string.sign();
        return string;
    }

    pub inline fn as64(chars: anytype) u64 {
        // We're expecting `chars` to be `*const [n:0]u8` with n <= 8
        if (chars.len > 8) {
            @compileError("Small.as64 must have 8 characters or less");
        }

        return internalAs64(chars);
    }

    pub fn signature(self: *const Small) []const u8 {
        if (self.size <= comptime get_medium_size()) {
            return self.slice();
        }
        return &self.short;
    }

    pub fn little64(self: *const Small) StringError!u64 {
        const chars = self.slice();
        if (chars.len > 8) {
            return StringError.string_too_long;
        }
        return internalAs64(chars);
    }

    pub inline fn at(self: *const Small, at_index: anytype) u8 {
        var index: i64 = @intCast(at_index);
        if (index < 0) {
            index += self.size;
            if (index < 0) {
                return 0;
            }
        } else if (index >= self.size) {
            return 0;
        }
        return self.slice()[@intCast(index)];
    }

    pub inline fn inBounds(self: *const Small, index: usize) u8 {
        std.debug.assert(index < self.count());
        return self.slice()[index];
    }

    pub inline fn count(self: *const Small) u16 {
        return self.size;
    }

    /// Only use at start of string creation.  It won't be automatically signed;
    /// so you can't rely on that.
    pub fn buffer(self: *Small) []u8 {
        const medium_size = comptime get_medium_size();
        if (self.size <= medium_size) {
            const full_small_buffer: *[medium_size]u8 = @ptrCast(&self.short[0]);
            return full_small_buffer[0..self.size];
        } else {
            const full_buffer: *[Small.max_size]u8 = @ptrCast(self.remaining.pointer);
            return full_buffer[0..self.size];
        }
    }

    pub inline fn in(self: *const Small, in_range: Range) []const u8 {
        return self.slice()[@intCast(in_range.start)..@intCast(in_range.end)];
    }

    pub inline fn fullRange(self: *const Small) Range {
        return .{ .start = 0, .end = self.size };
    }

    pub fn slice(self: *const Small) []const u8 {
        const medium_size = comptime get_medium_size();

        if (self.size <= medium_size) {
            const full_small_buffer: *const [medium_size]u8 = @ptrCast(&self.short[0]);
            return full_small_buffer[0..self.size];
        } else {
            const full_buffer: *const [Small.max_size]u8 = @ptrCast(self.remaining.pointer);
            return full_buffer[0..self.size];
        }
    }

    pub inline fn printLine(self: *const Small, writer: anytype) !void {
        try writer.print("{s}\n", .{self.slice()});
    }

    pub inline fn print(self: *const Small, writer: anytype) !void {
        try writer.print("{s}", .{self.slice()});
    }

    pub fn equals(self: Small, other: Small) bool {
        if (self.size != other.size) {
            return false;
        }
        return std.mem.eql(u8, self.slice(), other.slice());
    }

    pub fn expectEquals(a: Small, b: Small) !void {
        const equal = a.equals(b);
        if (!equal) {
            std.debug.print("expected {s}, got {s}\n", .{ b.slice(), a.slice() });
        }
        try std.testing.expect(equal);
    }

    pub fn expectNotEquals(a: Small, b: Small) !void {
        const equal = a.equals(b);
        if (equal) {
            std.debug.print("expected {s} to NOT equal {s}\n", .{ a.slice(), b.slice() });
        }
        try std.testing.expect(!equal);
    }

    pub fn expectEqualsString(self: Small, string: []const u8) !void {
        try std.testing.expectEqualStrings(string, self.slice());
    }
};

fn internalAs64(chars: []const u8) u64 {
    std.debug.assert(chars.len <= 8);
    var result: u64 = 0;
    var index: u6 = 0;
    while (index < chars.len) {
        const char64: u64 = chars[index];
        result |= char64 << (index * 8);
        index += 1;
    }
    return result;
}

test "Small size is correct" {
    try std.testing.expectEqual(16, @sizeOf(Small));
    const small_string: Small = .{};
    try std.testing.expectEqual(2, @sizeOf(@TypeOf(small_string.size)));
    try std.testing.expectEqual(6, @sizeOf(@TypeOf(small_string.short)));
    try std.testing.expectEqual(8, @sizeOf(@TypeOf(small_string.remaining.if_small)));
    try std.testing.expectEqual(14, Small.get_medium_size());
    // TODO: check for `small_string.remaining.pointer` being at +8 from start of small_string
    // try std.testing.expectEqual(8, @typeInfo(@TypeOf(small_string.remaining.pointer)).Pointer.alignment);
}

test "noAlloc works" {
    try std.testing.expectEqualStrings("this is ok man", Small.noAlloc("this is ok man").slice());
}

test "init64 works" {
    var little = Small.init64('*');
    try std.testing.expectEqualStrings("*", little.slice());

    little = Small.init64('_');
    try std.testing.expectEqualStrings("_", little.slice());

    little = Small.init64(30033);
    try std.testing.expectEqualStrings("Qu", little.slice());

    little = Small.init64(2764688058392519008);
    try std.testing.expectEqualStrings("`!@#$%^&", little.slice());
}

test "little64 works for small strings" {
    var little = Small.noAlloc("*");
    try std.testing.expectEqual('*', try little.little64());
    try little.expectEquals(Small.init64('*'));

    little = Small.noAlloc("_");
    try std.testing.expectEqual('_', try little.little64());
    try little.expectEquals(Small.init64('_'));

    little = Small.noAlloc("Qu");
    try std.testing.expectEqual(30033, try little.little64());
    try little.expectEquals(Small.init64(30033));

    little = Small.noAlloc("`!@#$%^&");
    try std.testing.expectEqual(2764688058392519008, try little.little64());
    try little.expectEquals(Small.init64(2764688058392519008));
}

test "little64 throws for >8 character strings" {
    const not_little = Small.noAlloc("123456789");
    try std.testing.expectError(StringError.string_too_long, not_little.little64());
}

test "equals works for noAlloc strings" {
    const empty_string: Small = .{};
    try std.testing.expectEqual(true, empty_string.equals(Small.noAlloc("")));

    const string1 = Small.noAlloc("hi");
    const string2 = Small.noAlloc("hi");
    try std.testing.expectEqual(true, string1.equals(string2));
    try std.testing.expectEqualStrings("hi", string1.slice());
    try string1.expectEquals(string2);

    const string3 = Small.noAlloc("hI");
    try std.testing.expectEqual(false, string1.equals(string3));
    try string1.expectNotEquals(string3);

    var string4 = try Small.init("hi this is going to be more than 16 characters");
    defer string4.deinit();
    try std.testing.expectEqual(false, string1.equals(string4));
    try string1.expectNotEquals(string4);
}

test "equals works for large strings" {
    var string1 = try Small.init("hello world this is over 16 characters");
    defer string1.deinit();
    var string2 = try Small.init("hello world this is over 16 characters");
    defer string2.deinit();
    try std.testing.expectEqual(true, string1.equals(string2));
    try std.testing.expectEqualStrings("hello world this is over 16 characters", string1.slice());
    try string1.expectEquals(string2);

    var string3 = try Small.init("hello world THIS is over 16 characters");
    defer string3.deinit();
    try std.testing.expectEqual(false, string1.equals(string3));
    try string1.expectNotEquals(string3);

    const string4 = Small.noAlloc("hello");
    try std.testing.expectEqual(false, string1.equals(string4));
    try string1.expectNotEquals(string4);
}

test "at/inBounds works for large strings" {
    var string = try Small.init("Thumb thunk rink?");
    defer string.deinit();
    const count: i64 = @intCast(string.count());
    try std.testing.expectEqual('T', string.at(0));
    try std.testing.expectEqual('h', string.inBounds(1));
    try std.testing.expectEqual('u', string.at(2));
    try std.testing.expectEqual('m', string.inBounds(3));
    try std.testing.expectEqual('b', string.at(4));
    try std.testing.expectEqual(' ', string.inBounds(5));

    // Reverse indexing works as well for `at`
    try std.testing.expectEqual('T', string.at(-count));
    try std.testing.expectEqual('h', string.at(-count + 1));
    try std.testing.expectEqual('i', string.at(-4));
    try std.testing.expectEqual('n', string.at(-3));
    try std.testing.expectEqual('k', string.at(-2));
    try std.testing.expectEqual('?', string.at(-1));

    // OOBs works for `at`
    try std.testing.expectEqual(0, string.at(-count - 1));
    try std.testing.expectEqual(0, string.at(count));
}

test "at/inBounds works for small strings" {
    const string = Small.noAlloc("Hi there!!");
    const count: i64 = @intCast(string.count());
    try std.testing.expectEqual('H', string.at(0));
    try std.testing.expectEqual('i', string.inBounds(1));
    try std.testing.expectEqual(' ', string.at(2));
    try std.testing.expectEqual('t', string.inBounds(3));
    try std.testing.expectEqual('h', string.at(4));
    try std.testing.expectEqual('e', string.inBounds(5));

    // Reverse indexing works as well for `at`
    try std.testing.expectEqual('H', string.at(-count));
    try std.testing.expectEqual('i', string.at(-count + 1));
    try std.testing.expectEqual('r', string.at(-4));
    try std.testing.expectEqual('e', string.at(-3));
    try std.testing.expectEqual('!', string.at(-2));
    try std.testing.expectEqual('!', string.at(-1));

    // OOBs works for `at`
    try std.testing.expectEqual(0, string.at(-count - 1));
    try std.testing.expectEqual(0, string.at(count));
}

test "does not sign short strings" {
    var string = Small.noAlloc("below fourteen");
    try std.testing.expectEqualStrings("below fourteen", string.signature());
    try std.testing.expectEqual(14, string.count());
}

test "signs large strings" {
    var string = try Small.init("above sixteen chars");
    defer string.deinit();

    try std.testing.expectEqualStrings("ab19rs", string.signature());
    try std.testing.expectEqual(19, string.count());
}

test "signs very large strings" {
    // This is the largest string we can do:
    var string = try Small.init("g" ** Small.max_size);
    defer string.deinit();

    try std.testing.expectEqualStrings("g65535", string.signature());
    try std.testing.expectEqual(65535, string.count());
}

test "too large of a string" {
    try std.testing.expectError(StringError.string_too_long, Small.init("g" ** (Small.max_size + 1)));
}

test "copies all bytes of short string" {
    // This is mostly just me verifying how zig does memory.
    // We want string copies to be safe.
    var string_src = Small.noAlloc("0123456789abcd");
    string_src.size = 5;

    const string_dst = string_src;
    string_src.size = 14;

    for (string_src.buffer()) |*c| {
        c.* += 10;
    }
    try string_src.expectEquals(Small.noAlloc(":;<=>?@ABCklmn"));
    try string_dst.expectEquals(Small.noAlloc("01234"));
}
