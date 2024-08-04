const string = @import("string.zig");

const std = @import("std");
const testing = std.testing;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

const TokenError = error{
    OutOfMemory,
    InvalidToken,
};

pub const Tokenizer = struct {
    token_array: std.ArrayList(Token) = std.ArrayList(Token).init(allocator),
    next_token_index: usize = 0,

    pub fn deinit(self: *Tokenizer) void {
        self.token_array.deinit();
    }

    pub fn snapshot(self: *Tokenizer) usize {
        return self.next_token_index;
    }

    pub fn restore(self: *Tokenizer, snapshot_index: usize) void {
        self.next_token_index = snapshot_index;
    }

    /// Do not deinitialize the returned `Token`, it's owned by `Tokenizer`.
    pub fn peek(self: *Tokenizer) TokenError!Token {
        if (self.token_array.items.len <= self.next_token_index) {
            try self.read_next_token();
        }
        return self.token_array.items[self.next_token_index];
    }

    /// Do not deinitialize the returned `Token`, it's owned by `Tokenizer`.
    pub fn next(self: *Tokenizer) TokenError!Token {
        const result = try self.peek();
        if (!result.equals(.end)) {
            self.next_token_index += 1;
        }
        return result;
    }

    fn read_next_token(self: *Tokenizer) TokenError!void {
        self.token_array.append(.end) catch { return TokenError.OutOfMemory; };
        // TODO
    }
};

pub const TokenTag = enum {
    end,
    starts_upper,
    starts_lower,
};

pub const Token = union(TokenTag) {
    end: void,
    starts_upper: string.Small,
    starts_lower: string.Small,

    pub fn equals(a: Token, b: Token) bool {
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        if (tag_a != tag_b) return false;

        const info = switch (@typeInfo(Token)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(TokenTag, field_info.name) == tag_a) {
                const Field = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(Field, "equals")) {
                    return @field(a, field_info.name).equals(@field(b, field_info.name));
                } else {
                    return @field(a, field_info.name) == @field(b, field_info.name);
                }
            }
        }
        return false;
    }

    pub fn expectEquals(a: Token, b: Token) !void {
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        try testing.expectEqual(tag_a, tag_b);

        const info = switch (@typeInfo(Token)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(TokenTag, field_info.name) == tag_a) {
                const Field = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(Field, "expectEquals")) {
                    try @field(a, field_info.name).expectEquals(@field(b, field_info.name));
                    return;
                } else {
                    try testing.expectEqual(@field(a, field_info.name), @field(b, field_info.name));
                    return;
                }
            }
        }
        try testing.expect(false);
    }

    pub fn expectNotEquals(a: Token, b: Token) !void {
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        if (tag_a != tag_b) {
            return;
        }

        const info = switch (@typeInfo(Token)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(TokenTag, field_info.name) == tag_a) {
                const Field = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(Field, "expectNotEquals")) {
                    try @field(a, field_info.name).expectNotEquals(@field(b, field_info.name));
                    return;
                } else {
                    try testing.expect(@field(a, field_info.name) != @field(b, field_info.name));
                    return;
                }
            }
        }
        try testing.expect(false);
    }
};

test "basic tokenizer functionality" {
    var tokenizer: Tokenizer = .{};
    const token = try tokenizer.next();
    try token.expectEquals(.end);
}

test "token equality" {
    const end: Token = .end;
    try testing.expect(end.equals(.end));
    try end.expectEquals(.end);

    const starts_upper = Token{ .starts_upper = try string.Small.init("Cabbage") };
    try starts_upper.expectNotEquals(end);
    try testing.expect(!starts_upper.equals(.end));
    try starts_upper.expectEquals(starts_upper);
    try testing.expect(starts_upper.equals(starts_upper));
    try starts_upper.expectNotEquals(Token{ .starts_upper = try string.Small.init("Apples") });
    try testing.expect(!starts_upper.equals(Token{ .starts_upper = try string.Small.init("Apples") }));

    const starts_lower = Token{ .starts_lower = try string.Small.init("Cabbage") };
    try starts_lower.expectNotEquals(end);
    try testing.expect(!starts_lower.equals(end));
    try starts_lower.expectNotEquals(starts_upper);
    try testing.expect(!starts_lower.equals(starts_upper));
    try starts_lower.expectEquals(starts_lower);
    try testing.expect(starts_lower.equals(starts_lower));
    try starts_lower.expectNotEquals(Token{ .starts_lower = try string.Small.init("Apples") });
    try testing.expect(!starts_lower.equals(Token{ .starts_lower = try string.Small.init("Apples") }));
}
