const string = @import("string.zig");

const std = @import("std");
const testing = std.testing;

pub const Tokenizer = struct {
    pub fn next(self: *Tokenizer) Token {
        _ = self;
        return .end;
    }

    pub fn deinit(self: *Tokenizer) void {
        _ = self;
        //self.tokens.deinit();
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

        const info = @typeInfo(Token);
        inline for (info.fields) |field_info| {
            if (@field(TokenTag, field_info.name) == tag_a) {
                const Field = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(Field, "equals")) {
                    return @field(a, field_info.name).equals(&@field(b, field_info.name));
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
};

test "basic tokenizer functionality" {
    var tokenizer: Tokenizer = .{};
    try tokenizer.next().expectEquals(.end);
}

test "token equality" {
    var tokenizer: Tokenizer = .{};
    try tokenizer.next().expectEquals(.end);
}
