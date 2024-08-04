const common = @import("common.zig");

const string = @import("string.zig");

const std = @import("std");

const TokenError = error{
    OutOfMemory,
    InvalidToken,
};

pub const Tokenizer = struct {
    token_array: std.ArrayList(Token) = std.ArrayList(Token).init(common.allocator),
    next_token_index: usize = 0,
    line_array: std.ArrayList(string.Small) = std.ArrayList(string.Small).init(common.allocator),

    pub fn deinit(self: *Tokenizer) void {
        // TODO: for some reason, this doesn't work (we get a `const` cast problem;
        //      `token` appears to be a `*const Token` instead of a `*Token`).
        //while (self.token_array.popOrNull()) |*token| {
        //    token.deinit();
        //}
        while (true) {
            var token: Token = self.token_array.popOrNull() orelse break;
            token.deinit();
        }
        self.token_array.deinit();
        // TODO: this should work!
        //while (self.line_array.popOrNull()) |*line| {
        //    line.deinit();
        //}
        while (true) {
            var line = self.line_array.popOrNull() orelse break;
            line.deinit();
        }
        self.line_array.deinit();
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
        self.token_array.append(.end) catch {
            return TokenError.OutOfMemory;
        };
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

    pub fn deinit(self: Token) void {
        const tag = std.meta.activeTag(self);
        const info = switch (@typeInfo(Token)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(TokenTag, field_info.name) == tag) {
                const SubToken = @TypeOf(@field(self, field_info.name));
                if (std.meta.hasMethod(SubToken, "deinit")) {
                    // For some reason, we can't do `@field(self, field_info.name).deinit()`
                    // since that assumes the field will be `const`.
                    var sub_token = @field(self, field_info.name);
                    sub_token.deinit();
                }
            }
        }
    }

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
                const SubToken = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(SubToken, "equals")) {
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
        try std.testing.expectEqual(tag_a, tag_b);

        const info = switch (@typeInfo(Token)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(TokenTag, field_info.name) == tag_a) {
                const SubToken = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(SubToken, "expectEquals")) {
                    try @field(a, field_info.name).expectEquals(@field(b, field_info.name));
                    return;
                } else {
                    try std.testing.expectEqual(@field(a, field_info.name), @field(b, field_info.name));
                    return;
                }
            }
        }
        try std.testing.expect(false);
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
                const SubToken = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(SubToken, "expectNotEquals")) {
                    try @field(a, field_info.name).expectNotEquals(@field(b, field_info.name));
                    return;
                } else {
                    try std.testing.expect(@field(a, field_info.name) != @field(b, field_info.name));
                    return;
                }
            }
        }
        try std.testing.expect(false);
    }
};

test "basic tokenizer functionality" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();
    const token = try tokenizer.next();
    try token.expectEquals(.end);
}

test "tokenizer deiniting frees internal memory" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    // Add some tokens (and lines) to ensure that we are de-initing the lines.
    try tokenizer.token_array.append(Token{ .starts_upper = try string.Small.init("Big" ** 20) });
    try tokenizer.token_array.append(Token{ .starts_lower = try string.Small.init("trees" ** 25) });
    try tokenizer.token_array.append(Token{ .starts_upper = try string.Small.init("Wigs" ** 30) });

    try tokenizer.line_array.append(try string.Small.init("long line of stuff" ** 5));
    try tokenizer.line_array.append(try string.Small.init("other line of stuff" ** 6));
    try tokenizer.line_array.append(try string.Small.init("big line again" ** 7));
}

test "token equality" {
    const end: Token = .end;
    try std.testing.expect(end.equals(.end));
    try end.expectEquals(.end);

    const starts_upper = Token{ .starts_upper = try string.Small.init("Cabbage") };
    try starts_upper.expectNotEquals(end);
    try std.testing.expect(!starts_upper.equals(.end));
    try starts_upper.expectEquals(starts_upper);
    try std.testing.expect(starts_upper.equals(starts_upper));
    try starts_upper.expectNotEquals(Token{ .starts_upper = try string.Small.init("Apples") });
    try std.testing.expect(!starts_upper.equals(Token{ .starts_upper = try string.Small.init("Apples") }));

    const starts_lower = Token{ .starts_lower = try string.Small.init("Cabbage") };
    try starts_lower.expectNotEquals(end);
    try std.testing.expect(!starts_lower.equals(end));
    try starts_lower.expectNotEquals(starts_upper);
    try std.testing.expect(!starts_lower.equals(starts_upper));
    try starts_lower.expectEquals(starts_lower);
    try std.testing.expect(starts_lower.equals(starts_lower));
    try starts_lower.expectNotEquals(Token{ .starts_lower = try string.Small.init("Apples") });
    try std.testing.expect(!starts_lower.equals(Token{ .starts_lower = try string.Small.init("Apples") }));
}
