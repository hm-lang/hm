const common = @import("common.zig");
const owned_list = @import("owned_list.zig");
const SmallString = @import("string.zig").Small;
const File = @import("file.zig").File;

const OwnedSmalls = owned_list.OwnedList(SmallString);
const OwnedTokens = owned_list.OwnedList(Token);

const std = @import("std");

const TokenError = error{
    out_of_memory,
    out_of_tokens,
    invalid_token,
};

pub const Tokenizer = struct {
    tokens: OwnedTokens = OwnedTokens.init(),
    file: File = .{},
    farthest_line_index: usize = 0,
    farthest_char_index: usize = 0,

    pub fn deinit(self: *Tokenizer) void {
        self.tokens.deinit();
        self.file.deinit();
    }

    /// Do not deinitialize the returned `Token`, it's owned by `Tokenizer`.
    pub fn at(self: *Tokenizer, token_index: usize) TokenError!Token {
        var count: usize = 0;
        while (token_index >= self.tokens.count()) {
            // We'll just keep appending `.end` to `tokens` if you keep
            // incrementing the index you pass in to `at()`, so don't do that.
            const token = try self.get_next_token();
            self.tokens.append(token) catch {
                return TokenError.out_of_memory;
            };
            count += 1;
        }
        if (count > 1) {
            std.debug.print("expected 0 or 1 token increments, not {d}\n", .{count});
        }
        return self.tokens.inBounds(token_index);
    }

    fn complete(self: *Tokenizer) TokenError!void {
        var last = try self.at(0);
        while (!last.equals(.end)) {
            last = try self.get_next_token();
            self.tokens.append(last) catch {
                return TokenError.out_of_memory;
            };
        }
    }

    fn get_next_token(self: *Tokenizer) TokenError!Token {
        if (self.farthest_line_index >= self.file.lines.count()) {
            return .end;
        }
        const line = self.file.lines.inBounds(self.farthest_line_index);
        while (true) {
            if (self.farthest_char_index >= line.count()) {
                self.farthest_char_index = 0;
                self.farthest_line_index += 1;
                return Token{ .newline = self.farthest_line_index };
            }
            // TODO: add a `tab` character, up to two per line.
            //      one if we go immediately to an identifier, two if we start with a symbol/operator
            //      (one for before and one for after).  alternatively retroactively change `tab`
            //      if we see an operator.  maybe have a `tab: [before: u8, after: u8]`.
            //      mash whitespace into `tab`.
            switch (line.inBounds(self.farthest_char_index)) {
                ' ' => {
                    self.farthest_char_index += 1;
                },
                'A'...'Z', '_' => return Token{ .starts_upper = try self.get_next_identifier(line) },
                'a'...'z' => return Token{ .starts_lower = try self.get_next_identifier(line) },
                else => return TokenError.invalid_token,
            }
        }
    }

    fn get_next_identifier(self: *Tokenizer, line: SmallString) TokenError!SmallString {
        // We've already checked and there's an alphabetical character at the self.farthest_char_index.
        const initial_char_index = self.farthest_char_index;
        self.farthest_char_index += 1;
        while (self.farthest_char_index < line.count()) {
            switch (line.inBounds(self.farthest_char_index)) {
                'A'...'Z', 'a'...'z', '0'...'9', '_' => {
                    self.farthest_char_index += 1;
                },
                else => break,
            }
        }
        return SmallString.init(line.slice()[initial_char_index..self.farthest_char_index]) catch {
            return TokenError.out_of_memory;
        };
    }
};

pub const TokenTag = enum {
    end,
    starts_upper,
    starts_lower,
    newline,
    // TODO: tab/indent
};

pub const Token = union(TokenTag) {
    end: void,
    starts_upper: SmallString,
    starts_lower: SmallString,
    newline: usize,

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
    const token = try tokenizer.at(0);
    try token.expectEquals(.end);
}

test "tokenizer deiniting frees internal memory" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    // Add some tokens (and lines) to ensure that we are de-initing the lines.
    try tokenizer.tokens.append(Token{ .starts_upper = try SmallString.init("Big" ** 20) });
    try tokenizer.tokens.append(Token{ .starts_lower = try SmallString.init("trees" ** 25) });
    try tokenizer.tokens.append(Token{ .starts_upper = try SmallString.init("Wigs" ** 30) });

    try tokenizer.file.lines.append(try SmallString.init("long line of stuff" ** 5));
    try tokenizer.file.lines.append(try SmallString.init("other line of stuff" ** 6));
    try tokenizer.file.lines.append(try SmallString.init("big line again" ** 7));
}

test "tokenizer skips whitespace" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    try tokenizer.file.lines.append(try SmallString.init("  Hello w_o_rld"));
    try tokenizer.file.lines.append(try SmallString.init("Second2    l1ne"));
    try tokenizer.file.lines.append(try SmallString.init("sp3cial Fin_ancial     _problems"));

    var token = try tokenizer.at(0);
    try token.expectEquals(Token{ .starts_upper = SmallString.noAlloc("Hello") });

    token = try tokenizer.at(1);
    try token.expectEquals(Token{ .starts_lower = SmallString.noAlloc("w_o_rld") });

    token = try tokenizer.at(2);
    try token.expectEquals(Token{ .newline = 1 });

    token = try tokenizer.at(3);
    try token.expectEquals(Token{ .starts_upper = SmallString.noAlloc("Second2") });

    token = try tokenizer.at(4);
    try token.expectEquals(Token{ .starts_lower = SmallString.noAlloc("l1ne") });

    token = try tokenizer.at(5);
    try token.expectEquals(Token{ .newline = 2 });

    token = try tokenizer.at(6);
    try token.expectEquals(Token{ .starts_lower = SmallString.noAlloc("sp3cial") });

    token = try tokenizer.at(7);
    try token.expectEquals(Token{ .starts_upper = SmallString.noAlloc("Fin_ancial") });

    token = try tokenizer.at(8);
    try token.expectEquals(Token{ .starts_upper = SmallString.noAlloc("_problems") });

    token = try tokenizer.at(9);
    try token.expectEquals(Token{ .newline = 3 });

    token = try tokenizer.at(10);
    try token.expectEquals(.end);
}

test "token equality" {
    const end: Token = .end;
    try std.testing.expect(end.equals(.end));
    try end.expectEquals(.end);

    const starts_upper = Token{ .starts_upper = try SmallString.init("Cabbage") };
    try starts_upper.expectNotEquals(end);
    try std.testing.expect(!starts_upper.equals(.end));
    try starts_upper.expectEquals(starts_upper);
    try std.testing.expect(starts_upper.equals(starts_upper));
    try starts_upper.expectNotEquals(Token{ .starts_upper = try SmallString.init("Apples") });
    try std.testing.expect(!starts_upper.equals(Token{ .starts_upper = try SmallString.init("Apples") }));

    const starts_lower = Token{ .starts_lower = try SmallString.init("Cabbage") };
    try starts_lower.expectNotEquals(end);
    try std.testing.expect(!starts_lower.equals(end));
    try starts_lower.expectNotEquals(starts_upper);
    try std.testing.expect(!starts_lower.equals(starts_upper));
    try starts_lower.expectEquals(starts_lower);
    try std.testing.expect(starts_lower.equals(starts_lower));
    try starts_lower.expectNotEquals(Token{ .starts_lower = try SmallString.init("Apples") });
    try std.testing.expect(!starts_lower.equals(Token{ .starts_lower = try SmallString.init("Apples") }));
}
