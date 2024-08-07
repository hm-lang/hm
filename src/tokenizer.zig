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
    // TODO: add an `error_past_token_index: usize = std.math.max(usize)`
    //      which gets updated and throws `error_past_this` if you're trying to access past it.

    pub fn deinit(self: *Tokenizer) void {
        self.tokens.deinit();
        self.file.deinit();
    }

    /// Returns the line index for the given token index.
    /// Looks backwards to find the nearest `newline` token.
    /// You should already have looked up to the token index via `at()` before calling this,
    /// so this has undefined behavior if it can't allocate the necessary tokens up to
    /// the passed-in `for_token_index`.
    fn lineIndexAt(self: *Tokenizer, at_token_index: usize) usize {
        var token_index: i64 = @intCast(at_token_index);
        if (token_index >= self.tokens.count()) {
            std.debug.print("try to get rid of these calls\n", .{});
            return 0;
        }
        while (token_index >= 0) {
            const token = self.tokens.inBounds(@intCast(token_index));
            switch (token) {
                .newline => |line_index| {
                    return line_index;
                },
                else => token_index -= 1,
            }
        }
        return 0;
    }

    fn lastTokenIndex(self: *Tokenizer) TokenError!usize {
        const count = self.tokens.count();
        return if (count > 0) count - 1 else TokenError.out_of_tokens;
    }

    /// Do not deinitialize the returned `Token`, it's owned by `Tokenizer`.
    /// You're not allowed to negative-index this because you should only
    /// care about the next token (or a slice of them) and not the last token.
    /// Plus it's not obvious if that should be the current last token or
    /// the last token after we've completed adding all tokens.
    pub fn at(self: *Tokenizer, token_index: usize) TokenError!Token {
        var count: usize = 0;
        while (token_index >= self.tokens.count()) {
            // We'll just keep appending `.end` to `tokens` if you keep
            // incrementing the index you pass in to `at()`, so don't do that.
            _ = try self.addNextToken();
            count += 1;
        }
        if (count > 1) {
            common.stderr.print("expected 0 or 1 token increments, not {d}\n", .{count}) catch {};
        }
        return self.tokens.inBounds(token_index);
    }

    fn complete(self: *Tokenizer) TokenError!void {
        var last = try self.at(0);
        while (!last.equals(.end)) {
            last = try self.addNextToken();
        }
    }

    fn addNextToken(self: *Tokenizer) TokenError!Token {
        const starting_char_index = self.farthest_char_index;
        const original_count = self.tokens.count();
        const next = try self.getNextExplicitToken();
        if (next.isWhitespace() or (original_count > 0 and self.tokens.inBounds(original_count - 1).isTab())) {
            // No need to add an implied tab between existing whitespace...
        } else {
            // Add an "implied" tab so we can keep track of where we are.
            self.tokens.append(Token{ .tab = starting_char_index }) catch {
                return TokenError.out_of_memory;
            };
        }
        self.tokens.append(next) catch {
            return TokenError.out_of_memory;
        };
        return next;
    }

    fn getNextExplicitToken(self: *Tokenizer) TokenError!Token {
        if (self.farthest_line_index >= self.file.lines.count()) {
            return .end;
        }
        const line = self.file.lines.inBounds(self.farthest_line_index);
        if (self.farthest_char_index >= line.count()) {
            return self.getNewline();
        }
        const starting_char = line.inBounds(self.farthest_char_index);
        const starts_with_whitespace = starting_char == ' ';
        if (starts_with_whitespace) {
            while (true) {
                self.farthest_char_index += 1;
                if (self.farthest_char_index >= line.count()) {
                    // Ignore whitespace at the end of a line.
                    return self.getNewline();
                }
                if (line.inBounds(self.farthest_char_index) != ' ') {
                    return Token{ .tab = self.farthest_char_index };
                }
            }
        } else {
            switch (starting_char) {
                'A'...'Z', '_' => return Token{ .starts_upper = try self.getNextIdentifier(line) },
                // TODO: do we want to support `and` here?  we could just use `&&`
                //      so that `X and(Y)` would be ok to overload.
                //      mostly eventually we need `xor`.
                'a'...'z' => return Token{ .starts_lower = try self.getNextIdentifier(line) },
                // TODO: '@' => return Token { ???: self.getNextIdentifier(line) },
                // TODO: '"' and '\''
                // TODO: '[', '(', and '{', with corresponding ']', ')', and '}'.
                // TODO: '#'.
                else => return Token{ .operator = try self.getNextOperator(line) },
            }
        }
    }

    fn getNewline(self: *Tokenizer) Token {
        self.farthest_char_index = 0;
        self.farthest_line_index += 1;
        return Token{ .newline = self.farthest_line_index };
    }

    fn getNextIdentifier(self: *Tokenizer, line: SmallString) TokenError!SmallString {
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

    fn getNextOperator(self: *Tokenizer, line: SmallString) TokenError!u64 {
        const initial_char_index = self.farthest_char_index;
        errdefer {
            self.addErrorAt(
                self.lastTokenIndex() catch 0,
                SmallString.Range.of(initial_char_index, self.farthest_char_index),
                "invalid operator",
            );
        }
        self.farthest_char_index += 1;
        while (self.farthest_char_index < line.count()) {
            switch (line.inBounds(self.farthest_char_index)) {
                '?', '~', '!', '@', '$', '%', '^', '&', '*', '/', '+', '-', '=', '>', '<', ':', ';', '.' => {
                    self.farthest_char_index += 1;
                },
                else => break,
            }
        }
        const buffer = line.slice()[initial_char_index..self.farthest_char_index];
        if (buffer.len > 8) {
            return TokenError.invalid_token;
        }
        const small = SmallString.init(buffer) catch unreachable;
        const operator = small.little64() catch unreachable;
        switch (operator) {
            SmallString.as64("="),
            SmallString.as64("+"),
            SmallString.as64("-"),
            SmallString.as64("*"),
            SmallString.as64("/"),
            => {},
            else => {
                return TokenError.invalid_token;
            },
        }
        return operator;
    }

    // TODO: add some optional "extra lines" to add as well as the error message
    //      for extra debugging help.
    /// Adds an error around the given token index (i.e., on the line after that token).
    // TODO: we probably should be able to add `error_columns` based on `at_token_index`.
    //      but we probably need to add a `Token.invalid: SmallString` and add it, then look its columns up.
    pub fn addErrorAt(self: *Tokenizer, at_token_index: usize, error_columns: SmallString.Range, error_message: []const u8) void {
        const after_line_index = self.lineIndexAt(at_token_index);
        // TODO: these comment lines need to be skipped while parsing and removed from self.file
        //      i.e., any line starting with '#@!'
        var string = getErrorLine(error_columns, error_message) catch {
            self.printErrorMessage(after_line_index, error_columns, error_message);
            return;
        };
        self.file.lines.insert(after_line_index + 1, string) catch {
            self.file.lines.inBounds(after_line_index).printLine(common.stderr) catch {};
            string.printLine(common.stderr) catch {};
            string.deinit();
            return;
        };
        // This can't happen while tokenizing, but if we come back in parsing and add an error,
        // we'll want to ensure that our tokenizer stays where it expects.
        // TODO: add a test for this.
        // TODO: we probably need to increment every Token.newline *after* this point, because
        //      `lineIndexAt` will no longer work.  alternatively we say that we break invariants
        //      after `addErrorLine` and that users should no longer try to grab tokens.
        // TODO: add a `error_after` usize which comes down from usize.max if we `addErrorLineAt`.
        if (self.farthest_line_index > after_line_index) {
            self.farthest_line_index += 1;
        }
        // TODO: print `lines[after_line_index]` and `string` to common.stderr.
        //      get fancy with the colors around error_columns.
    }

    fn getErrorLine(error_columns: SmallString.Range, error_message: []const u8) SmallString.Error!SmallString {
        const compiler_error_start = "#@!";
        // Error message prefix if we can add the error message before `^~~~~~`:
        // +1 for the additional space between `error_message` and '^'.
        const pre_length = compiler_error_start.len + error_message.len + 1;
        var string: SmallString = undefined;
        if (error_columns.start >= pre_length) {
            // Error goes before "^~~~~", e.g.
            // `#@!     this is an error ^~~~~`
            const total_length: usize = @intCast(error_columns.end);
            string = try SmallString.allocExactly(total_length);
            var buffer = string.buffer();
            @memcpy(buffer[0..compiler_error_start.len], compiler_error_start);
            // -1 for the space between `error_message` and '^'
            const error_message_start = error_columns.start - error_message.len - 1;
            @memset(buffer[compiler_error_start.len..error_message_start], ' ');
            @memcpy(buffer[error_message_start .. error_columns.start - 1], error_message);
            buffer[error_columns.start - 1] = ' ';
            buffer[error_columns.start] = '^';
            @memset(buffer[error_columns.start + 1 .. error_columns.end], '~');
        } else {
            // Error goes after ^~~~~, e.g.,
            // `#@!  ^~~~~ this is an error`
            const squiggles_start = @max(compiler_error_start.len, error_columns.start);
            const total_length: usize = @max(compiler_error_start.len, error_columns.end) + error_message.len + 1;
            string = try SmallString.allocExactly(total_length);
            var buffer = string.buffer();
            // Always add in the starting `#@!`:
            @memcpy(buffer[0..compiler_error_start.len], compiler_error_start);
            if (error_columns.start >= compiler_error_start.len) {
                // Error comes completely after `#@!`, earliest at `#@!^~~~ there's an error here`
                @memset(buffer[compiler_error_start.len..error_columns.start], ' ');
                buffer[error_columns.start] = '^';
                @memset(buffer[error_columns.start + 1 .. error_columns.end], '~');
                buffer[error_columns.end] = ' ';
                @memcpy(buffer[error_columns.end + 1 ..], error_message);
            } else if (error_columns.end > compiler_error_start.len) {
                // Error was hitting into `#@!` a bit, just truncate `^~~`
                // `^~~~~ the error message`  +
                // `#@!`    =
                // `#@!~~ the error message`
                @memset(buffer[squiggles_start..error_columns.end], '~');
                buffer[error_columns.end] = ' ';
                @memcpy(buffer[error_columns.end + 1 ..], error_message);
            } else {
                // Error is completely within the `#@!` part, so just show
                // `#@! the error message`
                buffer[compiler_error_start.len] = ' ';
                @memcpy(buffer[compiler_error_start.len + 1 ..], error_message);
            }
        }
        string.sign();
        return string;
    }

    fn printErrorMessage(self: *Tokenizer, after_line_index: usize, error_columns: SmallString.Range, error_message: []const u8) void {
        // TODO: also print line[after_line_index]
        _ = self;
        common.stderr.print(
            "error {s} on line {d}:{d}-{d}\n",
            .{
                error_message,
                after_line_index,
                error_columns.start,
                error_columns.end,
            },
        ) catch {};
    }
};

pub const TokenTag = enum {
    end,
    starts_upper,
    starts_lower,
    newline,
    tab,
    operator,
};

pub const Token = union(TokenTag) {
    end: void,
    starts_upper: SmallString,
    starts_lower: SmallString,
    newline: usize,
    tab: usize,
    operator: u64,

    // TODO: add more of these tag helpers
    pub fn isNewline(self: Token) bool {
        return std.meta.activeTag(self) == TokenTag.newline;
    }

    pub fn isTab(self: Token) bool {
        return std.meta.activeTag(self) == TokenTag.tab;
    }

    pub fn isWhitespace(self: Token) bool {
        switch (std.meta.activeTag(self)) {
            TokenTag.newline, TokenTag.tab, TokenTag.end => return true,
            else => return false,
        }
    }

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
        // TODO: add an `errdefer` that will std.debug.print both `self` and `other`
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        try std.testing.expectEqual(tag_b, tag_a);

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
                    try std.testing.expectEqual(@field(b, field_info.name), @field(a, field_info.name));
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

    const newline = Token{ .newline = 123 };
    try newline.expectNotEquals(end);
    try std.testing.expect(!newline.equals(end));
    try newline.expectNotEquals(starts_upper);
    try std.testing.expect(!newline.equals(starts_upper));
    try newline.expectNotEquals(starts_lower);
    try std.testing.expect(!newline.equals(starts_lower));
    try newline.expectEquals(newline);
    try std.testing.expect(newline.equals(newline));
    try newline.expectNotEquals(Token{ .newline = 456 });
    try std.testing.expect(!newline.equals(Token{ .newline = 456 }));

    const tab = Token{ .tab = 123 };
    try tab.expectNotEquals(end);
    try std.testing.expect(!tab.equals(end));
    try tab.expectNotEquals(starts_upper);
    try std.testing.expect(!tab.equals(starts_upper));
    try tab.expectNotEquals(starts_lower);
    try std.testing.expect(!tab.equals(starts_lower));
    try tab.expectNotEquals(newline);
    try std.testing.expect(!tab.equals(newline));
    try tab.expectEquals(tab);
    try std.testing.expect(tab.equals(tab));
    try tab.expectNotEquals(Token{ .tab = 456 });
    try std.testing.expect(!tab.equals(Token{ .tab = 456 }));

    const operator = Token{ .operator = 123 };
    try operator.expectNotEquals(end);
    try std.testing.expect(!operator.equals(end));
    try operator.expectNotEquals(starts_upper);
    try std.testing.expect(!operator.equals(starts_upper));
    try operator.expectNotEquals(starts_lower);
    try std.testing.expect(!operator.equals(starts_lower));
    try operator.expectNotEquals(newline);
    try std.testing.expect(!operator.equals(newline));
    try operator.expectNotEquals(Token{ .tab = 123 });
    try std.testing.expect(!operator.equals(Token{ .tab = 123 }));
    try operator.expectEquals(operator);
    try std.testing.expect(operator.equals(operator));
    try operator.expectNotEquals(Token{ .operator = 456 });
    try std.testing.expect(!operator.equals(Token{ .operator = 456 }));
}

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

test "valid tokenizer operators" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    try tokenizer.file.lines.append(SmallString.noAlloc("="));
    try tokenizer.file.lines.append(SmallString.noAlloc("+"));
    try tokenizer.file.lines.append(SmallString.noAlloc("-"));
    try tokenizer.file.lines.append(SmallString.noAlloc("*"));
    try tokenizer.file.lines.append(SmallString.noAlloc("/"));

    var count: usize = 0;
    for (0..tokenizer.file.lines.count()) |line_index| {
        const line = tokenizer.file.lines.inBounds(line_index);

        var token = try tokenizer.at(count);
        try token.expectEquals(Token{ .tab = 0 });
        count += 1;

        token = try tokenizer.at(count);
        try token.expectEquals(Token{ .operator = try line.little64() });
        count += 1;

        token = try tokenizer.at(count);
        try token.expectEquals(Token{ .newline = line_index + 1 });
        count += 1;
    }
}

test "invalid tokenizer operators" {
    var invalid_lines = OwnedSmalls.init();
    defer invalid_lines.deinit();

    try invalid_lines.append(SmallString.noAlloc("=+"));
    try invalid_lines.append(SmallString.noAlloc("+++"));
    try invalid_lines.append(SmallString.noAlloc("*/"));
    try invalid_lines.append(SmallString.noAlloc("~~"));

    for (invalid_lines.items()) |line| {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();
        try tokenizer.file.lines.append(line);
        try tokenizer.file.lines.append(SmallString.noAlloc("second line"));

        // Technically we'll get an error even with `tokenizer.at(0)`
        // because we backfill an implicit tab, but that's a bit of
        // an implementation detail.
        try std.testing.expectError(TokenError.invalid_token, tokenizer.at(1));

        try tokenizer.file.lines.inBounds(0).expectEquals(line);
        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@! invalid operator");
        try tokenizer.file.lines.inBounds(2).expectEqualsString("second line");
    }
}

test "Tokenizer.addErrorAt" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();
    try tokenizer.file.lines.append(SmallString.noAlloc("zeroth"));
    try tokenizer.file.lines.append(SmallString.noAlloc("first"));
    try tokenizer.file.lines.append(SmallString.noAlloc("second"));
    try tokenizer.file.lines.append(SmallString.noAlloc("third"));
    try tokenizer.file.lines.append(SmallString.noAlloc("fourth"));
    try tokenizer.file.lines.append(SmallString.noAlloc("fifth"));
    try tokenizer.complete();

    // Add errors backwards since they'll be ignored for earlier errors otherwise.
    tokenizer.addErrorAt(5 * 3 + 1, .{ .start = 15, .end = 16 }, "bookmarked");
    tokenizer.addErrorAt(4 * 3 + 1, .{ .start = 11, .end = 16 }, "pad");
    tokenizer.addErrorAt(3 * 3 + 1, .{ .start = 10, .end = 14 }, "far out error");
    tokenizer.addErrorAt(2 * 3 + 1, .{ .start = 4, .end = 7 }, "with squiggles");
    tokenizer.addErrorAt(1 * 3 + 1, .{ .start = 3, .end = 4 }, "immediate caret");
    tokenizer.addErrorAt(0 * 3 + 1, .{ .start = 0, .end = 3 }, "hidden caret and squiggles");

    try tokenizer.file.lines.inBounds(5 * 2 + 1).expectEqualsString("#@! bookmarked ^");
    try tokenizer.file.lines.inBounds(4 * 2 + 1).expectEqualsString("#@!    pad ^~~~~");
    try tokenizer.file.lines.inBounds(3 * 2 + 1).expectEqualsString("#@!       ^~~~ far out error");
    try tokenizer.file.lines.inBounds(2 * 2 + 1).expectEqualsString("#@! ^~~ with squiggles");
    try tokenizer.file.lines.inBounds(1 * 2 + 1).expectEqualsString("#@!^ immediate caret");
    try tokenizer.file.lines.inBounds(0 * 2 + 1).expectEqualsString("#@! hidden caret and squiggles");
}

test "tokenizer tokenizing" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    try tokenizer.file.lines.append(try SmallString.init("  Hello w_o_rld   /  "));
    try tokenizer.file.lines.append(try SmallString.init("Second2    -l1ne   "));
    try tokenizer.file.lines.append(try SmallString.init("sp3cial* Fin_ancial  +  _problems"));

    var count: usize = 0;
    var token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 2 });
    try std.testing.expectEqual(0, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .starts_upper = SmallString.noAlloc("Hello") });
    try std.testing.expectEqual(0, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 8 }); // absolute tab
    try std.testing.expectEqual(0, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .starts_lower = SmallString.noAlloc("w_o_rld") });
    try std.testing.expectEqual(0, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 18 });
    try std.testing.expectEqual(0, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .operator = '/' });
    try std.testing.expectEqual(0, tokenizer.lineIndexAt(count));

    // Ignores tab/whitespace at the end

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .newline = 1 });
    try std.testing.expectEqual(1, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 0 });
    try std.testing.expectEqual(1, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .starts_upper = SmallString.noAlloc("Second2") });
    try std.testing.expectEqual(1, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 11 });
    try std.testing.expectEqual(1, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .operator = '-' });
    try std.testing.expectEqual(1, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 12 }); // implied tab
    try std.testing.expectEqual(1, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .starts_lower = SmallString.noAlloc("l1ne") });
    try std.testing.expectEqual(1, tokenizer.lineIndexAt(count));

    // Ignores tab/whitespace at the end

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .newline = 2 });
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 0 });
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .starts_lower = SmallString.noAlloc("sp3cial") });
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 7 }); // implied tab
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .operator = '*' });
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 9 });
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .starts_upper = SmallString.noAlloc("Fin_ancial") });
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 21 });
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .operator = '+' });
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .tab = 24 });
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .starts_upper = SmallString.noAlloc("_problems") });
    try std.testing.expectEqual(2, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(Token{ .newline = 3 });
    try std.testing.expectEqual(3, tokenizer.lineIndexAt(count));

    count += 1;
    token = try tokenizer.at(count);
    try token.expectEquals(.end);
    try std.testing.expectEqual(3, tokenizer.lineIndexAt(count));
}
