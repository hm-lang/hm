const common = @import("common.zig");
const owned_list = @import("owned_list.zig");
const SmallString = @import("string.zig").Small;
const File = @import("file.zig").File;
const Token = @import("token.zig").Token;

const OwnedSmalls = owned_list.OwnedList(SmallString);
const OwnedTokens = owned_list.OwnedList(Token);

const std = @import("std");

const TokenizerError = error{
    out_of_memory,
    out_of_tokens,
};

pub const Tokenizer = struct {
    tokens: OwnedTokens = OwnedTokens.init(),
    file: File = .{},
    farthest_line_index: u32 = 0,
    farthest_char_index: u16 = 0,
    /// This is the last token that you can grab, which is set by EOF or an error.
    /// If you ever backdate an error via `addErrorAt`, that will become the last
    /// "valid" token and you won't be able to grab tokens past that point anymore
    /// via `at()`.  (Not recommended, but you could still do so via `tokens.at`.)
    last_token_index: usize = std.math.maxInt(usize),

    pub fn deinit(self: *Tokenizer) void {
        self.tokens.deinit();
        self.file.deinit();
    }

    /// Returns the line index for the given token index.
    /// Looks backwards to find the nearest `newline` token.
    /// You should already have looked up to the token index via `at()` before calling this,
    /// so this has undefined behavior if it can't allocate the necessary tokens up to
    /// the passed-in `for_token_index`.
    fn lineIndexAt(self: *const Tokenizer, at_token_index: usize) usize {
        std.debug.assert(at_token_index < self.tokens.count());
        var token_index: i64 = @intCast(at_token_index);
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

    /// Gets the line columns for the given token.
    fn columnsAt(self: *const Tokenizer, at_token_index: usize) SmallString.Range {
        std.debug.assert(at_token_index < self.tokens.count());
        const token = self.tokens.inBounds(at_token_index);
        switch (token) {
            .invalid => |invalid| return invalid.columns,
            .newline => |line_index| {
                const line_length = self.file.lines.inBounds(line_index - 1).count();
                return .{ .start = common.before(line_length) catch 0, .end = line_length };
            },
            .tab => |tab_u| {
                // TODO: switch to u16 in tab: `Token{tab: u16}`
                const tab: u16 = @intCast(tab_u);
                return .{ .start = tab, .end = tab + 1 };
            },
            else => if (self.tokens.before(at_token_index)) |before_token| {
                switch (before_token) {
                    .tab => |tab_u| {
                        const tab: u16 = @intCast(tab_u);
                        return .{ .start = tab, .end = tab + token.countChars() };
                    },
                    else => {
                        // We add an implied tab between everything that's not
                        // whitespace (see `fn appendToken`).  So we're not sure
                        // what's happening here, so go ham.
                        std.debug.print("expected to see a tab at tokens[index] or tokens[index - 1]\n", .{});
                        return self.fullLineColumnsAt(at_token_index);
                    },
                }
            } else {
                std.debug.print("expected to see a tab at tokens[0]\n", .{});
                return self.fullLineColumnsAt(at_token_index);
            },
        }
    }

    fn fullLineColumnsAt(self: *const Tokenizer, at_token_index: usize) SmallString.Range {
        const line_index = self.lineIndexAt(at_token_index);
        if (self.file.lines.at(line_index)) |line| {
            return line.fullRange();
        } else {
            return .{ .start = 0, .end = 16 };
        }
    }

    fn lastTokenIndex(self: *Tokenizer) TokenizerError!usize {
        const count = self.tokens.count();
        return if (count > 0) count - 1 else TokenizerError.out_of_tokens;
    }

    /// Do not deinitialize the returned `Token`, it's owned by `Tokenizer`.
    /// You're not allowed to negative-index this because you should only
    /// care about the next token (or a slice of them) and not the last token.
    /// Plus it's not obvious if that should be the current last token or
    /// the last token after we've completed adding all tokens.
    pub fn at(self: *Tokenizer, token_index: usize) TokenizerError!Token {
        if (token_index > self.last_token_index) {
            return TokenizerError.out_of_tokens;
        }
        while (token_index >= self.tokens.count()) {
            _ = try self.addNextToken();
            if (token_index > self.last_token_index) {
                return TokenizerError.out_of_tokens;
            }
        }
        return self.tokens.inBounds(token_index);
    }

    fn complete(self: *Tokenizer) TokenizerError!void {
        var last = try self.at(0);
        while (!last.equals(.end)) {
            last = try self.addNextToken();
        }
    }

    fn appendToken(self: *Tokenizer, starting_char_index: u16, next: Token) TokenizerError!void {
        errdefer switch (next) {
            .invalid => |invalid| {
                common.stdout.print("ran out of memory adding an invalid token on {d}:{d}-{d}\n", .{
                    self.farthest_line_index,
                    invalid.columns.start,
                    invalid.columns.end,
                }) catch {};
                @panic("ran out of memory");
            },
            else => {},
        };
        const initial_count = self.tokens.count();

        if (next.isWhitespace() or common.when(self.tokens.before(initial_count), Token.isTab)) {
            // No need to add an implied tab between existing whitespace...
        } else {
            // Add an "implied" tab so we can keep track of where we are.
            self.tokens.append(Token{ .tab = starting_char_index }) catch {
                return TokenizerError.out_of_memory;
            };
        }
        self.tokens.append(next) catch {
            return TokenizerError.out_of_memory;
        };
        // Some tokens have secondary effects.
        switch (next) {
            .invalid => |invalid| {
                const error_message = switch (invalid.type) {
                    .operator => "invalid operator",
                };
                self.addErrorAt(self.tokens.count() - 1, error_message);
            },
            .end => {
                self.last_token_index = self.tokens.count() - 1;
            },
            .newline => {
                if (self.farthest_line_index == 0) {
                    @panic("you have too many lines in this file, we overflowed a u32");
                }
            },
            else => {},
        }
    }

    fn addNextToken(self: *Tokenizer) TokenizerError!Token {
        // We need to pass in the `starting_char_index` in case we need to
        // add an implicit tab before appending the next explicit token.
        const starting_char_index = self.farthest_char_index;
        const next = try self.getNextExplicitToken();
        try self.appendToken(starting_char_index, next);
        return next;
    }

    /// This should only fail for memory issues (e.g., allocating a string
    /// for an identifier).  Return an `InvalidToken` otherwise.
    fn getNextExplicitToken(self: *Tokenizer) TokenizerError!Token {
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
                else => return self.getNextOperator(line),
            }
        }
    }

    fn getNewline(self: *Tokenizer) Token {
        self.farthest_char_index = 0;
        self.farthest_line_index += 1;
        // TODO: if self.farthest_line_index wraps to 0, addErrorAt
        return Token{ .newline = self.farthest_line_index };
    }

    fn getNextIdentifier(self: *Tokenizer, line: SmallString) TokenizerError!SmallString {
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
            return TokenizerError.out_of_memory;
        };
    }

    fn getNextOperator(self: *Tokenizer, line: SmallString) Token {
        const initial_char_index = self.farthest_char_index;
        errdefer {
            self.appendInvalidToken(
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
            return self.getInvalidToken(initial_char_index, Token.InvalidType.operator);
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
                return self.getInvalidToken(initial_char_index, Token.InvalidType.operator);
            },
        }
        return Token{ .operator = operator };
    }

    /// The final column will be assumed to be self.farthest_char_index.
    fn getInvalidToken(self: *const Tokenizer, start_column: u16, invalid_type: Token.InvalidType) Token {
        return Token{ .invalid = .{
            .columns = .{ .start = start_column, .end = self.farthest_char_index },
            .type = invalid_type,
        } };
    }

    /// Indicates that the token at a given token index is an error.
    // TODO: add some optional "extra lines" to add as well as the error message
    //      for extra debugging help.
    /// Adds an error around the given token index (i.e., on the line after that token).
    pub fn addErrorAt(self: *Tokenizer, at_token_index: usize, error_message: []const u8) void {
        std.debug.assert(at_token_index < self.tokens.count());
        std.debug.assert(at_token_index < self.last_token_index);
        self.last_token_index = at_token_index;
        const error_columns = self.columnsAt(at_token_index);
        const error_line_index = self.lineIndexAt(at_token_index);
        // TODO: these comment lines need to be skipped while parsing and removed from self.file
        //      i.e., any line starting with '#@!'
        var string = getErrorLine(error_columns, error_message) catch {
            self.printErrorMessage(error_line_index, error_columns, error_message);
            return;
        };
        self.file.lines.insert(error_line_index + 1, string) catch {
            self.file.lines.inBounds(error_line_index).printLine(common.stderr) catch {};
            string.printLine(common.stderr) catch {};
            string.deinit();
            return;
        };
        // TODO: print `lines[error_line_index]` and `string` to common.stderr.
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

    fn printErrorMessage(self: *Tokenizer, error_line_index: usize, error_columns: SmallString.Range, error_message: []const u8) void {
        // TODO: also print line[error_line_index]
        _ = self;
        common.stderr.print(
            "error {s} on line {d}:{d}-{d}\n",
            .{
                error_message,
                error_line_index,
                error_columns.start,
                error_columns.end,
            },
        ) catch {};
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

        try (try tokenizer.at(1)).expectEquals(Token{ .invalid = .{
            .columns = line.fullRange(),
            .type = Token.InvalidType.operator,
        } });

        try tokenizer.file.lines.inBounds(0).expectEquals(line);
        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@! invalid operator");
        try tokenizer.file.lines.inBounds(2).expectEqualsString("second line");
    }
}

test "indented invalid tokenizer operators" {
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();
        try tokenizer.file.lines.append(try SmallString.init("                     =+="));

        try (try tokenizer.at(1)).expectEquals(Token{ .invalid = .{
            .columns = .{ .start = 21, .end = 24 },
            .type = Token.InvalidType.operator,
        } });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@! invalid operator ^~~");
    }
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();
        try tokenizer.file.lines.append(SmallString.noAlloc(" +-+"));

        try (try tokenizer.at(1)).expectEquals(Token{ .invalid = .{
            .columns = .{ .start = 1, .end = 4 },
            .type = Token.InvalidType.operator,
        } });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@!~ invalid operator");
    }
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();
        try tokenizer.file.lines.append(SmallString.noAlloc("       -----/"));

        try (try tokenizer.at(1)).expectEquals(Token{ .invalid = .{
            .columns = .{ .start = 7, .end = 13 },
            .type = Token.InvalidType.operator,
        } });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@!    ^~~~~~ invalid operator");
    }
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();
        try tokenizer.file.lines.append(SmallString.noAlloc("%%%%%%%%%%")); // > 8 chars to test buffer overrun

        try (try tokenizer.at(1)).expectEquals(Token{ .invalid = .{
            .columns = .{ .start = 0, .end = 10 },
            .type = Token.InvalidType.operator,
        } });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@!~~~~~~~ invalid operator");
    }
}

test "Tokenizer.addErrorAt" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();
    try tokenizer.file.lines.append(try SmallString.init("ze ro"));
    try tokenizer.file.lines.append(try SmallString.init("on e"));
    try tokenizer.file.lines.append(try SmallString.init("two ddd"));
    try tokenizer.file.lines.append(try SmallString.init("third     cccc"));
    try tokenizer.file.lines.append(try SmallString.init("fourth     BBBBB"));
    try tokenizer.file.lines.append(try SmallString.init("fifth          a"));
    try tokenizer.complete();

    // Add errors backwards since they'll be broken by earlier errors otherwise.
    tokenizer.addErrorAt(5 * 5 + 3, "bookmarked"); // target the second identifier
    tokenizer.addErrorAt(4 * 5 + 3, "pad");
    tokenizer.addErrorAt(3 * 5 + 3, "far out error");
    tokenizer.addErrorAt(2 * 5 + 3, "with squiggles");
    tokenizer.addErrorAt(1 * 5 + 3, "immediate caret");
    tokenizer.addErrorAt(0 * 5 + 1, "hidden caret and squiggles"); // target the first identifier

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
