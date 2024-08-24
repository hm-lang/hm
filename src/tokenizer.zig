const common = @import("common.zig");
const OwnedList = @import("owned_list.zig").OwnedList;
const SmallString = @import("string.zig").Small;
const File = @import("file.zig").File;
const Operator = @import("operator.zig").Operator;
const Token = @import("token.zig").Token;

const OwnedSmalls = OwnedList(SmallString);
const OwnedTokens = OwnedList(Token);
const OwnedOpens = OwnedList(Token.Open);

const std = @import("std");

const TokenizerError = error{
    out_of_memory,
    out_of_tokens,
};

pub const Tokenizer = struct {
    tokens: OwnedTokens = OwnedTokens.init(),
    opens: OwnedOpens = OwnedOpens.init(),
    file: File = .{},
    farthest_line_index: u32 = 0,
    farthest_char_index: u16 = 0,
    /// This is the last token that you can grab, which is set by EOF or an error.
    /// If you ever backdate an error via `addErrorAt`, that will become the last
    /// "valid" token and you won't be able to grab tokens past that point anymore
    /// via `at()`.  (Not recommended, but you could still do so via `tokens.at`.)
    last_token_index: usize = std.math.maxInt(usize),

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.opens.deinit();
        self.file.deinit();
    }

    /// Do not deinitialize the returned `Token`, it's owned by `Tokenizer`.
    /// You're not allowed to negative-index this because you should only
    /// care about the next token (or a slice of them) and not the last token.
    /// Plus it's not obvious if that should be the current last token or
    /// the last token after we've completed adding all tokens.
    pub fn at(self: *Self, token_index: usize) TokenizerError!Token {
        // TODO: switch to `valid_token_count` and check `>=` instead of `>`
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

    pub fn printDebugInfoAt(self: *Self, token_index: usize) void {
        _ = self.at(token_index) catch {};
        if (token_index >= self.tokens.count()) {
            common.debugStderr.print("token {d} was out of bounds (count: {d})\n", .{
                token_index,
                self.tokens.count(),
            }) catch {};
            return;
        }
        const line_index = self.lineIndexAt(token_index);
        const columns = self.columnsAt(token_index);
        common.debugStderr.print("token {d} was on line {d}:{d}-{d}\n", .{
            token_index,
            line_index + 1,
            columns.start,
            columns.end,
        }) catch {};
        self.tokens.inBounds(token_index).printLine(common.debugStderr) catch {};
    }

    pub fn complete(self: *Self) TokenizerError!void {
        var last = try self.at(0);
        while (!last.equals(.end) and self.last_token_index >= self.tokens.count()) {
            last = try self.addNextToken();
        }
    }

    fn addNextToken(self: *Self) TokenizerError!Token {
        // TODO: if we get an error here in errdefer, we should move up
        //      last_token_index to self.tokens.count() - 1

        // We need to pass in the `starting_char_index` in case we need to
        // add an implicit tab before appending the next explicit token.
        const starting_char_index = self.farthest_char_index;
        if (self.farthest_line_index >= self.file.lines.count()) {
            try self.appendTokenAndPerformHooks(starting_char_index, .end);
            return .end;
        }
        const line = self.file.lines.inBounds(self.farthest_line_index);
        const next = if (self.farthest_char_index >= line.count())
            self.getNextNewline()
        else if (common.when(self.opens.at(-1), Token.Open.isQuote)) {
            // Inside quotes, we don't have hooks (besides those for `.end` and `.newline`).
            const token = try self.getNextInQuoteToken(line);
            try self.justAppendToken(token);
            return token;
        } else try self.getNextExplicitToken(line);
        try self.appendTokenAndPerformHooks(starting_char_index, next);
        return next;
    }

    inline fn justAppendToken(self: *Self, token: Token) TokenizerError!void {
        self.tokens.append(token) catch {
            return TokenizerError.out_of_memory;
        };
    }

    fn appendTokenAndPerformHooks(self: *Self, starting_char_index: u16, next: Token) TokenizerError!void {
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

        if (next.isNewline()) {
            // No need to add implicit spacing before a newline, but we do
            // need to see if we're currently in a multiline quote.
            // Multiline quotes automatically close out at the end of the line.
            const maybe_open = self.opens.at(-1);
            if (maybe_open) |open| {
                if (open == .multiline_quote) {
                    _ = self.opens.pop();
                    try self.justAppendToken(Token{ .close = .multiline_quote });
                }
            }
        } else if (next.isWhitespace() or common.when(self.tokens.before(initial_count), Token.isSpacing)) {
            // No need to add implicit spacing between existing whitespace...
        } else {
            // Add implied spacing so we can keep track of where we are.
            try self.justAppendToken(Token{ .spacing = .{
                .absolute = starting_char_index,
                .relative = 0,
            } });
        }
        try self.justAppendToken(next);
        // Some tokens have secondary effects.
        switch (next) {
            .invalid => |invalid| {
                self.addErrorAt(self.tokens.count() - 1, invalid.type.error_message());
            },
            .end => {
                self.last_token_index = self.tokens.count() - 1;
                const last_open = self.opens.at(-1) orelse return;
                self.addErrorAt(self.last_token_index, Token.InvalidType.expected_close(last_open).error_message());
            },
            .newline => {
                if (self.farthest_line_index == 0) {
                    @panic("you have too many lines in this file, we overflowed a u32");
                }
                self.removeNextErrorLines();
                const last_open = self.opens.at(-1) orelse return;
                if (last_open.isQuote()) {
                    // Multiline quotes should have already been taken care of in the pre-append logic,
                    // but single-line quotes (single quotes and double quotes) should not keep going here.
                    self.addErrorAt(self.tokens.count() - 1, Token.InvalidType.expected_close(last_open).error_message());
                }
            },
            else => {},
        }
    }

    fn getNextInQuoteToken(self: *Self, line: SmallString) TokenizerError!Token {
        const last_open = common.assert(self.opens.at(-1));
        const close_char = last_open.closeChar();
        const initial_char_index = self.farthest_char_index;

        var is_escaped = false;
        while (true) {
            const char = line.at(self.farthest_char_index);
            switch (char) {
                0 => {
                    // We reached the end of the line.
                    // Note that we need to have moved forward at least a little bit
                    // since we've already checked for EOL before calling this.
                    return Token{ .slice = try smallString(line.in(.{
                        .start = initial_char_index,
                        .end = self.farthest_char_index,
                    })) };
                },
                '$' => if (is_escaped) {
                    is_escaped = false;
                } else switch (line.at(self.farthest_char_index + 1)) {
                    '(' => return try self.maybeInterpolate(Token.Open.paren, line, initial_char_index),
                    '[' => return try self.maybeInterpolate(Token.Open.bracket, line, initial_char_index),
                    '{' => return try self.maybeInterpolate(Token.Open.brace, line, initial_char_index),
                    else => {},
                },
                '\\' => {
                    is_escaped = !is_escaped;
                },
                else => if (is_escaped) {
                    is_escaped = false;
                } else if (char != close_char) {
                    // don't do anything here.
                } else if (initial_char_index == self.farthest_char_index) {
                    _ = self.opens.pop();
                    self.farthest_char_index += 1;
                    return Token{ .close = last_open };
                } else {
                    return Token{ .slice = try smallString(line.in(.{
                        .start = initial_char_index,
                        .end = self.farthest_char_index,
                    })) };
                },
            }
            self.farthest_char_index += 1;
        }

        return TokenizerError.out_of_tokens;
    }

    fn maybeInterpolate(self: *Self, open: Token.Open, line: SmallString, initial_char_index: u16) TokenizerError!Token {
        // There was a `$` and then a `{` (or whatever.  `}` for balance.)
        // First check to see if we had any slices to add.
        if (self.farthest_char_index > initial_char_index) {
            try self.justAppendToken(Token{ .slice = try smallString(line.in(.{
                .start = initial_char_index,
                .end = self.farthest_char_index,
            })) });
        }
        self.farthest_char_index += 2;
        self.opens.append(open) catch return TokenizerError.out_of_memory;
        return Token{ .interpolation_open = open };
    }

    /// This should only fail for memory issues (e.g., allocating a string
    /// for an identifier).  Return an `InvalidToken` otherwise.
    fn getNextExplicitToken(self: *Self, line: SmallString) TokenizerError!Token {
        const initial_char_index = self.farthest_char_index;
        const starting_char = line.inBounds(self.farthest_char_index);
        const starts_with_whitespace = starting_char == ' ';
        if (starts_with_whitespace) {
            while (true) {
                self.farthest_char_index += 1;
                if (self.farthest_char_index >= line.count()) {
                    // Ignore whitespace at the end of a line.
                    return self.getNextNewline();
                }
                if (line.inBounds(self.farthest_char_index) != ' ') {
                    return Token{ .spacing = .{
                        .absolute = self.farthest_char_index,
                        .relative = self.farthest_char_index - initial_char_index,
                    } };
                }
            }
        } else {
            switch (starting_char) {
                '#' => return try self.getNextComment(line),
                'A'...'Z', '_' => return Token{ .starts_upper = try self.getNextIdentifier(line) },
                // TODO: do we want to support `and` here?  we could just use `&&`
                //      so that `X and(Y)` would be ok to overload.
                //      mostly eventually we need `xor`. maybe just use `&|` or |&`
                'a'...'z' => return Token{ .starts_lower = try self.getNextIdentifier(line) },
                '0'...'9' => return self.getNextNumber(line),
                '@' => return Token{ .annotation = try self.getNextIdentifier(line) },
                '\'' => return try self.getNextOpen(Token.Open.single_quote),
                '"' => return try self.getNextOpen(Token.Open.double_quote),
                '(' => return try self.getNextOpen(Token.Open.paren),
                '[' => return try self.getNextOpen(Token.Open.bracket),
                '{' => return try self.getNextOpen(Token.Open.brace),
                ')' => return self.getNextClose(Token.Close.paren),
                ']' => return self.getNextClose(Token.Close.bracket),
                '}' => return self.getNextClose(Token.Close.brace),
                ',' => return self.getNextComma(line),
                '!' => return self.getNextExclamationOperator(line),
                '?' => return self.getNextQuestionOperator(line),
                '$' => return self.getNextLambdaOperator(line),
                '&' => return try self.getNextAmpersandOperator(line),
                else => return self.getNextOperator(line),
            }
        }
    }

    fn getNextComment(self: *Self, line: SmallString) TokenizerError!Token {
        std.debug.assert(line.at(self.farthest_char_index) == '#');
        const initial_char_index = self.farthest_char_index;
        self.farthest_char_index += 1;
        // TODO: multiline comments, if desired.
        const midline_open = switch (line.at(self.farthest_char_index)) {
            // TODO: '@' => compiler comment
            '(' => Token.Open.paren,
            '[' => Token.Open.bracket,
            '{' => Token.Open.brace,
            else => {
                self.farthest_char_index = line.count();
                return Token{ .comment = try smallString(line.slice()[initial_char_index..self.farthest_char_index]) };
            },
        };
        const midline_close_char = midline_open.closeChar();
        var end_on_hashtag = false;
        while (true) {
            self.farthest_char_index += 1;
            const char = line.at(self.farthest_char_index);
            switch (char) {
                0 => return Token{ .invalid = .{
                    .columns = .{ .start = initial_char_index, .end = self.farthest_char_index },
                    .type = .midline_comment,
                } },
                '#' => if (end_on_hashtag) {
                    self.farthest_char_index += 1;
                    return Token{ .comment = try smallString(line.slice()[initial_char_index..self.farthest_char_index]) };
                },
                else => {
                    end_on_hashtag = char == midline_close_char;
                },
            }
        }
    }

    fn getNextIdentifier(self: *Self, line: SmallString) TokenizerError!SmallString {
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
        return smallString(line.slice()[initial_char_index..self.farthest_char_index]);
    }

    fn getNextNumber(self: *Self, line: SmallString) TokenizerError!Token {
        // We've already checked and there's a numerical character at the self.farthest_char_index.
        const initial_char_index = self.farthest_char_index;
        self.farthest_char_index += 1;
        var seen_e = false;
        var seen_fraction_separator = false;
        while (self.farthest_char_index < line.count()) {
            switch (line.inBounds(self.farthest_char_index)) {
                '0'...'9', '_' => {
                    self.farthest_char_index += 1;
                },
                'e', 'E' => {
                    self.farthest_char_index += 1;
                    if (seen_e) {
                        return Token{ .invalid = .{
                            .columns = .{ .start = initial_char_index, .end = self.farthest_char_index },
                            .type = Token.InvalidType.number,
                        } };
                    }
                    seen_e = true;
                },
                '.' => {
                    self.farthest_char_index += 1;
                    if (seen_e or seen_fraction_separator) {
                        return Token{ .invalid = .{
                            .columns = .{ .start = initial_char_index, .end = self.farthest_char_index },
                            .type = Token.InvalidType.number,
                        } };
                    }
                    seen_fraction_separator = true;
                },
                else => break,
            }
        }
        return Token{ .number = try smallString(line.slice()[initial_char_index..self.farthest_char_index]) };
    }

    fn getNextOpen(self: *Self, open: Token.Open) TokenizerError!Token {
        self.farthest_char_index += 1;
        self.opens.append(open) catch return TokenizerError.out_of_memory;
        return Token{ .open = open };
    }

    fn getNextClose(self: *Self, close: Token.Close) Token {
        std.debug.assert(!close.isQuote());
        const initial_char_index = self.farthest_char_index;
        self.farthest_char_index += 1;

        const last_open = self.opens.pop() orelse return Token{ .invalid = .{
            .columns = .{ .start = initial_char_index, .end = self.farthest_char_index },
            .type = .unexpected_close,
        } };

        if (last_open != close) {
            return Token{ .invalid = .{
                .columns = .{ .start = initial_char_index, .end = self.farthest_char_index },
                .type = Token.InvalidType.expected_close(last_open),
            } };
        }

        return Token{ .close = close };
    }

    fn getNextNewline(self: *Self) Token {
        self.farthest_char_index = 0;
        self.farthest_line_index += 1;
        return Token{ .newline = self.farthest_line_index };
    }

    fn getNextComma(self: *Self, line: SmallString) Token {
        const initial_char_index = self.farthest_char_index;
        // Commas are special in that they should never be combined
        // with other operators, e.g., ",+" should parse as ',' then '+'.
        self.farthest_char_index += 1;
        if (line.at(self.farthest_char_index) != ',') {
            return Token.comma;
        }
        // We have a problem, we should only have one comma.
        // If you want to do something like `[,,3]` use `[Null, Null, 3]`
        self.farthest_char_index += 1;
        while (line.at(self.farthest_char_index) == ',') {
            self.farthest_char_index += 1;
        }
        return Token{ .invalid = .{
            .columns = .{ .start = initial_char_index, .end = self.farthest_char_index },
            .type = .too_many_commas,
        } };
    }

    fn getNextExclamationOperator(self: *Self, line: SmallString) Token {
        std.debug.assert(line.at(self.farthest_char_index) == '!');
        switch (line.at(self.farthest_char_index + 1)) {
            '!' => {
                self.farthest_char_index += 2;
                return Token{ .operator = Operator.not_not };
            },
            '=' => {
                self.farthest_char_index += 2;
                return Token{ .operator = Operator.not_equal };
            },
            else => {
                self.farthest_char_index += 1;
                return Token{ .operator = Operator.not };
            },
        }
    }

    fn getNextQuestionOperator(self: *Self, line: SmallString) Token {
        std.debug.assert(line.at(self.farthest_char_index) == '?');
        if (line.at(self.farthest_char_index + 1) == '?') {
            return self.getNextOperator(line);
        }
        self.farthest_char_index += 1;
        return Token{ .operator = Operator.nullify };
    }

    fn getNextLambdaOperator(self: *Self, line: SmallString) Token {
        std.debug.assert(line.at(self.farthest_char_index) == '$');
        const initial_char_index = self.farthest_char_index;
        self.farthest_char_index += 1;
        while (line.at(self.farthest_char_index) == '$') {
            self.farthest_char_index += 1;
        }
        const lambda_length = self.farthest_char_index - initial_char_index;
        const operator: Operator = switch (lambda_length) {
            1 => .lambda1,
            2 => .lambda2,
            3 => .lambda3,
            4 => .lambda4,
            5 => .lambda5,
            6 => .lambda6,
            7 => .lambda7,
            8 => .lambda8,
            else => return self.getInvalidToken(initial_char_index, Token.InvalidType.operator),
        };
        return Token{ .operator = operator };
    }

    /// &| is a special operator to create multiline strings, so check for that first.
    fn getNextAmpersandOperator(self: *Self, line: SmallString) TokenizerError!Token {
        std.debug.assert(line.at(self.farthest_char_index) == '&');
        // check for a multiline string operator first...
        if (line.at(self.farthest_char_index + 1) == '|') {
            self.farthest_char_index += 2;
            self.opens.append(.multiline_quote) catch return TokenizerError.out_of_memory;
            return Token{ .open = .multiline_quote };
        }
        return self.getNextOperator(line);
    }

    fn getNextOperator(self: *Self, line: SmallString) Token {
        const initial_char_index = self.farthest_char_index;
        self.farthest_char_index += 1;
        while (self.farthest_char_index < line.count()) {
            switch (line.inBounds(self.farthest_char_index)) {
                '?',
                '~',
                '@',
                '%',
                '^',
                '&',
                '*',
                '/',
                '+',
                '-',
                '=',
                '>',
                '<',
                ':',
                ';',
                '.',
                => {
                    self.farthest_char_index += 1;
                },
                else => break,
            }
        }
        const buffer = line.slice()[initial_char_index..self.farthest_char_index];
        const operator = Operator.init(buffer);
        return if (operator == .none)
            self.getInvalidToken(initial_char_index, Token.InvalidType.operator)
        else
            Token{ .operator = operator };
    }

    /// The final column will be assumed to be self.farthest_char_index.
    fn getInvalidToken(self: *const Self, start_column: u16, invalid_type: Token.InvalidType) Token {
        return Token{ .invalid = .{
            .columns = .{ .start = start_column, .end = self.farthest_char_index },
            .type = invalid_type,
        } };
    }

    /// Indicates that the token at a given token index is an error.
    // TODO: add some optional "extra lines" to add as well as the error message
    //      for extra debugging help.
    /// Adds an error around the given token index (i.e., on the line after that token).
    pub fn addErrorAt(self: *Self, at_token_index: usize, error_message: []const u8) void {
        if (false) {
            common.debugStderr.print("\n\n", .{}) catch {};
            self.tokens.printLine(common.debugStderr) catch {};
        }
        std.debug.assert(at_token_index < self.tokens.count());
        const error_columns = self.columnsAt(at_token_index);
        const error_line_index = self.lineIndexAt(at_token_index);
        if (false) {
            common.debugStderr.print("{s} at token {d} that was on line {d}:{d}-{d}\n", .{
                error_message,
                at_token_index,
                error_line_index + 1,
                error_columns.start,
                error_columns.end,
            }) catch {};
        }
        if (at_token_index >= self.last_token_index) {
            common.debugStderr.print("token was past the last valid token index {d}\n", .{self.last_token_index}) catch {};
            return;
        }
        self.last_token_index = at_token_index;
        var string = getErrorLine(error_columns, error_message) catch {
            self.printErrorMessage(error_line_index, error_columns, error_message);
            return;
        };
        self.file.lines.insert(error_line_index + 1, string) catch {
            self.file.lines.inBounds(error_line_index).printLine(common.debugStderr) catch {};
            string.printLine(common.debugStderr) catch {};
            string.deinit();
            return;
        };
        // TODO: print `lines[error_line_index]` and `string` to common.stderr.
        //      get fancy with the colors around error_columns.
    }

    fn removeNextErrorLines(self: *Self) void {
        while (self.farthest_line_index < self.file.lines.count()) {
            var line = self.file.lines.inBounds(self.farthest_line_index);
            if (!line.contains("#@!", common.At.start)) {
                return;
            }
            _ = self.file.lines.remove(self.farthest_line_index);
            line.deinit();
        }
    }

    fn getErrorLine(error_columns: SmallString.Range, error_message: []const u8) SmallString.Error!SmallString {
        const compiler_error_start = "#@!";
        // Error message prefix if we can add the error message before `^~~~~~`:
        // +2 for the additional spaces before and after `error_message`.
        const pre_length = compiler_error_start.len + error_message.len + 2;
        var string: SmallString = undefined;
        if (error_columns.start >= pre_length) {
            // Error goes before "^~~~~", e.g.
            // `#@!     this is an error ^~~~~`
            const total_length: usize = @intCast(error_columns.end);
            string = try SmallString.allocExactly(total_length);
            var buffer = string.buffer();
            @memcpy(buffer[0..compiler_error_start.len], compiler_error_start);
            buffer[compiler_error_start.len] = ' ';
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

    fn printErrorMessage(self: *Self, error_line_index: usize, error_columns: SmallString.Range, error_message: []const u8) void {
        // TODO: also print line[error_line_index]
        _ = self;
        common.debugStderr.print(
            "error {s} on line {d}:{d}-{d}\n",
            .{
                error_message,
                error_line_index + 1,
                error_columns.start,
                error_columns.end,
            },
        ) catch {};
    }

    /// Returns the line index for the given token index.
    /// Looks backwards to find the nearest `newline` token.
    /// You should already have looked up to the token index via `at()` before calling this,
    /// so this has undefined behavior if it can't allocate the necessary tokens up to
    /// the passed-in `for_token_index`.
    fn lineIndexAt(self: *const Self, at_token_index: usize) usize {
        std.debug.assert(at_token_index < self.tokens.count());
        var token_index: i64 = @intCast(at_token_index);
        while (token_index >= 0) {
            const token = self.tokens.inBounds(@intCast(token_index));
            switch (token) {
                .newline => |line_index| if (at_token_index == token_index) {
                    // We're looking for the line index of the newline itself,
                    // which is actually one up.
                    // TODO: add explicit tests for this, the implicit problem was hard to debug
                    return line_index - 1;
                } else {
                    return line_index;
                },
                else => token_index -= 1,
            }
        }
        return 0;
    }

    /// Gets the line columns for the given token.
    fn columnsAt(self: *const Self, at_token_index: usize) SmallString.Range {
        std.debug.assert(at_token_index < self.tokens.count());
        const token = self.tokens.inBounds(at_token_index);
        switch (token) {
            .invalid => |invalid| return invalid.columns,
            .newline => |line_index| {
                const line_length = self.file.lines.inBounds(line_index - 1).count();
                return .{ .start = line_length, .end = line_length + 1 };
            },
            .end => {
                const line_length = (self.file.lines.at(-1) orelse SmallString{}).count();
                return .{ .start = line_length, .end = line_length + 1 };
            },
            .spacing => |spacing| {
                return .{ .start = spacing.absolute, .end = spacing.absolute + 1 };
            },
            else => if (self.tokens.before(at_token_index)) |before_token| {
                switch (before_token) {
                    .spacing => |spacing| {
                        return .{ .start = spacing.absolute, .end = spacing.absolute + token.countChars() };
                    },
                    else => {
                        // We add implicit spacing between everything that's not
                        // whitespace (see `fn appendTokenAndPerformHooks`).  So
                        // we're not sure what's happening here, so go ham.
                        common.debugStderr.print("expected to see a tab at tokens[index] or tokens[index - 1]\n", .{}) catch {};
                        return self.fullLineColumnsAt(at_token_index);
                    },
                }
            } else {
                common.debugStderr.print("expected to see spacing at tokens[0]\n", .{}) catch {};
                return self.fullLineColumnsAt(at_token_index);
            },
        }
    }

    fn fullLineColumnsAt(self: *const Self, at_token_index: usize) SmallString.Range {
        const line_index = self.lineIndexAt(at_token_index);
        if (self.file.lines.at(line_index)) |line| {
            return line.fullRange();
        } else {
            return .{ .start = 0, .end = 16 };
        }
    }

    fn lastTokenIndex(self: *Self) TokenizerError!usize {
        const count = self.tokens.count();
        return if (count > 0) count - 1 else TokenizerError.out_of_tokens;
    }

    fn smallString(buffer: []const u8) TokenizerError!SmallString {
        return SmallString.init(buffer) catch TokenizerError.out_of_memory;
    }

    const Self = @This();
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
        try token.expectEquals(Token{ .spacing = .{ .absolute = 0, .relative = 0 } });
        count += 1;

        token = try tokenizer.at(count);
        try token.expectEquals(Token{ .operator = Operator.init64(try line.big64()) });
        count += 1;

        token = try tokenizer.at(count);
        try token.expectEquals(Token{ .newline = @intCast(line_index + 1) });
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
    try tokenizer.file.lines.append(try SmallString.init("t wo"));
    try tokenizer.file.lines.append(try SmallString.init("th  ree"));
    try tokenizer.file.lines.append(try SmallString.init("fourth     four"));
    try tokenizer.file.lines.append(try SmallString.init("fifth       fifth"));
    try tokenizer.file.lines.append(try SmallString.init("sixth          a"));
    try tokenizer.file.lines.append(try SmallString.init("seventh             BB"));
    try tokenizer.complete();

    // Add errors backwards since they'll be broken by earlier errors otherwise.
    tokenizer.addErrorAt(7 * 5 + 3, "needs two spaces"); // target the second identifier
    tokenizer.addErrorAt(6 * 5 + 3, "bookmarked");
    tokenizer.addErrorAt(5 * 5 + 3, "five");
    tokenizer.addErrorAt(4 * 5 + 3, "four out error");
    tokenizer.addErrorAt(3 * 5 + 3, "with squiggles");
    tokenizer.addErrorAt(2 * 5 + 3, "just squiggles");
    tokenizer.addErrorAt(1 * 5 + 3, "immediate caret");
    tokenizer.addErrorAt(0 * 5 + 1, "hidden caret and squiggles"); // target the first identifier

    try tokenizer.file.lines.inBounds(7 * 2 + 1).expectEqualsString("#@!                 ^~ needs two spaces");
    //                                     e.g., this doesn't work: "#@!needs two spaces ^~"
    try tokenizer.file.lines.inBounds(6 * 2 + 1).expectEqualsString("#@! bookmarked ^");
    try tokenizer.file.lines.inBounds(5 * 2 + 1).expectEqualsString("#@!    five ^~~~~");
    try tokenizer.file.lines.inBounds(4 * 2 + 1).expectEqualsString("#@!        ^~~~ four out error");
    try tokenizer.file.lines.inBounds(3 * 2 + 1).expectEqualsString("#@! ^~~ with squiggles");
    try tokenizer.file.lines.inBounds(2 * 2 + 1).expectEqualsString("#@!~ just squiggles");
    try tokenizer.file.lines.inBounds(1 * 2 + 1).expectEqualsString("#@!^ immediate caret");
    try tokenizer.file.lines.inBounds(0 * 2 + 1).expectEqualsString("#@! hidden caret and squiggles");
}

test "tokenizer tokenizing" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    try tokenizer.file.lines.append(try SmallString.init("  Hello w_o_rld2  /  "));
    try tokenizer.file.lines.append(try SmallString.init("#@! ^ this was an error last time"));
    try tokenizer.file.lines.append(try SmallString.init("2.73456    -l1ne   "));
    try tokenizer.file.lines.append(try SmallString.init("#@! if we have multiple lines"));
    try tokenizer.file.lines.append(try SmallString.init("#@! it's because of additional info"));
    try tokenizer.file.lines.append(try SmallString.init("sp3cial* Fin_ancial  +  _problems"));
    try tokenizer.file.lines.append(try SmallString.init("#@!    assume we should get rid of this"));
    // TODO: test number tokenizing errors like `45e123.4` and `123.456.789`
    try tokenizer.file.lines.append(try SmallString.init("45.6e123   7E10 400."));
    try tokenizer.file.lines.append(try SmallString.init("3 ,+7,  ;= -80"));
    try tokenizer.file.lines.append(try SmallString.init("@[] @{} @() @ @hello_world  @A"));

    try tokenizer.complete();
    try tokenizer.tokens.expectEqualsSlice(&[_]Token{
        Token{ .spacing = .{ .absolute = 2, .relative = 2 } },
        Token{ .starts_upper = SmallString.noAlloc("Hello") },
        Token{ .spacing = .{ .absolute = 8, .relative = 1 } },
        Token{ .starts_lower = SmallString.noAlloc("w_o_rld2") },
        Token{ .spacing = .{ .absolute = 18, .relative = 2 } },
        Token{ .operator = .divide },
        // Ignores spacing at the end
        Token{ .newline = 1 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .number = SmallString.noAlloc("2.73456") },
        Token{ .spacing = .{ .absolute = 11, .relative = 4 } },
        Token{ .operator = .minus },
        Token{ .spacing = .{ .absolute = 12, .relative = 0 } },
        Token{ .starts_lower = SmallString.noAlloc("l1ne") },
        // Ignores spacing at the end
        Token{ .newline = 2 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .starts_lower = SmallString.noAlloc("sp3cial") },
        Token{ .spacing = .{ .absolute = 7, .relative = 0 } },
        Token{ .operator = .multiply },
        Token{ .spacing = .{ .absolute = 9, .relative = 1 } },
        Token{ .starts_upper = SmallString.noAlloc("Fin_ancial") },
        Token{ .spacing = .{ .absolute = 21, .relative = 2 } },
        Token{ .operator = .plus },
        Token{ .spacing = .{ .absolute = 24, .relative = 2 } },
        Token{ .starts_upper = SmallString.noAlloc("_problems") },
        Token{ .newline = 3 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .number = SmallString.noAlloc("45.6e123") },
        Token{ .spacing = .{ .absolute = 11, .relative = 3 } },
        Token{ .number = SmallString.noAlloc("7E10") },
        Token{ .spacing = .{ .absolute = 16, .relative = 1 } },
        Token{ .number = SmallString.noAlloc("400.") },
        Token{ .newline = 4 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .number = SmallString.noAlloc("3") },
        Token{ .spacing = .{ .absolute = 2, .relative = 1 } },
        Token{ .operator = .comma },
        Token{ .spacing = .{ .absolute = 3, .relative = 0 } },
        Token{ .operator = .plus },
        Token{ .spacing = .{ .absolute = 4, .relative = 0 } },
        Token{ .number = SmallString.noAlloc("7") },
        Token{ .spacing = .{ .absolute = 5, .relative = 0 } },
        Token{ .operator = .comma },
        Token{ .spacing = .{ .absolute = 8, .relative = 2 } },
        Token{ .operator = .declare_writable },
        Token{ .spacing = .{ .absolute = 11, .relative = 1 } },
        Token{ .operator = .minus },
        Token{ .spacing = .{ .absolute = 12, .relative = 0 } },
        Token{ .number = SmallString.noAlloc("80") },
        Token{ .newline = 5 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .annotation = SmallString.noAlloc("@") },
        Token{ .spacing = .{ .absolute = 1, .relative = 0 } },
        Token{ .open = Token.Open.bracket },
        Token{ .spacing = .{ .absolute = 2, .relative = 0 } },
        Token{ .close = Token.Close.bracket },
        Token{ .spacing = .{ .absolute = 4, .relative = 1 } },
        Token{ .annotation = SmallString.noAlloc("@") },
        Token{ .spacing = .{ .absolute = 5, .relative = 0 } },
        Token{ .open = Token.Open.brace },
        Token{ .spacing = .{ .absolute = 6, .relative = 0 } },
        Token{ .close = Token.Close.brace },
        Token{ .spacing = .{ .absolute = 8, .relative = 1 } },
        Token{ .annotation = SmallString.noAlloc("@") },
        Token{ .spacing = .{ .absolute = 9, .relative = 0 } },
        Token{ .open = Token.Open.paren },
        Token{ .spacing = .{ .absolute = 10, .relative = 0 } },
        Token{ .close = Token.Close.paren },
        Token{ .spacing = .{ .absolute = 12, .relative = 1 } },
        Token{ .annotation = SmallString.noAlloc("@") },
        Token{ .spacing = .{ .absolute = 14, .relative = 1 } },
        Token{ .annotation = SmallString.noAlloc("@hello_world") },
        Token{ .spacing = .{ .absolute = 28, .relative = 2 } },
        Token{ .annotation = SmallString.noAlloc("@A") },
        Token{ .newline = 6 },
        .end,
    });

    // Tokenizer will clean up the compile errors in the file automatically:
    try tokenizer.file.lines.inBounds(0).expectEqualsString("  Hello w_o_rld2  /  ");
    try tokenizer.file.lines.inBounds(1).expectEqualsString("2.73456    -l1ne   ");
    try tokenizer.file.lines.inBounds(2).expectEqualsString("sp3cial* Fin_ancial  +  _problems");
    try tokenizer.file.lines.inBounds(3).expectEqualsString("45.6e123   7E10 400.");
    try tokenizer.file.lines.inBounds(4).expectEqualsString("3 ,+7,  ;= -80");
    try tokenizer.file.lines.inBounds(5).expectEqualsString("@[] @{} @() @ @hello_world  @A");
}

test "tokenizer exclamation operators" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    try tokenizer.file.lines.append(try SmallString.init("! !! !!$ !="));

    try tokenizer.complete();
    try tokenizer.tokens.expectEqualsSlice(&[_]Token{
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .operator = .not },
        Token{ .spacing = .{ .absolute = 2, .relative = 1 } },
        Token{ .operator = .not_not },
        Token{ .spacing = .{ .absolute = 5, .relative = 1 } },
        Token{ .operator = .not_not },
        Token{ .spacing = .{ .absolute = 7, .relative = 0 } },
        Token{ .operator = .lambda1 },
        Token{ .spacing = .{ .absolute = 9, .relative = 1 } },
        Token{ .operator = .not_equal },
        Token{ .newline = 1 },
        .end,
    });
}

test "tokenizer question operators" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    try tokenizer.file.lines.append(try SmallString.init("?: ?? ?; ??= ?."));

    try tokenizer.complete();
    try tokenizer.tokens.expectEqualsSlice(&[_]Token{
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        // we want `?;` to parse as `?` then `;`, etc.
        Token{ .operator = .nullify },
        Token{ .spacing = .{ .absolute = 1, .relative = 0 } },
        Token{ .operator = .declare_readonly },
        Token{ .spacing = .{ .absolute = 3, .relative = 1 } },
        // we do allow `??` to parse together, and same with `??=`
        Token{ .operator = .nullish_or },
        Token{ .spacing = .{ .absolute = 6, .relative = 1 } },
        Token{ .operator = .nullify },
        Token{ .spacing = .{ .absolute = 7, .relative = 0 } },
        Token{ .operator = .declare_writable },
        Token{ .spacing = .{ .absolute = 9, .relative = 1 } },
        Token{ .operator = .nullish_or_assign },
        Token{ .spacing = .{ .absolute = 13, .relative = 1 } },
        Token{ .operator = .nullify },
        Token{ .spacing = .{ .absolute = 14, .relative = 0 } },
        Token{ .operator = .declare_temporary },
        Token{ .newline = 1 },
        .end,
    });
}

// TODO: invalid lambda test
test "tokenizer lambda operators" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    try tokenizer.file.lines.append(try SmallString.init("$$ !$ --$$$ $$$$$$$$"));
    try tokenizer.file.lines.append(try SmallString.init("$$$$$$$ $$$$$$ $$$$$ $$$$"));

    try tokenizer.complete();
    try tokenizer.tokens.expectEqualsSlice(&[_]Token{
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .operator = .lambda2 },
        Token{ .spacing = .{ .absolute = 3, .relative = 1 } },
        Token{ .operator = .not },
        Token{ .spacing = .{ .absolute = 4, .relative = 0 } },
        Token{ .operator = .lambda1 },
        Token{ .spacing = .{ .absolute = 6, .relative = 1 } },
        Token{ .operator = .decrement },
        Token{ .spacing = .{ .absolute = 8, .relative = 0 } },
        Token{ .operator = .lambda3 },
        Token{ .spacing = .{ .absolute = 12, .relative = 1 } },
        Token{ .operator = .lambda8 },
        Token{ .newline = 1 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .operator = Operator.lambda7 },
        Token{ .spacing = .{ .absolute = 8, .relative = 1 } },
        Token{ .operator = Operator.lambda6 },
        Token{ .spacing = .{ .absolute = 15, .relative = 1 } },
        Token{ .operator = Operator.lambda5 },
        Token{ .spacing = .{ .absolute = 21, .relative = 1 } },
        Token{ .operator = Operator.lambda4 },
        Token{ .newline = 2 },
        .end,
    });
}

test "tokenizer ampersand operators" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    try tokenizer.file.lines.append(try SmallString.init("&& &= &&= &|"));

    try tokenizer.complete();
    try tokenizer.tokens.expectEqualsSlice(&[_]Token{
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .operator = .logical_and },
        Token{ .spacing = .{ .absolute = 3, .relative = 1 } },
        Token{ .operator = .bitwise_and_assign },
        Token{ .spacing = .{ .absolute = 6, .relative = 1 } },
        Token{ .operator = .logical_and_assign },
        Token{ .spacing = .{ .absolute = 10, .relative = 1 } },
        Token{ .open = Token.Open.multiline_quote },
        Token{ .close = Token.Close.multiline_quote },
        Token{ .newline = 1 },
        .end,
    });
}

test "tokenizer parentheses ok" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    try tokenizer.file.lines.append(try SmallString.init("([{"));
    try tokenizer.file.lines.append(try SmallString.init("    }  ()[]{[[]( )]} ]   )"));
    try tokenizer.complete();

    try tokenizer.tokens.expectEqualsSlice(&[_]Token{
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .open = Token.Open.paren },
        Token{ .spacing = .{ .absolute = 1, .relative = 0 } },
        Token{ .open = Token.Open.bracket },
        Token{ .spacing = .{ .absolute = 2, .relative = 0 } },
        Token{ .open = Token.Open.brace },
        Token{ .newline = 1 },
        Token{ .spacing = .{ .absolute = 4, .relative = 4 } },
        Token{ .close = Token.Close.brace },
        Token{ .spacing = .{ .absolute = 7, .relative = 2 } },
        Token{ .open = Token.Open.paren },
        Token{ .spacing = .{ .absolute = 8, .relative = 0 } },
        Token{ .close = Token.Close.paren },
        Token{ .spacing = .{ .absolute = 9, .relative = 0 } },
        Token{ .open = Token.Open.bracket },
        Token{ .spacing = .{ .absolute = 10, .relative = 0 } },
        Token{ .close = Token.Close.bracket },
        Token{ .spacing = .{ .absolute = 11, .relative = 0 } },
        Token{ .open = Token.Open.brace },
        Token{ .spacing = .{ .absolute = 12, .relative = 0 } },
        Token{ .open = Token.Open.bracket },
        Token{ .spacing = .{ .absolute = 13, .relative = 0 } },
        Token{ .open = Token.Open.bracket },
        Token{ .spacing = .{ .absolute = 14, .relative = 0 } },
        Token{ .close = Token.Close.bracket },
        Token{ .spacing = .{ .absolute = 15, .relative = 0 } },
        Token{ .open = Token.Open.paren },
        Token{ .spacing = .{ .absolute = 17, .relative = 1 } },
        Token{ .close = Token.Close.paren },
        Token{ .spacing = .{ .absolute = 18, .relative = 0 } },
        Token{ .close = Token.Close.bracket },
        Token{ .spacing = .{ .absolute = 19, .relative = 0 } },
        Token{ .close = Token.Close.brace },
        Token{ .spacing = .{ .absolute = 21, .relative = 1 } },
        Token{ .close = Token.Close.bracket },
        Token{ .spacing = .{ .absolute = 25, .relative = 3 } },
        Token{ .close = Token.Close.paren },
        Token{ .newline = 2 },
        .end,
    });
}

test "tokenizer parentheses failure" {
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        // Keeping balance in the universe: [
        try tokenizer.file.lines.append(try SmallString.init("(    ]"));

        var count: usize = 0;
        try (try tokenizer.at(count)).expectEquals(Token{ .spacing = .{ .absolute = 0, .relative = 0 } });
        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .open = Token.Open.paren });

        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .spacing = .{ .absolute = 5, .relative = 4 } });
        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .invalid = .{
            .columns = .{ .start = 5, .end = 6 },
            .type = Token.InvalidType.expected_close_paren,
        } });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@!  ^ expected `)`");
    }
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        // Keeping balance in the universe: (
        try tokenizer.file.lines.append(try SmallString.init("  [)"));

        var count: usize = 0;
        try (try tokenizer.at(count)).expectEquals(Token{ .spacing = .{ .absolute = 2, .relative = 2 } });
        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .open = Token.Open.bracket });

        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .spacing = .{ .absolute = 3, .relative = 0 } });
        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .invalid = .{
            .columns = .{ .start = 3, .end = 4 },
            .type = Token.InvalidType.expected_close_bracket,
        } });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@!^ expected `]`");
    }
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        // Keeping balance in the universe: [
        try tokenizer.file.lines.append(try SmallString.init("    {            ]"));

        var count: usize = 0;
        try (try tokenizer.at(count)).expectEquals(Token{ .spacing = .{ .absolute = 4, .relative = 4 } });
        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .open = Token.Open.brace });

        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .spacing = .{ .absolute = 17, .relative = 12 } });
        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .invalid = .{
            .columns = .{ .start = 17, .end = 18 },
            .type = Token.InvalidType.expected_close_brace,
        } });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@! expected `}` ^");
    }
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        // Keeping balance in the universe: [
        try tokenizer.file.lines.append(try SmallString.init(" ]"));

        var count: usize = 0;
        try (try tokenizer.at(count)).expectEquals(Token{ .spacing = .{ .absolute = 1, .relative = 1 } });
        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .invalid = .{
            .columns = .{ .start = 1, .end = 2 },
            .type = Token.InvalidType.unexpected_close,
        } });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@! no corresponding open");
    }
}

test "tokenizer multiline quote parsing" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    // Empty strings
    try tokenizer.file.lines.append(try SmallString.init("  &|"));
    // 1-char strings
    try tokenizer.file.lines.append(try SmallString.init("&| "));
    // No need to escape things like other quotes or parentheses
    try tokenizer.file.lines.append(try SmallString.init("    &|$&|*'\"()[]{}`"));
    try tokenizer.complete();

    try tokenizer.tokens.expectEqualsSlice(&[_]Token{
        Token{ .spacing = .{ .absolute = 2, .relative = 2 } },
        Token{ .open = Token.Open.multiline_quote },
        Token{ .close = Token.Close.multiline_quote },
        Token{ .newline = 1 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .open = Token.Open.multiline_quote },
        Token{ .slice = SmallString.noAlloc(" ") },
        Token{ .close = Token.Close.multiline_quote },
        Token{ .newline = 2 },
        Token{ .spacing = .{ .absolute = 4, .relative = 4 } },
        Token{ .open = Token.Open.multiline_quote },
        Token{ .slice = SmallString.noAlloc("$&|*'\"()[]{}`") },
        Token{ .close = Token.Close.multiline_quote },
        Token{ .newline = 3 },
        .end,
    });
    try tokenizer.opens.expectEqualsSlice(&[_]Token.Open{});
}

test "tokenizer simple quote parsing" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    // Empty strings
    try tokenizer.file.lines.append(try SmallString.init("''   \"\""));
    // 1-char strings
    try tokenizer.file.lines.append(try SmallString.init("' ' \" \""));
    // strings of the other char, with space afterwards (should be ignored)
    try tokenizer.file.lines.append(try SmallString.init("'\"' \"'\"  "));
    // longer strings, no space between identifiers
    try tokenizer.file.lines.append(try SmallString.init("'abc'\"defgh\""));
    try tokenizer.complete();

    try tokenizer.tokens.expectEqualsSlice(&[_]Token{
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .open = Token.Open.single_quote },
        Token{ .close = Token.Close.single_quote },
        Token{ .spacing = .{ .absolute = 5, .relative = 3 } },
        Token{ .open = Token.Open.double_quote },
        Token{ .close = Token.Close.double_quote },
        Token{ .newline = 1 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .open = Token.Open.single_quote },
        Token{ .slice = SmallString.noAlloc(" ") },
        Token{ .close = Token.Close.single_quote },
        Token{ .spacing = .{ .absolute = 4, .relative = 1 } },
        Token{ .open = Token.Open.double_quote },
        Token{ .slice = SmallString.noAlloc(" ") },
        Token{ .close = Token.Close.double_quote },
        Token{ .newline = 2 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .open = Token.Open.single_quote },
        Token{ .slice = SmallString.noAlloc("\"") },
        Token{ .close = Token.Close.single_quote },
        Token{ .spacing = .{ .absolute = 4, .relative = 1 } },
        Token{ .open = Token.Open.double_quote },
        Token{ .slice = SmallString.noAlloc("'") },
        Token{ .close = Token.Close.double_quote },
        Token{ .newline = 3 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .open = Token.Open.single_quote },
        Token{ .slice = SmallString.noAlloc("abc") },
        Token{ .close = Token.Close.single_quote },
        Token{ .spacing = .{ .absolute = 5, .relative = 0 } },
        Token{ .open = Token.Open.double_quote },
        Token{ .slice = SmallString.noAlloc("defgh") },
        Token{ .close = Token.Close.double_quote },
        Token{ .newline = 4 },
        .end,
    });
    try tokenizer.opens.expectEqualsSlice(&[_]Token.Open{});
}

test "tokenizer interpolation parsing" {
    {
        // Testing $() inside ''
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        // Checking slices around interpolation as well.
        try tokenizer.file.lines.append(try SmallString.init("'hello, $(Name)!'"));
        try tokenizer.complete();

        try tokenizer.tokens.expectEqualsSlice(&[_]Token{
            Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
            Token{ .open = Token.Open.single_quote },
            Token{ .slice = SmallString.noAlloc("hello, ") },
            Token{ .interpolation_open = Token.Open.paren },
            Token{ .spacing = .{ .absolute = 10, .relative = 0 } },
            Token{ .starts_upper = SmallString.noAlloc("Name") },
            Token{ .spacing = .{ .absolute = 14, .relative = 0 } },
            Token{ .close = Token.Close.paren },
            Token{ .slice = SmallString.noAlloc("!") },
            Token{ .close = Token.Close.single_quote },
            Token{ .newline = 1 },
            .end,
        });
        try tokenizer.opens.expectEqualsSlice(&[_]Token.Open{});
    }
    {
        // Testing $[] inside ""
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        // No slices before/after interpolation.
        try tokenizer.file.lines.append(try SmallString.init("\"$[Wow, hi]\""));
        try tokenizer.complete();

        try tokenizer.tokens.expectEqualsSlice(&[_]Token{
            Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
            Token{ .open = Token.Open.double_quote },
            Token{ .interpolation_open = Token.Open.bracket },
            Token{ .spacing = .{ .absolute = 3, .relative = 0 } },
            Token{ .starts_upper = SmallString.noAlloc("Wow") },
            Token{ .spacing = .{ .absolute = 6, .relative = 0 } },
            Token{ .operator = .comma },
            Token{ .spacing = .{ .absolute = 8, .relative = 1 } },
            Token{ .starts_lower = SmallString.noAlloc("hi") },
            Token{ .spacing = .{ .absolute = 10, .relative = 0 } },
            Token{ .close = Token.Close.bracket },
            Token{ .close = Token.Close.double_quote },
            Token{ .newline = 1 },
            .end,
        });
        try tokenizer.opens.expectEqualsSlice(&[_]Token.Open{});
    }
    {
        // Testing ${} inside ""
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        // Also test interior spaces
        try tokenizer.file.lines.append(try SmallString.init("\"${ frankly(),  'Idgad' }\""));
        try tokenizer.complete();

        try tokenizer.tokens.expectEqualsSlice(&[_]Token{
            Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
            Token{ .open = Token.Open.double_quote },
            Token{ .interpolation_open = Token.Open.brace },
            Token{ .spacing = .{ .absolute = 4, .relative = 1 } },
            Token{ .starts_lower = SmallString.noAlloc("frankly") },
            Token{ .spacing = .{ .absolute = 11, .relative = 0 } },
            Token{ .open = Token.Open.paren },
            Token{ .spacing = .{ .absolute = 12, .relative = 0 } },
            Token{ .close = Token.Close.paren },
            Token{ .spacing = .{ .absolute = 13, .relative = 0 } },
            Token{ .operator = .comma },
            Token{ .spacing = .{ .absolute = 16, .relative = 2 } },
            Token{ .open = Token.Open.single_quote },
            Token{ .slice = SmallString.noAlloc("Idgad") },
            Token{ .close = Token.Open.single_quote },
            Token{ .spacing = .{ .absolute = 24, .relative = 1 } },
            Token{ .close = Token.Close.brace },
            Token{ .close = Token.Close.double_quote },
            Token{ .newline = 1 },
            .end,
        });
        try tokenizer.opens.expectEqualsSlice(&[_]Token.Open{});
    }
    {
        // Testing $() $[] and ${} inside &|
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        try tokenizer.file.lines.append(try SmallString.init("&|wow $(Name)-$[hi]=${*}"));
        try tokenizer.complete();

        try tokenizer.tokens.expectEqualsSlice(&[_]Token{
            Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
            Token{ .open = Token.Open.multiline_quote },
            Token{ .slice = SmallString.noAlloc("wow ") },
            Token{ .interpolation_open = Token.Open.paren },
            Token{ .spacing = .{ .absolute = 8, .relative = 0 } },
            Token{ .starts_upper = SmallString.noAlloc("Name") },
            Token{ .spacing = .{ .absolute = 12, .relative = 0 } },
            Token{ .close = Token.Close.paren },
            Token{ .slice = SmallString.noAlloc("-") },
            Token{ .interpolation_open = Token.Open.bracket },
            Token{ .spacing = .{ .absolute = 16, .relative = 0 } },
            Token{ .starts_lower = SmallString.noAlloc("hi") },
            Token{ .spacing = .{ .absolute = 18, .relative = 0 } },
            Token{ .close = Token.Close.bracket },
            Token{ .slice = SmallString.noAlloc("=") },
            Token{ .interpolation_open = Token.Open.brace },
            Token{ .spacing = .{ .absolute = 22, .relative = 0 } },
            Token{ .operator = .multiply },
            Token{ .spacing = .{ .absolute = 23, .relative = 0 } },
            Token{ .close = Token.Close.brace },
            Token{ .close = Token.Close.multiline_quote },
            Token{ .newline = 1 },
            .end,
        });
        try tokenizer.opens.expectEqualsSlice(&[_]Token.Open{});
    }
}

test "tokenizer nested interpolations" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();

    // We don't recommend doing this, but it is a "stress" test.
    try tokenizer.file.lines.append(try SmallString.init("\"a${'very$(nice* ) q' \"wow$[ Hi,"));
    try tokenizer.file.lines.append(try SmallString.init(" Hey, hello , 'Super$( -Nested)Bros' ]\"}z\""));
    try tokenizer.complete();

    try tokenizer.tokens.expectEqualsSlice(&[_]Token{
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .open = Token.Open.double_quote },
        Token{ .slice = SmallString.noAlloc("a") },
        Token{ .interpolation_open = Token.Open.brace },
        Token{ .spacing = .{ .absolute = 4, .relative = 0 } },
        Token{ .open = Token.Open.single_quote },
        Token{ .slice = SmallString.noAlloc("very") },
        Token{ .interpolation_open = Token.Open.paren },
        Token{ .spacing = .{ .absolute = 11, .relative = 0 } },
        Token{ .starts_lower = SmallString.noAlloc("nice") },
        Token{ .spacing = .{ .absolute = 15, .relative = 0 } },
        Token{ .operator = .multiply },
        Token{ .spacing = .{ .absolute = 17, .relative = 1 } },
        Token{ .close = Token.Close.paren },
        Token{ .slice = SmallString.noAlloc(" q") },
        Token{ .close = Token.Close.single_quote },
        Token{ .spacing = .{ .absolute = 22, .relative = 1 } },
        Token{ .open = Token.Open.double_quote },
        Token{ .slice = SmallString.noAlloc("wow") },
        Token{ .interpolation_open = Token.Open.bracket },
        Token{ .spacing = .{ .absolute = 29, .relative = 1 } },
        Token{ .starts_upper = SmallString.noAlloc("Hi") },
        Token{ .spacing = .{ .absolute = 31, .relative = 0 } },
        Token{ .operator = .comma },
        Token{ .newline = 1 },
        Token{ .spacing = .{ .absolute = 1, .relative = 1 } },
        Token{ .starts_upper = SmallString.noAlloc("Hey") },
        Token{ .spacing = .{ .absolute = 4, .relative = 0 } },
        Token{ .operator = .comma },
        Token{ .spacing = .{ .absolute = 6, .relative = 1 } },
        Token{ .starts_lower = SmallString.noAlloc("hello") },
        Token{ .spacing = .{ .absolute = 12, .relative = 1 } },
        Token{ .operator = .comma },
        Token{ .spacing = .{ .absolute = 14, .relative = 1 } },
        Token{ .open = Token.Open.single_quote },
        Token{ .slice = SmallString.noAlloc("Super") },
        Token{ .interpolation_open = Token.Open.paren },
        Token{ .spacing = .{ .absolute = 23, .relative = 1 } },
        Token{ .operator = .minus },
        Token{ .spacing = .{ .absolute = 24, .relative = 0 } },
        Token{ .starts_upper = SmallString.noAlloc("Nested") },
        Token{ .spacing = .{ .absolute = 30, .relative = 0 } },
        Token{ .close = Token.Close.paren },
        Token{ .slice = SmallString.noAlloc("Bros") },
        Token{ .close = Token.Close.single_quote },
        Token{ .spacing = .{ .absolute = 37, .relative = 1 } },
        Token{ .close = Token.Close.bracket },
        Token{ .close = Token.Close.double_quote },
        Token{ .spacing = .{ .absolute = 39, .relative = 0 } },
        Token{ .close = Token.Close.brace },
        Token{ .slice = SmallString.noAlloc("z") },
        Token{ .close = Token.Close.double_quote },
        Token{ .newline = 2 },
        .end,
    });
    try tokenizer.opens.expectEqualsSlice(&[_]Token.Open{});
}

test "tokenizer quote failures" {
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        try tokenizer.file.lines.append(try SmallString.init("    ' "));
        try tokenizer.complete();

        try tokenizer.tokens.expectEqualsSlice(&[_]Token{
            Token{ .spacing = .{ .absolute = 4, .relative = 4 } },
            Token{ .open = Token.Open.single_quote },
            Token{ .slice = SmallString.noAlloc(" ") },
            Token{ .newline = 1 },
        });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@!   ^ expected closing `'`");
    }
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        try tokenizer.file.lines.append(try SmallString.init("\"abc"));
        try tokenizer.complete();

        try tokenizer.tokens.expectEqualsSlice(&[_]Token{
            Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
            Token{ .open = Token.Open.double_quote },
            Token{ .slice = SmallString.noAlloc("abc") },
            Token{ .newline = 1 },
        });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@! ^ expected closing `\"`");
    }
}

test "tokenizer comma errors" {
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        try tokenizer.file.lines.append(try SmallString.init("   ,,"));

        var count: usize = 0;
        try (try tokenizer.at(count)).expectEquals(Token{ .spacing = .{ .absolute = 3, .relative = 3 } });
        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .invalid = .{
            .columns = .{ .start = 3, .end = 5 },
            .type = Token.InvalidType.too_many_commas,
        } });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@!^~ too many commas");
    }
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();

        try tokenizer.file.lines.append(try SmallString.init("      ,,,,,"));

        var count: usize = 0;
        try (try tokenizer.at(count)).expectEquals(Token{ .spacing = .{ .absolute = 6, .relative = 6 } });
        count += 1;
        try (try tokenizer.at(count)).expectEquals(Token{ .invalid = .{
            .columns = .{ .start = 6, .end = 11 },
            .type = Token.InvalidType.too_many_commas,
        } });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@!   ^~~~~ too many commas");
    }
}

test "tokenizer comments" {
    var tokenizer: Tokenizer = .{};
    defer tokenizer.deinit();
    try tokenizer.file.lines.append(try SmallString.init("# full comment"));
    try tokenizer.file.lines.append(try SmallString.init("3#end the line"));
    try tokenizer.file.lines.append(try SmallString.init("  B   #  also EOL"));
    try tokenizer.file.lines.append(try SmallString.init("#( OH YEAH )# hi"));
    // For balance, `[`
    try tokenizer.file.lines.append(try SmallString.init("start  #[[great]]]#Finish"));
    try tokenizer.file.lines.append(try SmallString.init("odd#{{ok}#")); // `}` for balance

    try tokenizer.complete();

    try tokenizer.tokens.expectEqualsSlice(&[_]Token{
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .comment = SmallString.noAlloc("# full comment") },
        Token{ .newline = 1 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .number = SmallString.noAlloc("3") },
        Token{ .spacing = .{ .absolute = 1, .relative = 0 } },
        Token{ .comment = SmallString.noAlloc("#end the line") },
        Token{ .newline = 2 },
        Token{ .spacing = .{ .absolute = 2, .relative = 2 } },
        Token{ .starts_upper = SmallString.noAlloc("B") },
        Token{ .spacing = .{ .absolute = 6, .relative = 3 } },
        Token{ .comment = SmallString.noAlloc("#  also EOL") },
        Token{ .newline = 3 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .comment = SmallString.noAlloc("#( OH YEAH )#") },
        Token{ .spacing = .{ .absolute = 14, .relative = 1 } },
        Token{ .starts_lower = SmallString.noAlloc("hi") },
        Token{ .newline = 4 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .starts_lower = SmallString.noAlloc("start") },
        Token{ .spacing = .{ .absolute = 7, .relative = 2 } },
        // `[` for balance
        Token{ .comment = SmallString.noAlloc("#[[great]]]#") },
        Token{ .spacing = .{ .absolute = 19, .relative = 0 } },
        Token{ .starts_upper = SmallString.noAlloc("Finish") },
        Token{ .newline = 5 },
        Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
        Token{ .starts_lower = SmallString.noAlloc("odd") },
        Token{ .spacing = .{ .absolute = 3, .relative = 0 } },
        Token{ .comment = SmallString.noAlloc("#{{ok}#") }, // `}` for balance
        Token{ .newline = 6 },
        .end,
    });
}

test "tokenizer comment errors" {
    {
        var tokenizer: Tokenizer = .{};
        defer tokenizer.deinit();
        try tokenizer.file.lines.append(try SmallString.init("hi #[unending comment"));

        try tokenizer.complete();

        try tokenizer.tokens.expectEqualsSlice(&[_]Token{
            Token{ .spacing = .{ .absolute = 0, .relative = 0 } },
            Token{ .starts_lower = SmallString.noAlloc("hi") },
            Token{ .spacing = .{ .absolute = 3, .relative = 1 } },
            Token{ .invalid = .{ .columns = .{ .start = 3, .end = 21 }, .type = .midline_comment } },
        });

        try tokenizer.file.lines.inBounds(1).expectEqualsString("#@!^~~~~~~~~~~~~~~~~~ midline comment should end this line");
    }
}
