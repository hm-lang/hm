const SmallString = @import("string.zig").Small;
const Operator = @import("operator.zig").Operator;
const common = @import("common.zig");
const Tab = @import("tab.zig").Tab;

const std = @import("std");

const TokenTag = enum {
    // [0]:
    invalid,
    file_end,
    // includes newlines.
    // TODO: i think i want to return to newlines as separate
    spacing,
    starts_upper,
    starts_lower,
    /// a part of a string (pure string), e.g., "hello, world"
    /// becomes just the inner part (no quotes).  escape sequences
    /// will still be present, e.g., \" for escaping the quote.
    // [5]:
    slice,
    number,
    operator,
    open,
    close,
    // [10]:
    /// E.g., for "$(MyLogic)" inside a string, the opening paren.
    /// also ok for "$[MyLogic]" or '${MyLogic}'
    interpolation_open,
    annotation,
    comment,
};

pub const Token = union(TokenTag) {
    invalid: InvalidToken,
    file_end: void,
    spacing: SpacingToken,
    starts_upper: SmallString,
    starts_lower: SmallString,
    slice: SmallString,
    /// We don't try to create a `dbl` or `int` here, just represent it faithfully for now.
    number: SmallString,
    operator: Operator,
    open: Open,
    close: Close,
    /// Only paren, bracket, and brace are valid here.
    interpolation_open: Open,
    annotation: SmallString,
    comment: SmallString,

    pub inline fn tag(self: Token) TokenTag {
        return std.meta.activeTag(self);
    }

    pub fn deinit(self: Token) void {
        const my_tag = std.meta.activeTag(self);
        const info = switch (@typeInfo(Token)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(TokenTag, field_info.name) == my_tag) {
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

    pub fn isNewline(self: Self) bool {
        return switch (self) {
            .spacing => |spacing| spacing.isNewline(),
            .file_end => true,
            else => false,
        };
    }

    pub fn countChars(self: Token) u16 {
        return switch (self) {
            .invalid => |invalid| invalid.columns.count(),
            .file_end => 0,
            .spacing => 0,
            .starts_upper => |string| string.count(),
            .starts_lower => |string| string.count(),
            .slice => |string| string.count(),
            .number => |string| string.count(),
            .operator => |operator| operator.string().count(),
            .open => 1,
            .interpolation_open => 2,
            .close => 1,
            .annotation => |string| string.count(),
            .comment => |string| string.count(),
        };
    }

    pub fn isFileEnd(self: Token) bool {
        return std.meta.activeTag(self) == TokenTag.file_end;
    }

    pub fn isSpacing(self: Token) bool {
        return std.meta.activeTag(self) == TokenTag.spacing;
    }

    pub fn isWhitespace(self: Token) bool {
        switch (std.meta.activeTag(self)) {
            TokenTag.spacing, TokenTag.file_end => return true,
            else => return false,
        }
    }

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        switch (self) {
            .invalid => |invalid| {
                try writer.print("Token{{ .invalid = .{{ .columns = .{{ start = {d}, .end = {d} }}, .type = {d} }} }}", .{
                    invalid.columns.start,
                    invalid.columns.end,
                    @intFromEnum(invalid.type),
                });
            },
            .file_end => {
                try writer.print(".file_end", .{});
            },
            .spacing => |value| {
                try writer.print("Token{{ .spacing = .{{ .absolute = {d}, .relative = {d}, .line = {d} }} }}", .{
                    value.absolute,
                    value.relative,
                    value.line,
                });
            },
            .starts_upper => |string| {
                try writer.print("Token{{ .starts_upper = try SmallString.init(\"", .{});
                try string.print(writer);
                try writer.print("\") }}", .{});
            },
            .starts_lower => |string| {
                try writer.print("Token{{ .starts_lower = try SmallString.init(\"", .{});
                try string.print(writer);
                try writer.print("\") }}", .{});
            },
            .slice => |string| {
                try writer.print("Token{{ .slice = try SmallString.init(\"", .{});
                try string.print(writer);
                try writer.print("\") }}", .{});
            },
            .number => |string| {
                try writer.print("Token{{ .number = try SmallString.init(\"", .{});
                try string.print(writer);
                try writer.print("\") }}", .{});
            },
            .operator => |operator| {
                try writer.print("Token{{ .operator = ", .{});
                try operator.print(writer);
                try writer.print(" }}", .{});
            },
            .open => |open| {
                try writer.print("Token{{ .open = Token.Open.{s} }}", .{open.slice()});
            },
            .interpolation_open => |open| {
                try writer.print("Token{{ .interpolation_open = Token.Open.{s} }}", .{open.slice()});
            },
            .close => |close| {
                try writer.print("Token{{ .close = Token.Close.{s} }}", .{close.slice()});
            },
            .annotation => |string| {
                try writer.print("Token{{ .annotation = try SmallString.init(\"", .{});
                try string.print(writer);
                try writer.print("\") }}", .{});
            },
            .comment => |string| {
                try writer.print("Token{{ .comment = try SmallString.init(\"", .{});
                try string.print(writer);
                try writer.print("\") }}", .{});
            },
        }
    }

    pub fn equals(a: Self, b: Self) bool {
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        if (tag_a != tag_b) return false;

        const info = switch (@typeInfo(Self)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(Tag, field_info.name) == tag_a) {
                const SubField = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(SubField, "equals")) {
                    return @field(a, field_info.name).equals(@field(b, field_info.name));
                } else {
                    return @field(a, field_info.name) == @field(b, field_info.name);
                }
            }
        }
        return false;
    }

    pub fn expectEquals(a: Self, b: Self) !void {
        const stderr = common.debugStderr;
        errdefer {
            stderr.print("expected:\n", .{}) catch {};
            b.printLine(stderr) catch {};

            stderr.print("got:\n", .{}) catch {};
            a.printLine(stderr) catch {};
        }
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        try std.testing.expectEqual(tag_b, tag_a);

        const info = switch (@typeInfo(Self)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(Tag, field_info.name) == tag_a) {
                const SubField = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(SubField, "expectEquals")) {
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

    pub fn expectNotEquals(a: Self, b: Self) !void {
        const stderr = common.debugStderr;
        errdefer {
            stderr.print("expected NOT this, but got it:\n", .{}) catch {};
            a.printLine(stderr) catch {};
        }
        try std.testing.expect(!a.equals(b));
    }

    pub const comma = Self{ .operator = Operator.comma };

    pub const Open = enum {
        paren,
        bracket,
        brace,
        single_quote,
        double_quote,
        multiline_quote,

        pub fn slice(self: Open) []const u8 {
            return switch (self) {
                .paren => "paren",
                .bracket => "bracket",
                .brace => "brace",
                .single_quote => "single_quote",
                .double_quote => "double_quote",
                .multiline_quote => "multiline_quote",
            };
        }

        pub fn fromTab(tabbed_open: Tab.Open) common.Error!Open {
            return switch (tabbed_open) {
                .paren => .paren,
                .bracket => .bracket,
                .brace => .brace,
                .none => common.Error.invalid_argument,
            };
        }

        pub fn toTab(self: Open) common.Error!Tab.Open {
            return switch (self) {
                .paren => .paren,
                .bracket => .bracket,
                .brace => .brace,
                .single_quote,
                .double_quote,
                .multiline_quote,
                => common.Error.invalid_argument,
            };
        }

        pub fn printLine(self: Open, writer: anytype) !void {
            try self.print(writer);
            try writer.print("\n", .{});
        }

        pub fn print(self: Open, writer: anytype) !void {
            // TODO: consider doing separate `Open.paren` and `Close.brace` logic.
            try writer.print("{s}", .{self.slice()});
        }

        pub fn isQuote(self: Open) bool {
            return switch (self) {
                .paren, .bracket, .brace => false,
                .single_quote, .double_quote, .multiline_quote => true,
            };
        }

        pub fn openChar(self: Open) u8 {
            return switch (self) {
                .paren => '(',
                .bracket => '[',
                .brace => '{',
                .single_quote => '\'',
                .double_quote => '"',
                .multiline_quote => 0,
            };
        }

        pub fn closeChar(self: Open) u8 {
            return switch (self) {
                .paren => ')',
                .bracket => ']',
                .brace => '}',
                .single_quote => '\'',
                .double_quote => '"',
                .multiline_quote => 0,
            };
        }
    };
    pub const Close = Open;
    pub const InvalidType = InvalidTokenType;

    pub const Tag = TokenTag;
    pub const Spacing = SpacingToken;
    const Self = @This();
};

pub const InvalidToken = struct {
    const Self = @This();

    columns: SmallString.Range,
    type: InvalidTokenType,

    pub fn equals(self: Self, other: Self) bool {
        return self.columns.equals(other.columns) and self.type == other.type;
    }

    pub fn expectEquals(self: Self, other: Self) !void {
        try std.testing.expectEqual(other.type, self.type);
        try self.columns.expectEquals(other.columns);
    }

    pub fn expectNotEquals(self: Self, other: Self) !void {
        try std.testing.expect(!self.equals(other));
    }
};

const InvalidTokenType = enum {
    const Self = @This();

    operator,
    expected_close_paren,
    expected_close_bracket,
    expected_close_brace,
    expected_single_quote,
    expected_double_quote,
    unexpected_close, // there was a close with no open paren/brace/bracket
    number,
    too_many_commas,
    midline_comment,

    pub fn error_message(self: Self) []const u8 {
        return switch (self) {
            .operator => "invalid operator",
            // Had a `(` somewhere that was closed by something else...
            .expected_close_paren => "expected `)`",
            // Had a `[` somewhere that was closed by something else...
            .expected_close_bracket => "expected `]`",
            // Had a `{` somewhere that was closed by something else...
            .expected_close_brace => "expected `}`",
            // Had a `'` somewhere that was closed by something else...
            .expected_single_quote => "expected closing `'` by end of line",
            // Had a `"` somewhere that was closed by something else...
            .expected_double_quote => "expected closing `\"` by end of line",
            // Had a close that didn't have a corresponding open
            .unexpected_close => "no corresponding open",
            .number => "invalid number",
            .too_many_commas => "too many commas",
            .midline_comment => "midline comment should end this line",
        };
    }

    pub fn expected_close(for_open: Token.Open) Self {
        std.debug.assert(for_open != Token.Open.multiline_quote);
        return @enumFromInt(@intFromEnum(Self.expected_close_paren) + @intFromEnum(for_open));
    }
};

pub const SpacingToken = struct {
    const Self = @This();

    absolute: u16,
    relative: u16,
    line: u32,

    pub inline fn isNewline(self: Self) bool {
        return self.absolute == self.relative;
    }

    pub fn getNewlineIndex(self: Self) ?u32 {
        return if (self.isNewline()) self.line else null;
    }

    /// Returns the tab for a newline token, or null.
    pub fn getNewlineTab(self: Self) ?u16 {
        return if (self.isNewline()) self.absolute else null;
    }

    pub fn equals(self: Self, other: Self) bool {
        return self.absolute == other.absolute and self.relative == other.relative and self.line == other.line;
    }

    pub fn expectEquals(self: Self, other: Self) !void {
        try std.testing.expectEqual(other.absolute, self.absolute);
        try std.testing.expectEqual(other.relative, self.relative);
        try std.testing.expectEqual(other.line, self.line);
    }

    pub fn expectNotEquals(self: Self, other: Self) !void {
        try std.testing.expect(!self.equals(other));
    }
};

test "Token size is correct" {
    try std.testing.expectEqual(8 * 3, @sizeOf(Token));
}

test "invalid token" {
    try std.testing.expectEqual(InvalidTokenType.expected_close_paren, InvalidTokenType.expected_close(Token.Open.paren));
    try std.testing.expectEqual(InvalidTokenType.expected_close_bracket, InvalidTokenType.expected_close(Token.Open.bracket));
    try std.testing.expectEqual(InvalidTokenType.expected_close_brace, InvalidTokenType.expected_close(Token.Open.brace));
}

test "valid operator tokens" {
    const OwnedSmalls = @import("owned_list.zig").OwnedList(SmallString);
    var operators = try OwnedSmalls.of(&[_]SmallString{
        SmallString.noAlloc("~"),
        SmallString.noAlloc("++"),
        SmallString.noAlloc("--"),
        SmallString.noAlloc("="),
        SmallString.noAlloc("=="),
        SmallString.noAlloc("<"),
        SmallString.noAlloc("<="),
        SmallString.noAlloc(">"),
        SmallString.noAlloc(">="),
        SmallString.noAlloc("+"),
        SmallString.noAlloc("+="),
        SmallString.noAlloc("-"),
        SmallString.noAlloc("-="),
        SmallString.noAlloc("*"),
        SmallString.noAlloc("*="),
        SmallString.noAlloc("**"),
        SmallString.noAlloc("**="),
        SmallString.noAlloc("^"),
        SmallString.noAlloc("^="),
        SmallString.noAlloc("/"),
        SmallString.noAlloc("/="),
        SmallString.noAlloc("//"),
        SmallString.noAlloc("//="),
        SmallString.noAlloc("%"),
        SmallString.noAlloc("%="),
        SmallString.noAlloc("%%"),
        SmallString.noAlloc("%%="),
        SmallString.noAlloc("?"),
        SmallString.noAlloc("??"),
        SmallString.noAlloc("??="),
        SmallString.noAlloc("!"),
        SmallString.noAlloc("!!"),
        SmallString.noAlloc("!="),
        SmallString.noAlloc(":"),
        SmallString.noAlloc(";"),
        SmallString.noAlloc("."),
        SmallString.noAlloc(","),
        SmallString.noAlloc("&&"),
        SmallString.noAlloc("&&="),
        SmallString.noAlloc("||"),
        SmallString.noAlloc("||="),
        SmallString.noAlloc("&"),
        SmallString.noAlloc("&="),
        SmallString.noAlloc("|"),
        SmallString.noAlloc("|="),
        SmallString.noAlloc("><"),
        SmallString.noAlloc("><="),
        SmallString.noAlloc("<>"),
        SmallString.noAlloc("<<"),
        SmallString.noAlloc("<<="),
        SmallString.noAlloc(">>"),
        SmallString.noAlloc(">>="),
        SmallString.noAlloc("$"),
        SmallString.noAlloc("$$"),
        SmallString.noAlloc("$$$"),
        SmallString.noAlloc("$$$$"),
        SmallString.noAlloc("$$$$$"),
        SmallString.noAlloc("$$$$$$"),
        SmallString.noAlloc("$$$$$$$"),
        SmallString.noAlloc("$$$$$$$$"),
        SmallString.noAlloc("::"),
        SmallString.noAlloc(";;"),
        SmallString.noAlloc(".."),
        SmallString.noAlloc(";:"),
        SmallString.noAlloc(":;"),
        SmallString.noAlloc(";."),
        SmallString.noAlloc(".;"),
        SmallString.noAlloc(":."),
        SmallString.noAlloc(".:"),
        SmallString.noAlloc(":;."),
        SmallString.noAlloc(";:."),
        SmallString.noAlloc(":.;"),
        SmallString.noAlloc(";.:"),
        SmallString.noAlloc(".:;"),
        SmallString.noAlloc(".;:"),
    });
    defer operators.deinit();

    for (operators.items()) |string_operator| {
        const operator = Operator.init(string_operator.slice());
        if (operator == .none) {
            std.debug.print("expected {s} to be a valid operator\n", .{string_operator.slice()});
            return common.Error.unknown;
        }
        try operator.string().expectEquals(string_operator);
    }
}

test "invalid operator tokens" {
    const OwnedSmalls = @import("owned_list.zig").OwnedList(SmallString);
    var operators = try OwnedSmalls.of(&[_]SmallString{
        SmallString.noAlloc("=?"),
        SmallString.noAlloc("+++"),
        SmallString.noAlloc("+-+"),
        SmallString.noAlloc("/|\\"),
    });
    defer operators.deinit();

    for (operators.items()) |string_operator| {
        const operator = Operator.init(string_operator.slice());
        if (operator == .none) continue;

        std.debug.print("expected {s} to be an invalid operator, got {d}\n", .{ string_operator.slice(), operator.string().slice() });
    }
}

test "token equality" {
    const invalid = Token{ .invalid = .{ .columns = .{ .start = 3, .end = 8 }, .type = InvalidTokenType.operator } };
    try invalid.expectEquals(invalid);
    try std.testing.expect(invalid.equals(invalid));
    try invalid.expectNotEquals(Token{
        .invalid = .{
            .columns = .{ .start = 4, .end = 8 }, // different start
            .type = InvalidTokenType.operator,
        },
    });
    try invalid.expectNotEquals(Token{
        .invalid = .{
            .columns = .{ .start = 3, .end = 4 }, // different end
            .type = InvalidTokenType.operator,
        },
    });
    try invalid.expectNotEquals(Token{
        .invalid = .{
            .columns = .{ .start = 3, .end = 8 }, // same
            .type = InvalidTokenType.unexpected_close, // different
        },
    });

    const end: Token = .file_end;
    try std.testing.expect(end.equals(end));
    try end.expectEquals(end);

    const starts_upper = Token{ .starts_upper = try SmallString.init("Cabbage") };
    try starts_upper.expectNotEquals(end);
    try std.testing.expect(!starts_upper.equals(end));
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

    const spacing = Token{ .spacing = .{ .absolute = 123, .relative = 4, .line = 55 } };
    try spacing.expectNotEquals(end);
    try std.testing.expect(!spacing.equals(end));
    try spacing.expectNotEquals(starts_upper);
    try std.testing.expect(!spacing.equals(starts_upper));
    try spacing.expectNotEquals(starts_lower);
    try std.testing.expect(!spacing.equals(starts_lower));
    try spacing.expectEquals(spacing);
    try std.testing.expect(spacing.equals(spacing));
    try spacing.expectNotEquals(Token{ .spacing = .{ .absolute = 456, .relative = 4, .line = 55 } });
    try std.testing.expect(!spacing.equals(Token{ .spacing = .{ .absolute = 123, .relative = 5, .line = 55 } }));
    try spacing.expectNotEquals(Token{ .spacing = .{ .absolute = 123, .relative = 4, .line = 53 } });
    try std.testing.expect(!spacing.equals(Token{ .spacing = .{ .absolute = 123, .relative = 4, .line = 53 } }));

    const operator = Token{ .operator = .bitwise_xor };
    try operator.expectNotEquals(end);
    try std.testing.expect(!operator.equals(end));
    try operator.expectNotEquals(starts_upper);
    try std.testing.expect(!operator.equals(starts_upper));
    try operator.expectNotEquals(starts_lower);
    try std.testing.expect(!operator.equals(starts_lower));
    try operator.expectNotEquals(spacing);
    try std.testing.expect(!operator.equals(spacing));
    try operator.expectEquals(operator);
    try std.testing.expect(operator.equals(operator));
    try operator.expectNotEquals(Token{ .operator = .bitwise_flip });
    try std.testing.expect(!operator.equals(Token{ .operator = .bitwise_and }));
}
