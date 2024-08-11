pub const SmallString = @import("string.zig").Small;
const common = @import("common.zig");

const std = @import("std");

pub const TokenTag = enum {
    invalid,
    end,
    newline,
    tab,
    starts_upper,
    starts_lower,
    number,
    operator,
    open,
    close,
};

pub const Token = union(TokenTag) {
    const Self = @This();

    pub const InvalidType = InvalidTokenType;
    pub const Open = enum {
        paren,
        bracket,
        brace,

        pub fn slice(self: Open) []const u8 {
            return switch (self) {
                .paren => "paren",
                .bracket => "bracket",
                .brace => "brace",
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
    };
    pub const Close = Open;

    invalid: InvalidToken,
    end: void,
    newline: u32,
    tab: u16,
    starts_upper: SmallString,
    starts_lower: SmallString,
    // We don't try to create a `dbl` or `int` here, just represent it faithfully for now.
    number: SmallString,
    operator: u64,
    open: Open,
    close: Close,

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

    pub fn countChars(self: Token) u16 {
        return switch (self) {
            .invalid => |invalid| invalid.columns.count(),
            .end => 0,
            .newline => 0,
            .tab => 4,
            .starts_upper => |string| string.count(),
            .starts_lower => |string| string.count(),
            .number => |string| string.count(),
            .operator => |operator| SmallString.init64(operator).count(),
            .open => 1,
            .close => 1,
        };
    }

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
            .end => {
                try writer.print(".end", .{});
            },
            .newline => |value| {
                try writer.print("Token{{ .newline = {d} }}", .{value});
            },
            .tab => |value| {
                try writer.print("Token{{ .tab = {d} }}", .{value});
            },
            .starts_upper => |string| {
                try writer.print("Token{{ .starts_upper = try SmallString.init(\"", .{});
                try string.print(writer);
                try writer.print("\")}}", .{});
            },
            .starts_lower => |string| {
                try writer.print("Token{{ .starts_lower = try SmallString.init(\"", .{});
                try string.print(writer);
                try writer.print("\")}}", .{});
            },
            .number => |number| {
                try writer.print("Token{{ .number = try SmallString.init(\"", .{});
                try number.print(writer);
                try writer.print("\")}}", .{});
            },
            .operator => |operator| {
                try writer.print("Token{{ .operator = SmallString.as64(\"", .{});
                try SmallString.init64(operator).print(writer);
                try writer.print("\")}}", .{});
            },
            .open => |open| {
                try writer.print("Token{{ .open = Token.Open.{s} }}", .{open.slice()});
            },
            .close => |close| {
                try writer.print("Token{{ .close = Token.Close.{s} }}", .{close.slice()});
            },
        }
    }

    pub fn openChar(open: Open) u8 {
        return switch (open) {
            Open.paren => '(',
            Open.bracket => '[',
            Open.brace => '{',
        };
    }

    pub fn closeChar(close: Close) u8 {
        return switch (close) {
            Close.paren => ')',
            Close.bracket => ']',
            Close.brace => '}',
        };
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
        errdefer {
            common.stderr.print("expected:\n", .{}) catch {};
            b.printLine(common.stderr) catch {};

            common.stderr.print("got:\n", .{}) catch {};
            a.printLine(common.stderr) catch {};
        }
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
        errdefer {
            common.stderr.print("expected NOT this, but got it:\n", .{}) catch {};
            a.printLine(common.stderr) catch {};
        }
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
    unexpected_close, // there was a close with no open paren/brace/bracket
    number,

    pub fn error_message(self: Self) []const u8 {
        return switch (self) {
            .operator => "invalid operator",
            // Had a `(` somewhere that was closed by something else...
            .expected_close_paren => "expected `)`",
            // Had a `[` somewhere that was closed by something else...
            .expected_close_bracket => "expected `]`",
            // Had a `{` somewhere that was closed by something else...
            .expected_close_brace => "expected `}`",
            // Had a close that didn't have a corresponding open
            .unexpected_close => "no corresponding open",
            .number => "invalid number",
        };
    }

    pub fn expected_close(for_open: Token.Open) Self {
        return @enumFromInt(@intFromEnum(Self.expected_close_paren) + @intFromEnum(for_open));
    }
};

test "invalid token" {
    try std.testing.expectEqual(InvalidTokenType.expected_close_paren, InvalidTokenType.expected_close(Token.Open.paren));
    try std.testing.expectEqual(InvalidTokenType.expected_close_bracket, InvalidTokenType.expected_close(Token.Open.bracket));
    try std.testing.expectEqual(InvalidTokenType.expected_close_brace, InvalidTokenType.expected_close(Token.Open.brace));
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
