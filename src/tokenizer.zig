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

pub const Token = union(TokenTag) {
    end: void,
    starts_upper: u64,
    starts_lower: u64,
};

pub const TokenTag = enum {
    end,
    starts_upper,
    starts_lower,
};

test "basic tokenizer functionality" {
    var tokenizer: Tokenizer = .{};
    try testing.expectEqual(tokenizer.next(), Token.end);
}
