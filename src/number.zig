const std = @import("std");

const NumberTag = enum {
    /// Signed ints
    ti8,
    ti16,
    ti32,
    ti64,
    /// Unsigned ints
    tu8,
    tu16,
    tu32,
    tu64,
    /// Floats
    tf32,
    tf64,
    // TODO: tint, // bigint
};

pub const Number = union(NumberTag) {
    ti8: i8,
    ti16: i16,
    ti32: i32,
    ti64: i64,
    tu8: u8,
    tu16: u16,
    tu32: u32,
    tu64: u64,
    tf32: f32,
    tf64: f64,

    pub const Tag = NumberTag;
    const Self = @This();
};

test "number size" {
    try std.testing.expectEqual(2 * 8, @sizeOf(Number));
}
