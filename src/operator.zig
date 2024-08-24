const SmallString = @import("string.zig").Small;

const std = @import("std");

pub const Operator = enum(u64) {
    none = 0,
    new_generic = SmallString.as64("~"),
    increment = SmallString.as64("++"),
    decrement = SmallString.as64("--"),
    assign = SmallString.as64("="),
    equals = SmallString.as64("=="),
    less_than = SmallString.as64("<"),
    less_equal = SmallString.as64("<="),
    greater_than = SmallString.as64(">"),
    greater_equal = SmallString.as64(">="),
    plus = SmallString.as64("+"),
    plus_assign = SmallString.as64("+="),
    minus = SmallString.as64("-"),
    minus_assign = SmallString.as64("-="),
    multiply = SmallString.as64("*"),
    multiply_assign = SmallString.as64("*="),
    exponentiate1 = SmallString.as64("**"),
    exponentiate1_assign = SmallString.as64("**="),
    exponentiate = SmallString.as64("^"),
    exponentiate_assign = SmallString.as64("^="),
    divide = SmallString.as64("/"),
    divide_assign = SmallString.as64("/="),
    integer_divide = SmallString.as64("//"),
    integer_divide_assign = SmallString.as64("//="),
    modulus = SmallString.as64("%"),
    modulus_assign = SmallString.as64("%="),
    remainder = SmallString.as64("%%"),
    remainder_assign = SmallString.as64("%%="),
    nullify = SmallString.as64("?"),
    nullish_or = SmallString.as64("??"),
    nullish_or_assign = SmallString.as64("??="),
    not = SmallString.as64("!"),
    not_not = SmallString.as64("!!"),
    not_equal = SmallString.as64("!="),
    declare_readonly = SmallString.as64(":"),
    declare_writable = SmallString.as64(";"),
    declare_temporary = SmallString.as64("."),
    comma = SmallString.as64(","),
    logical_and = SmallString.as64("&&"),
    logical_and_assign = SmallString.as64("&&="),
    logical_or = SmallString.as64("||"),
    logical_or_assign = SmallString.as64("||="),
    bitwise_and = SmallString.as64("&"),
    bitwise_and_assign = SmallString.as64("&="),
    bitwise_or = SmallString.as64("|"),
    bitwise_or_assign = SmallString.as64("|="),
    bitwise_xor = SmallString.as64("><"),
    bitwise_xor_assign = SmallString.as64("><="),
    bitwise_flip = SmallString.as64("<>"),
    bitshift_left = SmallString.as64("<<"),
    bitshift_left_assign = SmallString.as64("<<="),
    bitshift_right = SmallString.as64(">>"),
    bitshift_right_assign = SmallString.as64(">>="),
    lambda1 = SmallString.as64("$"),
    lambda2 = SmallString.as64("$$"),
    lambda3 = SmallString.as64("$$$"),
    lambda4 = SmallString.as64("$$$$"),
    lambda5 = SmallString.as64("$$$$$"),
    lambda6 = SmallString.as64("$$$$$$"),
    lambda7 = SmallString.as64("$$$$$$$"),
    lambda8 = SmallString.as64("$$$$$$$$"),
    implicit_member_access = SmallString.as64(" "),
    readonly_member_access = SmallString.as64("::"),
    writable_member_access = SmallString.as64(";;"),
    temporary_member_access = SmallString.as64(".."),
    wr_member_access = SmallString.as64(";:"),
    rw_member_access = SmallString.as64(":;"),
    wt_member_access = SmallString.as64(";."),
    tw_member_access = SmallString.as64(".;"),
    rt_member_access = SmallString.as64(":."),
    tr_member_access = SmallString.as64(".:"),
    rwt_member_access = SmallString.as64(":;."),
    wrt_member_access = SmallString.as64(";:."),
    rtw_member_access = SmallString.as64(":.;"),
    wtr_member_access = SmallString.as64(";.:"),
    trw_member_access = SmallString.as64(".:;"),
    twr_member_access = SmallString.as64(".;:"),

    /// Returns 0 if `buffer` is an invalid operator, otherwise
    /// the numerical value of the operator (see `SmallString.as64`).
    pub fn init(buffer: []const u8) Self {
        if (buffer.len > 8) {
            return .none;
        }
        const small = SmallString.init(buffer) catch unreachable;
        const value64 = small.big64() catch unreachable;
        return init64(value64);
    }

    pub fn init64(value: u64) Self {
        return @enumFromInt(switch (value) {
            SmallString.as64("~"),
            SmallString.as64("++"),
            SmallString.as64("--"),
            SmallString.as64("="),
            SmallString.as64("=="),
            SmallString.as64("<"),
            SmallString.as64("<="),
            SmallString.as64(">"),
            SmallString.as64(">="),
            SmallString.as64("+"),
            SmallString.as64("+="),
            SmallString.as64("-"),
            SmallString.as64("-="),
            SmallString.as64("*"),
            SmallString.as64("*="),
            SmallString.as64("**"),
            SmallString.as64("**="),
            SmallString.as64("^"),
            SmallString.as64("^="),
            SmallString.as64("/"),
            SmallString.as64("/="),
            SmallString.as64("//"),
            SmallString.as64("//="),
            SmallString.as64("%"),
            SmallString.as64("%="),
            SmallString.as64("%%"),
            SmallString.as64("%%="),
            SmallString.as64("?"),
            SmallString.as64("??"),
            SmallString.as64("??="),
            SmallString.as64("!"),
            SmallString.as64("!!"),
            SmallString.as64("!="),
            SmallString.as64(":"),
            SmallString.as64(";"),
            SmallString.as64("."),
            SmallString.as64(","),
            SmallString.as64("&&"),
            SmallString.as64("&&="),
            SmallString.as64("||"),
            SmallString.as64("||="),
            SmallString.as64("&"),
            SmallString.as64("&="),
            SmallString.as64("|"),
            SmallString.as64("|="),
            SmallString.as64("><"),
            SmallString.as64("><="),
            SmallString.as64("<>"),
            SmallString.as64("<<"),
            SmallString.as64("<<="),
            SmallString.as64(">>"),
            SmallString.as64(">>="),
            SmallString.as64("$"),
            SmallString.as64("$$"),
            SmallString.as64("$$$"),
            SmallString.as64("$$$$"),
            SmallString.as64("$$$$$"),
            SmallString.as64("$$$$$$"),
            SmallString.as64("$$$$$$$"),
            SmallString.as64("$$$$$$$$"),
            SmallString.as64(" "),
            SmallString.as64("::"),
            SmallString.as64(";;"),
            SmallString.as64(".."),
            SmallString.as64(";:"),
            SmallString.as64(":;"),
            SmallString.as64(";."),
            SmallString.as64(".;"),
            SmallString.as64(":."),
            SmallString.as64(".:"),
            SmallString.as64(":;."),
            SmallString.as64(";:."),
            SmallString.as64(":.;"),
            SmallString.as64(";.:"),
            SmallString.as64(".:;"),
            SmallString.as64(".;:"),
            => value,
            // We also convert some unnecessarily verbose operators.
            SmallString.as64(":=") => comptime SmallString.as64(":"),
            SmallString.as64(";=") => comptime SmallString.as64(";"),
            else => 0,
        });
    }

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        return try switch (self) {
            .none => writer.print("Operator.none", .{}),
            .new_generic => writer.print("Operator.new_generic", .{}),
            .increment => writer.print("Operator.increment", .{}),
            .decrement => writer.print("Operator.decrement", .{}),
            .assign => writer.print("Operator.assign", .{}),
            .equals => writer.print("Operator.equals", .{}),
            .less_than => writer.print("Operator.less_than", .{}),
            .less_equal => writer.print("Operator.less_equal", .{}),
            .greater_than => writer.print("Operator.greater_than", .{}),
            .greater_equal => writer.print("Operator.greater_equal", .{}),
            .plus => writer.print("Operator.plus", .{}),
            .plus_assign => writer.print("Operator.plus_assign", .{}),
            .minus => writer.print("Operator.minus", .{}),
            .minus_assign => writer.print("Operator.minus_assign", .{}),
            .multiply => writer.print("Operator.multiply", .{}),
            .multiply_assign => writer.print("Operator.multiply_assign", .{}),
            .exponentiate1 => writer.print("Operator.exponentiate1", .{}),
            .exponentiate1_assign => writer.print("Operator.exponentiate1_assign", .{}),
            .exponentiate => writer.print("Operator.exponentiate", .{}),
            .exponentiate_assign => writer.print("Operator.exponentiate_assign", .{}),
            .divide => writer.print("Operator.divide", .{}),
            .divide_assign => writer.print("Operator.divide_assign", .{}),
            .integer_divide => writer.print("Operator.integer_divide", .{}),
            .integer_divide_assign => writer.print("Operator.integer_divide_assign", .{}),
            .modulus => writer.print("Operator.modulus", .{}),
            .modulus_assign => writer.print("Operator.modulus_assign", .{}),
            .remainder => writer.print("Operator.remainder", .{}),
            .remainder_assign => writer.print("Operator.remainder_assign", .{}),
            .nullify => writer.print("Operator.nullify", .{}),
            .nullish_or => writer.print("Operator.nullish_or", .{}),
            .nullish_or_assign => writer.print("Operator.nullish_or_assign", .{}),
            .not => writer.print("Operator.not", .{}),
            .not_not => writer.print("Operator.not_not", .{}),
            .not_equal => writer.print("Operator.not_equal", .{}),
            .declare_readonly => writer.print("Operator.declare_readonly", .{}),
            .declare_writable => writer.print("Operator.declare_writable", .{}),
            .declare_temporary => writer.print("Operator.declare_temporary", .{}),
            .comma => writer.print("Operator.comma", .{}),
            .logical_and => writer.print("Operator.logical_and", .{}),
            .logical_and_assign => writer.print("Operator.logical_and_assign", .{}),
            .logical_or => writer.print("Operator.logical_or", .{}),
            .logical_or_assign => writer.print("Operator.logical_or_assign", .{}),
            .bitwise_and => writer.print("Operator.bitwise_and", .{}),
            .bitwise_and_assign => writer.print("Operator.bitwise_and_assign", .{}),
            .bitwise_or => writer.print("Operator.bitwise_or", .{}),
            .bitwise_or_assign => writer.print("Operator.bitwise_or_assign", .{}),
            .bitwise_xor => writer.print("Operator.bitwise_xor", .{}),
            .bitwise_xor_assign => writer.print("Operator.bitwise_xor_assign", .{}),
            .bitwise_flip => writer.print("Operator.bitwise_flip", .{}),
            .bitshift_left => writer.print("Operator.bitshift_left", .{}),
            .bitshift_left_assign => writer.print("Operator.bitshift_left_assign", .{}),
            .bitshift_right => writer.print("Operator.bitshift_right", .{}),
            .bitshift_right_assign => writer.print("Operator.bitshift_right_assign", .{}),
            .lambda1 => writer.print("Operator.lambda1", .{}),
            .lambda2 => writer.print("Operator.lambda2", .{}),
            .lambda3 => writer.print("Operator.lambda3", .{}),
            .lambda4 => writer.print("Operator.lambda4", .{}),
            .lambda5 => writer.print("Operator.lambda5", .{}),
            .lambda6 => writer.print("Operator.lambda6", .{}),
            .lambda7 => writer.print("Operator.lambda7", .{}),
            .lambda8 => writer.print("Operator.lambda8", .{}),
            .implicit_member_access => writer.print("Operator.implicit_member_access", .{}),
            .readonly_member_access => writer.print("Operator.readonly_member_access", .{}),
            .writable_member_access => writer.print("Operator.writable_member_access", .{}),
            .temporary_member_access => writer.print("Operator.temporary_member_access", .{}),
            .wr_member_access => writer.print("Operator.wr_member_access", .{}),
            .rw_member_access => writer.print("Operator.rw_member_access", .{}),
            .wt_member_access => writer.print("Operator.wt_member_access", .{}),
            .tw_member_access => writer.print("Operator.tw_member_access", .{}),
            .rt_member_access => writer.print("Operator.rt_member_access", .{}),
            .tr_member_access => writer.print("Operator.tr_member_access", .{}),
            .rwt_member_access => writer.print("Operator.rwt_member_access", .{}),
            .wrt_member_access => writer.print("Operator.wrt_member_access", .{}),
            .rtw_member_access => writer.print("Operator.rtw_member_access", .{}),
            .wtr_member_access => writer.print("Operator.wtr_member_access", .{}),
            .trw_member_access => writer.print("Operator.trw_member_access", .{}),
            .twr_member_access => writer.print("Operator.twr_member_access", .{}),
        };
    }

    pub fn string(self: Self) SmallString {
        return SmallString.init64(@intFromEnum(self));
    }

    pub inline fn to64(self: Self) u64 {
        return @intFromEnum(self);
    }
    pub fn isPrefixable(self: Self) bool {
        return switch (self) {
            .new_generic,
            .not,
            .not_not,
            .nullify,
            positive,
            .increment,
            negate,
            .decrement,
            .lambda1,
            .lambda2,
            .lambda3,
            .lambda4,
            .lambda5,
            .lambda6,
            .lambda7,
            .lambda8,
            .readonly_member_access,
            .writable_member_access,
            .temporary_member_access,
            .wr_member_access,
            .rw_member_access,
            .wt_member_access,
            .tw_member_access,
            .rt_member_access,
            .tr_member_access,
            .rwt_member_access,
            .wrt_member_access,
            .rtw_member_access,
            .wtr_member_access,
            .trw_member_access,
            .twr_member_access,
            => true,
            else => false,
        };
    }

    pub fn isPostfixable(self: Self) bool {
        return switch (self) {
            moot,
            cancel_if_null,
            .increment,
            .decrement,
            => true,
            else => false,
        };
    }

    pub fn isInfixable(self: Self) bool {
        return switch (self) {
            .assign,
            .equals,
            .less_than,
            .less_equal,
            .greater_than,
            .greater_equal,
            .plus,
            .plus_assign,
            .minus,
            .minus_assign,
            .multiply,
            .multiply_assign,
            .exponentiate1,
            .exponentiate1_assign,
            .exponentiate,
            .exponentiate_assign,
            .divide,
            .divide_assign,
            .integer_divide,
            .integer_divide_assign,
            .modulus,
            .modulus_assign,
            .remainder,
            .remainder_assign,
            .nullish_or,
            .nullish_or_assign,
            .not_equal,
            .declare_readonly,
            .declare_writable,
            .comma,
            .implicit_member_access,
            .readonly_member_access,
            .writable_member_access,
            .temporary_member_access,
            .wr_member_access,
            .rw_member_access,
            .wt_member_access,
            .tw_member_access,
            .rt_member_access,
            .tr_member_access,
            .rwt_member_access,
            .wrt_member_access,
            .rtw_member_access,
            .wtr_member_access,
            .trw_member_access,
            .twr_member_access,
            .logical_and,
            .logical_and_assign,
            .logical_or,
            .logical_or_assign,
            .bitwise_and,
            .bitwise_and_assign,
            .bitwise_or,
            .bitwise_or_assign,
            .bitwise_xor,
            .bitwise_xor_assign,
            .bitwise_flip,
            .bitshift_left,
            .bitshift_left_assign,
            .bitshift_right,
            .bitshift_right_assign,
            => true,
            else => false,
        };
    }

    pub const positive: Self = .plus;
    pub const negate: Self = .minus;
    pub const negative: Self = .minus;
    pub const moot: Self = .not;
    pub const cancel_if_null: Self = .nullify;
    const Self = @This();
};

const OperationType = enum {
    none,
    prefix,
    infix,
    postfix,

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        return switch (self) {
            .none => writer.print("Operation.Type.none", .{}),
            .prefix => writer.print("Operation.Type.prefix", .{}),
            .infix => writer.print("Operation.Type.infix", .{}),
            .postfix => writer.print("Operation.Type.postfix", .{}),
        };
    }

    const Self = @This();
};

pub const Operation = struct {
    operator: Operator = .none,
    type: Type = Type.none,

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        try writer.print("Operation{{ .operator = ", .{});
        try self.operator.print(writer);
        try writer.print(", .type = ", .{});
        try self.type.print(writer);
        try writer.print("}}", .{});
    }

    pub fn isPrefix(self: Self) bool {
        return self.type == Type.prefix;
    }
    pub fn isInfix(self: Self) bool {
        return self.type == Type.infix;
    }
    pub fn isPostfix(self: Self) bool {
        return self.type == Type.postfix;
    }

    pub fn precedence(self: Self, compare: Compare) u8 {
        const rtl: u8 = @intFromEnum(compare);
        return switch (self.operator) {
            .none => 1,
            .new_generic => 10,
            .increment, .decrement => 30,
            .assign => 110,
            .equals => 90,
            .less_than => 90,
            .less_equal => 90,
            .greater_than => 90,
            .greater_equal => 90,
            .plus => if (self.isInfix()) 70 else 40 - rtl,
            .plus_assign => 110,
            .minus => if (self.isInfix()) 70 else 40 - rtl,
            .minus_assign => 110,
            .multiply => 60,
            .multiply_assign => 110,
            .exponentiate1 => 30,
            .exponentiate1_assign => 110,
            .exponentiate => 30,
            .exponentiate_assign => 110,
            .divide => 60,
            .divide_assign => 110,
            .integer_divide => 60,
            .integer_divide_assign => 110,
            .modulus => 60,
            .modulus_assign => 110,
            .remainder => 60,
            .remainder_assign => 110,
            .nullify => 20,
            .nullish_or => 20,
            .nullish_or_assign => 110,
            .not => 40 - rtl,
            .not_not => 40 - rtl,
            .not_equal => 110,
            .declare_readonly => 110,
            .declare_writable => 110,
            .declare_temporary => 110,
            .comma => 120,
            .logical_and => 80,
            .logical_and_assign => 110,
            .logical_or => 80,
            .logical_or_assign => 110,
            .bitwise_and => 70,
            .bitwise_and_assign => 110,
            .bitwise_or => 70,
            .bitwise_or_assign => 110,
            .bitwise_xor => 80,
            .bitwise_xor_assign => 110,
            .bitwise_flip => 40 - rtl,
            .bitshift_left => 50,
            .bitshift_left_assign => 110,
            .bitshift_right => 50,
            .bitshift_right_assign => 110,
            .lambda1 => 20,
            .lambda2 => 20,
            .lambda3 => 20,
            .lambda4 => 20,
            .lambda5 => 20,
            .lambda6 => 20,
            .lambda7 => 20,
            .lambda8 => 20,
            .implicit_member_access => 20,
            .readonly_member_access => 20,
            .writable_member_access => 20,
            .temporary_member_access => 20,
            .wr_member_access => 20,
            .rw_member_access => 20,
            .wt_member_access => 20,
            .tw_member_access => 20,
            .rt_member_access => 20,
            .tr_member_access => 20,
            .rwt_member_access => 20,
            .wrt_member_access => 20,
            .rtw_member_access => 20,
            .wtr_member_access => 20,
            .trw_member_access => 20,
            .twr_member_access => 20,
        };
    }

    pub const Compare = enum {
        on_left,
        on_right,
    };
    pub const Type = OperationType;
    const Self = @This();
};

test "rewritten operator tokens" {
    try std.testing.expectEqual(SmallString.as64(";"), Operator.init(";=").to64());
    try std.testing.expectEqual(SmallString.as64(":"), Operator.init(":=").to64());
}
