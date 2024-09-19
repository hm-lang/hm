const operator_zig = @import("operator.zig");
const Operator = operator_zig.Operator;
const Operation = operator_zig.Operation;
const Token = @import("token.zig").Token;

const std = @import("std");

const UntilTag = enum {
    precedence,
    close,
};

/// Necessary for prefix operations.
pub const Until = union(UntilTag) {
    precedence: u8,
    close: Token.Close,

    pub const file_end: Self = .{ .close = Token.Close.none };

    /// Will keep going until this prefix operator should win.
    pub fn prefix_strength_wins(operator: Operator) Self {
        const operation = Operation{ .operator = operator, .type = .prefix };
        return .{ .precedence = operation.precedence(Operation.Compare.on_left) };
    }

    pub fn closing(open: Token.Open) Self {
        return .{ .close = open };
    }

    pub fn shouldBreakBeforeOperation(self: Self, on_right: Operation) bool {
        switch (self) {
            .precedence => |left_precedence| {
                const right_precedence = on_right.precedence(Operation.Compare.on_right);
                return left_precedence < right_precedence;
            },
            else => return false,
        }
    }

    pub fn shouldBreakAtClose(self: Self, close: Token.Close) bool {
        switch (self) {
            .close => |self_close| {
                return self_close == close;
            },
            else => return false,
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

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        switch (self) {
            .precedence => |precedence| {
                try writer.print("Until{{ .precedence = {d} }}", .{precedence});
            },
        }
    }

    pub const Tag = UntilTag;
    const Self = @This();
};
