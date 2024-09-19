const common = @import("common.zig");
const Operator = @import("operator.zig").Operator;
const Operation = Operator.Operation;
const Token = @import("token.zig").Token;

const std = @import("std");

const UntilTag = enum {
    deindent,
    precedence_strong_as,
    precedence_weaker_than,
    close,
};

/// Necessary for prefix operations.
pub const Until = union(UntilTag) {
    deindent: void,
    precedence_strong_as: u8,
    precedence_weaker_than: u8,
    close: Token.Close,

    pub const file_end: Self = .{ .close = Token.Close.none };

    pub inline fn nextBlockEnds() Self {
        return .deindent;
    }

    /// Will keep going until we get to operators that are not as strong.
    pub fn operatorWeakerThan(operator: Operator) Self {
        const operation = Operation{ .operator = operator, .type = .prefix };
        return .{ .precedence_weaker_than = operation.precedence(Operation.Compare.on_left) };
    }

    pub fn operatorAsWeakAs(operator: Operator) Self {
        const operation = Operation{ .operator = operator, .type = .prefix };
        // TODO: should this be + or - 1??  double check.
        return .{ .precedence_weaker_than = operation.precedence(Operation.Compare.on_left) - 1 };
    }

    pub fn closing(open: Token.Open) Self {
        return .{ .close = open };
    }

    pub fn shouldBreakBeforeOperation(self: Self, on_right: Operation) bool {
        // This is confusingly a double negative.  `shouldBreakBeforeOperation` is negative,
        // and lower numerical precedence is higher in priority.
        // TODO: invert operator precedence so high = high priority
        switch (self) {
            .precedence_strong_as => |left_precedence| {
                const right_precedence = on_right.precedence(Operation.Compare.on_right);
                return left_precedence >= right_precedence;
            },
            .precedence_weaker_than => |left_precedence| {
                const right_precedence = on_right.precedence(Operation.Compare.on_right);
                return left_precedence < right_precedence;
            },
            else => return false,
        }
    }

    pub fn shouldBreakAtClose(self: Self, close: Token.Close) bool {
        return switch (self) {
            .close => |self_close| self_close == close,
            else => false,
        };
    }

    pub fn shouldBreakAtDeindent(self: Self) bool {
        common.debugPrint("checking if we should break at deindent for ", self);
        return switch (self) {
            .deindent => true,
            else => false,
        };
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
            .deindent => try writer.print("Until.nextBlockEnds()", .{}),
            .precedence_strong_as => |precedence| {
                try writer.print("Until{{ .precedence_strong_as = {d} }}", .{precedence});
            },
            .precedence_weaker_than => |precedence| {
                try writer.print("Until{{ .precedence_weaker_than = {d} }}", .{precedence});
            },
            .close => |close| {
                try writer.print("Until.closing(.", .{});
                try close.print(writer);
                try writer.print(")", .{});
            },
        }
    }

    pub const Tag = UntilTag;
    const Self = @This();
};
