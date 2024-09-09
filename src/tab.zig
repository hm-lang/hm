const std = @import("std");

pub fn Tabbed(comptime T: type) type {
    return struct {
        enclosed_by: Open = .none,
        /// Tab to the `enclosed_by` character.
        /// E.g., here the `external` tab is 4 and the `internal` tab is 12.
        /// &|    {        InternalData ...
        /// &|    }
        external: u16,
        /// Tab past the `enclosed_by` character to the actual content.
        // TODO: do we even need this??
        internal: u16,
        data: T,

        pub fn printLine(self: Self, writer: anytype) !void {
            try self.print(writer);
            try writer.print("\n", .{});
        }

        pub fn print(self: Self, writer: anytype) !void {
            if (self.open == .none) {
                try writer.print("Tab{{ .external = {d}, .internal = {d}, .data = ", .{ self.external, self.internal });
            } else {
                try writer.print("Tab{{ .open = .", .{});
                try open.print(writer);
                try writer.print(", .external = {d}, .internal = {d}, .data = ", .{ self.start, self.internal });
            }
            if (std.meta.hasMethod(T, "print")) {
                try self.data.print(writer);
            } else {
                try writer.print("{d}", .{self.data});
            }
            try writer.print(" }}", .{});
        }

        pub fn equals(a: Self, b: Self) bool {
            return a.internal == b.internal and a.external == b.external;
        }

        pub fn expectEquals(a: Self, b: Self) !void {
            try std.testing.expect(a.equals(b));
        }

        pub const Open = TabOpen;
        pub const Close = TabOpen;

        const Self = @This();
    };
}

const TabOpen = enum {
    none,
    paren,
    bracket,
    brace,

    pub fn slice(self: Self) []const u8 {
        return switch (self) {
            .none => "none",
            .paren => "paren",
            .bracket => "bracket",
            .brace => "brace",
        };
    }

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        try writer.print("{s}", .{self.slice()});
    }

    const Self = @This();
};
