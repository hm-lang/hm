const SmallString = @import("string.zig").Small;
const owned_list = @import("owned_list.zig");

const std = @import("std");
const testing = std.testing;

const OwnedSmalls = owned_list.OwnedList(SmallString);

pub const TestWriterData = struct {
    buffer: [65535]u8 = undefined,
    current_buffer_offset: usize = 0,
    lines: OwnedSmalls = OwnedSmalls.init(),
};

pub const TestWriter = struct {
    const Self = @This();
    pub const Error = error{
        out_of_memory,
        line_too_long,
    };

    data: *TestWriterData,

    pub fn init(my_data: *TestWriterData) TestWriter {
        return TestWriter{ .data = my_data };
    }

    pub fn print(self: Self, comptime format: []const u8, args: anytype) !void {
        return std.fmt.format(self, format, args);
    }

    pub fn writeAll(self: Self, chars: []const u8) Error!void {
        for (chars) |char| {
            if (char == '\n') {
                const line = try self.debufferLine();
                self.data.lines.append(line) catch return Error.out_of_memory;
            } else if (self.data.current_buffer_offset < self.data.buffer.len) {
                self.data.buffer[self.data.current_buffer_offset] = char;
                self.data.current_buffer_offset += 1;
            } else {
                return Error.line_too_long;
            }
        }
    }

    pub fn writeBytesNTimes(self: Self, bytes: []const u8, n: usize) Error!void {
        for (0..n) |i| {
            _ = i;
            try self.writeAll(bytes);
        }
    }

    fn debufferLine(self: Self) Error!SmallString {
        const line = SmallString.init(self.data.buffer[0..self.data.current_buffer_offset]) catch return Error.out_of_memory;
        self.data.current_buffer_offset = 0;
        return line;
    }

    fn pullLines(self: *Self) OwnedSmalls {
        const new_lines = OwnedSmalls.init();
        const old_lines = self.data.lines;
        self.data.lines = new_lines;
        return old_lines;
    }
};

test "can get lines printed to stdout" {
    const common = @import("common.zig");

    try common.stdout.print("oh yes, ", .{});
    try common.stdout.print("oh no\n", .{});
    try common.stdout.print("two in\none print\n", .{});

    var lines = common.stdout.pullLines();
    defer lines.deinit();
    try lines.expectEqualsSlice(&[_]SmallString{
        SmallString.noAlloc("oh yes, oh no"),
        SmallString.noAlloc("two in"),
        SmallString.noAlloc("one print"),
    });
}
