const std = @import("std");

pub const tokenizer = @import("tokenizer.zig");
pub const string = @import("string.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var buffer: [1024]u8 = undefined;
    {
        const tmp = try std.fs.cwd().realpath(".", &buffer);
        std.debug.print("\nfrom dir {s}...\n", .{tmp});
    }

    for (1..args.len) |i| {
        const file_name = args[i];
        std.debug.print("\nreading {s}...\n", .{file_name});

        const file = try std.fs.cwd().openFile(file_name, .{});
        defer file.close();

        var buffered_reader = std.io.bufferedReader(file.reader());
        var file_stream = buffered_reader.reader();

        while (try file_stream.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
            std.debug.print("got line: {s}\n", .{line});
        }
    }
}

test ".. range" {
    var last_index: usize = 0;
    for (1..4) |i| {
        last_index = i;
    }
    try std.testing.expectEqual(last_index, 3);
}

test "other dependencies (import using pub)" {
    std.testing.refAllDecls(@This());
}
