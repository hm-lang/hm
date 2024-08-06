const std = @import("std");

pub const tokenizer = @import("tokenizer.zig");
pub const SmallString = @import("string.zig").Small;
pub const File = @import("file.zig").File;

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
        // This possibly does one more allocation than I'd like to create `file.path`,
        // but I'd rather not redo the internals of `argsAlloc`.
        var file = File{ .path = try SmallString.init(args[i]) };
        defer file.deinit();
        std.debug.print("\nreading {s}...\n", .{file.path.slice()});

        try file.read();

        for (file.lines.items()) |line| {
            std.debug.print("got line: {s}\n", .{line.slice()});
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
