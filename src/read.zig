const File = @import("file.zig").File;
const Vargs = @import("vargs.zig").Vargs;

const std = @import("std");

pub fn subcommand(vargs: *Vargs) !void {
    while (vargs.shift()) |arg| {
        var file = File{ .path = arg };
        defer file.deinit();
        std.debug.print("\nreading {s}...\n", .{file.path.slice()});

        try file.read();

        for (file.lines.items()) |line| {
            std.debug.print("got line: {s}\n", .{line.slice()});
        }
    }
}
