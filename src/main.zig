pub const common = @import("common.zig");
pub const DoNothing = @import("do_nothing.zig").DoNothing;
pub const File = @import("file.zig").File;
pub const Node = @import("node.zig").Node;
pub const Number = @import("number.zig").Number;
pub const operator_zig = @import("operator.zig");
pub const Parser = @import("parser.zig").Parser;
pub const Run = @import("run.zig").Run;
pub const RunContext = @import("run_context.zig").RunContext;
pub const SmallString = @import("string.zig").Small;
pub const testing = @import("testing.zig");
pub const Token = @import("token.zig").Token;
pub const Tokenizer = @import("tokenizer.zig").Tokenizer;
pub const Until = @import("until.zig").Until;

pub const parser_tests = @import("parser_tests.zig");
pub const parser_declare_tests = @import("parser_declare_tests.zig");
pub const parser_if_tests = @import("parser_if_tests.zig");
pub const parser_operations_tests = @import("parser_operations_tests.zig");
pub const parser_what_tests = @import("parser_what_tests.zig");
pub const parser_while_tests = @import("parser_while_tests.zig");

const std = @import("std");

pub fn main() !void {
    const args = try std.process.argsAlloc(common.allocator);
    defer std.process.argsFree(common.allocator, args);

    var buffer: [1024]u8 = undefined;
    {
        const tmp = try std.fs.cwd().realpath(".", &buffer);
        std.debug.print("\nfrom dir {s}...\n", .{tmp});
    }

    for (1..args.len) |i| {
        // This likely does one more allocation than I'd like to create `file.path`,
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
