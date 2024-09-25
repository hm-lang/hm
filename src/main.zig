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
pub const Vargs = @import("vargs.zig").Vargs;

pub const parser_tests = @import("parser_tests.zig");
pub const parser_declare_tests = @import("parser_declare_tests.zig");
pub const parser_if_tests = @import("parser_if_tests.zig");
pub const parser_operations_tests = @import("parser_operations_tests.zig");
pub const parser_what_tests = @import("parser_what_tests.zig");
pub const parser_while_tests = @import("parser_while_tests.zig");

const std = @import("std");

pub fn main() !void {
    var buffer: [1024]u8 = undefined;
    {
        const tmp = try std.fs.cwd().realpath(".", &buffer);
        std.debug.print("\nreading from dir {s}...\n", .{tmp});
    }

    var vargs = try Vargs.init();
    const executable_name = vargs.shift() orelse {
        common.logError("expected to populate Vargs with executable name", .{});
        return common.Error.unknown;
    };

    const subcommand = vargs.shift() orelse {
        return logNeedSubcommand(MainError.no_subcommand, executable_name);
    };
    const subcommand64 = subcommand.big64() catch {
        logInvalidSubcommand(subcommand);
        return logNeedSubcommand(MainError.invalid_subcommand, executable_name);
    };
    switch (subcommand64) {
        SmallString.as64("read") => {},
        else => {
            logInvalidSubcommand(subcommand);
            return logNeedSubcommand(MainError.invalid_subcommand, executable_name);
        },
    }

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

const MainError = error{
    no_subcommand,
    invalid_subcommand,
};

fn logNeedSubcommand(e: MainError, executable_name: SmallString) MainError {
    common.logError(need_subcommand_error, .{executable_name.slice()});
    return e;
}

fn logInvalidSubcommand(subcommand: SmallString) void {
    common.logError("invalid subcommand: {s}\n", .{subcommand.slice()});
}

const need_subcommand_error =
    \\You should specify a subcommand like `read`.
    \\Usage: {s} [read] additional_arguments
    \\    read: additional_arguments are files you want to read.
;

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
