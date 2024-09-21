const common = @import("common.zig");
const Node = @import("node.zig").Node;
const Parser = @import("parser.zig").Parser;
const SmallString = @import("string.zig").Small;

const std = @import("std");

// General test structure:
// Organize by topic but within each topic put easier tests near the end.
// This makes easy tests fail last, and they are easier to debug
// if they are at the end of the input.

test "parser return is an operator" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
        common.debugPrint("# nodes:\n", parser.nodes);
    }
    const file_slice = [_][]const u8{
        "5 + return 3 * 4 - 7",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 9, .next = 0 } },
        Node{ .atomic_token = 1 }, // 5
        Node{ .prefix = .{ .operator = .op_return, .node = 8 } },
        Node{ .atomic_token = 7 }, // 3
        // [5]:
        Node{ .atomic_token = 11 }, // 4
        Node{ .binary = .{ .operator = .op_multiply, .left = 4, .right = 5 } },
        Node{ .atomic_token = 15 }, // 7
        Node{ .binary = .{ .operator = .op_minus, .left = 6, .right = 7 } },
        Node{ .binary = .{ .operator = .op_plus, .left = 2, .right = 3 } },
        // [10]:
        .end,
    });
    // No tampering done with the file, i.e., no errors.
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}
