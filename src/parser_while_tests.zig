const common = @import("common.zig");
const DoNothing = @import("do_nothing.zig").DoNothing;
const Node = @import("node.zig").Node;
const Parser = @import("parser.zig").Parser;

const std = @import("std");

test "parsing while/elif statements" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 10, .next = 0 } }, // root statement
        Node{ .atomic_token = 3 }, // Maybe_true
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 4 } }, // loop brace
        Node{ .statement = .{ .node = 5, .next = 0 } }, // loop statement
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } }, // loop indent
        Node{ .statement = .{ .node = 7, .next = 8 } }, // first statement in loop
        Node{ .atomic_token = 7 }, // Go_for_it33
        Node{ .statement = .{ .node = 9, .next = 0 } }, // second statement in loop
        Node{ .atomic_token = 9 }, // Go_for_it34
        // [10]:
        Node{ .while_loop = .{ .condition = 2, .loop_node = 3, .else_node = 19 } }, // while
        Node{ .atomic_token = 15 }, // Not_true
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 13 } }, // elif brace
        Node{ .statement = .{ .node = 14, .next = 0 } }, // elif statement
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 15 } }, // elif indent
        // [15]:
        Node{ .statement = .{ .node = 16, .next = 17 } }, // first statement in elif
        Node{ .callable_token = 19 }, // go_for_it22
        Node{ .statement = .{ .node = 18, .next = 0 } }, // second statement in elif
        Node{ .callable_token = 21 }, // go_for_it23
        Node{ .conditional = .{ .condition = 11, .if_node = 12, .else_node = 0 } }, // elif
        // [20]:
        .end,
    };
    {
        // Explicit brace, one-true-brace style
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "while Maybe_true {",
            "    Go_for_it33",
            "    Go_for_it34",
            "} elif Not_true {",
            "    go_for_it22",
            "    go_for_it23",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        // Explicit brace, Horstmann style
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "while Maybe_true",
            "{   Go_for_it33",
            "    Go_for_it34",
            "}",
            "elif Not_true",
            "{   go_for_it22",
            "    go_for_it23",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        // Implicit brace
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "while Maybe_true",
            "    Go_for_it33",
            "    Go_for_it34",
            "elif Not_true",
            "    go_for_it22",
            "    go_for_it23",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 8, .next = 0 } }, // root statement
            Node{ .atomic_token = 3 }, // Maybe_true
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } }, // loop block
            Node{ .statement = .{ .node = 5, .next = 6 } }, // first loop statement
            // [5]:
            Node{ .atomic_token = 5 }, // Go_for_it33
            Node{ .statement = .{ .node = 7, .next = 0 } }, // second loop statement
            Node{ .atomic_token = 7 }, // Go_for_it34
            Node{ .while_loop = .{ .condition = 2, .loop_node = 3, .else_node = 15 } }, // while
            Node{ .atomic_token = 11 }, // Not_true
            // [10]:
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 11 } }, // elif block
            Node{ .statement = .{ .node = 12, .next = 13 } }, // first elif statement
            Node{ .callable_token = 13 }, // go_for_it22
            Node{ .statement = .{ .node = 14, .next = 0 } }, // second elif statement
            Node{ .callable_token = 15 }, // go_for_it23
            // [15]:
            Node{ .conditional = .{ .condition = 9, .if_node = 10, .else_node = 0 } }, // elif
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parsing simple while statements" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 8, .next = 0 } }, // root statement
        Node{ .atomic_token = 3 }, // Verily_true
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 4 } }, // loop block
        Node{ .statement = .{ .node = 5, .next = 0 } }, // loop statement
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } }, // loop indent
        Node{ .statement = .{ .node = 7, .next = 0 } }, // loop indent statement
        Node{ .atomic_token = 7 }, // Very_do
        Node{ .while_loop = .{ .condition = 2, .loop_node = 3, .else_node = 0 } }, // while
        .end,
    };
    {
        // Explicit brace, one-true-brace style
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "while Verily_true {",
            "    Very_do",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        // Explicit brace, Horstmann style
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "while Verily_true",
            "{   Very_do",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        // Implicit brace
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "while Verily_true",
            "    Very_do",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 3 }, // Verily_true
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 0 } },
            // [5]:
            Node{ .atomic_token = 5 }, // Very_do
            Node{ .while_loop = .{ .condition = 2, .loop_node = 3, .else_node = 0 } },
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}
