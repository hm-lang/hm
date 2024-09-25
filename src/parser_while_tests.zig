const common = @import("common.zig");
const DoNothing = @import("do_nothing.zig").DoNothing;
const Node = @import("node.zig").Node;
const Parser = @import("parser.zig").Parser;

const std = @import("std");

test "parsing nested while statements" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 23, .next = 0 } }, // root statement
        Node{ .atomic_token = 1 }, // 5
        Node{ .atomic_token = 7 }, // Skelluton
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 5 } }, // root if brace
        // [5]:
        Node{ .statement = .{ .node = 6, .next = 0 } }, // root if brace statement
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 7 } }, // root if indent
        Node{ .statement = .{ .node = 14, .next = 0 } }, // root if indent statement
        Node{ .atomic_token = 13 }, // Brandenborg
        Node{ .enclosed = .{ .open = .brace, .tab = 4, .start = 10 } }, // inner if brace
        // [10]:
        Node{ .statement = .{ .node = 11, .next = 0 } }, // inner if statement
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 12 } }, // inner if indent
        Node{ .statement = .{ .node = 13, .next = 0 } }, // inner if indent statement
        Node{ .atomic_token = 17 }, // Chetty
        Node{ .while_loop = .{ .condition = 8, .loop_node = 9, .else_node = 15 } }, // inner if statement
        // [15]:
        Node{ .enclosed = .{ .open = .brace, .tab = 4, .start = 16 } }, // inner else brace
        Node{ .statement = .{ .node = 17, .next = 0 } }, // inner else statement
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 18 } }, // inner else indent
        Node{ .statement = .{ .node = 19, .next = 20 } }, // inner else indent first statement
        Node{ .atomic_token = 25 }, // Betty
        // [20]:
        Node{ .statement = .{ .node = 21, .next = 0 } }, // inner else indent second statement
        Node{ .atomic_token = 27 }, // Aetty
        Node{ .while_loop = .{ .condition = 3, .loop_node = 4, .else_node = 0 } }, // root if
        Node{ .binary = .{ .operator = .op_plus, .left = 2, .right = 25 } }, // 5 + ...
        Node{ .atomic_token = 35 }, // 3
        // [25]:
        Node{ .binary = .{ .operator = .op_multiply, .left = 22, .right = 24 } }, // root if * 3
        .end,
    };
    {
        // Explicit brace, one-true-brace style
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer parser.debug();
        const file_slice = [_][]const u8{
            "5 + while Skelluton {",
            "    while Brandenborg {",
            "        Chetty",
            "    } else {",
            "        Betty",
            "        Aetty",
            "    }",
            "} * 3",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        // Explicit brace, Horstmann style
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer parser.debug();
        const file_slice = [_][]const u8{
            "5 + while Skelluton",
            "{   while Brandenborg",
            "    {   Chetty",
            "    }",
            "    else",
            "    {   Betty",
            "        Aetty",
            "    }",
            "} * 3",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        // Implicit brace
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer parser.debug();
        const file_slice = [_][]const u8{
            "5 + 3 * while Skelluton",
            "    while Brandenborg",
            "        Chetty",
            "    else",
            "        Betty",
            "        Aetty",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } }, // root statement
            Node{ .atomic_token = 1 }, // 5
            Node{ .atomic_token = 5 }, // 3
            Node{ .binary = .{ .operator = .op_plus, .left = 2, .right = 19 } }, // 5 + ...
            // [5]:
            Node{ .atomic_token = 11 }, // Skelluton
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 7 } }, // root if indent
            Node{ .statement = .{ .node = 12, .next = 0 } }, // root if indent statement
            Node{ .atomic_token = 15 }, // Brandenborg
            Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 10 } }, // inner if indent
            // [10]:
            Node{ .statement = .{ .node = 11, .next = 0 } }, // inner if statement
            Node{ .atomic_token = 17 }, // Chetty
            Node{ .while_loop = .{ .condition = 8, .loop_node = 9, .else_node = 13 } }, // inner if
            Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 14 } }, // inner else
            Node{ .statement = .{ .node = 15, .next = 16 } }, // inner else first statement
            // [15]:
            Node{ .atomic_token = 21 }, // Betty
            Node{ .statement = .{ .node = 17, .next = 0 } }, // inner else second statement
            Node{ .atomic_token = 23 }, // Aetty
            Node{ .while_loop = .{ .condition = 5, .loop_node = 6, .else_node = 0 } }, // root if
            Node{ .binary = .{ .operator = .op_multiply, .left = 3, .right = 18 } }, // 3 * if...
            // [20]:
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

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

        try parser.complete(DoNothing{});

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

        try parser.complete(DoNothing{});

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

        try parser.complete(DoNothing{});

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

        try parser.complete(DoNothing{});

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

        try parser.complete(DoNothing{});

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

        try parser.complete(DoNothing{});

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
