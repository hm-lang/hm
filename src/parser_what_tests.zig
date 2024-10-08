const common = @import("common.zig");
const DoNothing = @import("do_nothing.zig").DoNothing;
const Node = @import("node.zig").Node;
const Parser = @import("parser.zig").Parser;

const std = @import("std");

test "parsing what statements" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 38, .next = 0 } }, // root statement
        Node{ .atomic_token = 3 }, // This_value
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 4 } }, // root brace
        Node{ .statement = .{ .node = 5, .next = 0 } }, // root brace statement
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } }, // root brace indent
        Node{ .statement = .{ .node = 13, .next = 14 } }, // first indent statement (123)
        Node{ .atomic_token = 7 }, // 123
        Node{ .enclosed = .{ .open = .brace, .tab = 4, .start = 9 } }, // 123 { ... }
        Node{ .statement = .{ .node = 10, .next = 0 } }, // statement in 123 { ... }
        // [10]:
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 11 } }, // indent in 123 { ... }
        Node{ .statement = .{ .node = 12, .next = 0 } }, // indent statement in 123 { ... }
        Node{ .callable_token = 11 }, // finish_early
        Node{ .binary = .{ .operator = .op_indent, .left = 7, .right = 8 } }, // 123 |> { ... }
        Node{ .statement = .{ .node = 15, .next = 16 } }, // second indent statement (A)
        // [15]:
        Node{ .atomic_token = 15 }, // A
        Node{ .statement = .{ .node = 25, .next = 26 } }, // third indent statement (B)
        Node{ .atomic_token = 19 }, // B
        Node{ .enclosed = .{ .open = .brace, .tab = 4, .start = 19 } }, // brace for B |> {...}
        Node{ .statement = .{ .node = 20, .next = 0 } }, // statement in B |> {...}
        // [20]:
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 21 } }, // indent in B |> {...}
        Node{ .statement = .{ .node = 22, .next = 23 } }, // first statement in B |> {...} indent
        Node{ .atomic_token = 23 }, // Celsius
        Node{ .statement = .{ .node = 24, .next = 0 } }, // second statement in B |> {...} indent
        Node{ .atomic_token = 25 }, // Draco
        // [25]:
        Node{ .binary = .{ .operator = .op_indent, .left = 17, .right = 18 } }, // B |> { ... }
        Node{ .statement = .{ .node = 37, .next = 0 } }, // third indent statement (Int5)
        Node{ .atomic_token = 29 }, // Int5
        Node{ .callable_token = 33 }, // int
        Node{ .binary = .{ .operator = .op_declare_readonly, .left = 27, .right = 28 } }, // Int5: int
        // [30]:
        Node{ .enclosed = .{ .open = .brace, .tab = 4, .start = 31 } }, // Int5 |> {...} brace
        Node{ .statement = .{ .node = 32, .next = 0 } }, // statement in Int5 brace
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 33 } }, // indent in Int5 brace
        Node{ .statement = .{ .node = 36, .next = 0 } }, // indent statement inside Int5 brace
        Node{ .atomic_token = 37 }, // Int5
        // [35]:
        Node{ .atomic_token = 41 }, // 3477
        Node{ .binary = .{ .operator = .op_multiply, .left = 34, .right = 35 } }, // Int5 * 3477
        Node{ .binary = .{ .operator = .op_indent, .left = 29, .right = 30 } }, // Int5 |> { ... }
        Node{ .what = .{ .evaluate = 2, .block = 3 } }, // what
        .end,
    };
    {
        // Explicit brace, one-true-brace style
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer parser.debug();
        const file_slice = [_][]const u8{
            "what This_value {",
            "    123 {",
            "        finish_early",
            "    }",
            "    A, B {",
            "        Celsius",
            "        Draco",
            "    }",
            "    Int5: int {",
            "        Int5 * 3477",
            "    }",
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
        errdefer parser.debug();
        const file_slice = [_][]const u8{
            "what This_value",
            "{   123",
            "    {   finish_early",
            "    }",
            "    A, B",
            "    {   Celsius",
            "        Draco",
            "    }",
            "    Int5: int",
            "    {   Int5 * 3477",
            "    }",
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
        errdefer parser.debug();
        const file_slice = [_][]const u8{
            "what This_value",
            "    123",
            "        finish_early",
            "    A, B",
            "        Celsius",
            "        Draco",
            "    Int5: int",
            "        Int5 * 3477",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 30, .next = 0 } },
            Node{ .atomic_token = 3 }, // This_value
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } }, // what indent
            Node{ .statement = .{ .node = 9, .next = 10 } }, // first statement in what indent (123...)
            // [5]:
            Node{ .atomic_token = 5 }, // 123
            Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 7 } }, // indent after 123
            Node{ .statement = .{ .node = 8, .next = 0 } }, // first statement in 123 indent
            Node{ .callable_token = 7 }, // finish_early
            Node{ .binary = .{ .operator = .op_indent, .left = 5, .right = 6 } }, // 123 |> ...
            // [10]:
            Node{ .statement = .{ .node = 11, .next = 12 } }, // second statement in what indent (A)
            Node{ .atomic_token = 9 }, // A
            Node{ .statement = .{ .node = 19, .next = 20 } }, // third statement in what indent (B...)
            Node{ .atomic_token = 13 }, // B
            Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 15 } }, // B's indent
            // [15]:
            Node{ .statement = .{ .node = 16, .next = 17 } }, // first statement in B indent
            Node{ .atomic_token = 15 }, // Celsius
            Node{ .statement = .{ .node = 18, .next = 0 } }, // second statement in B indent
            Node{ .atomic_token = 17 }, // Draco
            Node{ .binary = .{ .operator = .op_indent, .left = 13, .right = 14 } }, // B |> ...
            // [20]:
            Node{ .statement = .{ .node = 29, .next = 0 } }, // fourth statement in what indent (Int5...)
            Node{ .atomic_token = 19 }, // Int5
            Node{ .callable_token = 23 }, // int
            Node{ .binary = .{ .operator = .op_declare_readonly, .left = 21, .right = 22 } }, // Int5: int
            Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 25 } }, // Int5 |> ...
            // [25]:
            Node{ .statement = .{ .node = 28, .next = 0 } }, // first statement in Int5 indent
            Node{ .atomic_token = 25 }, // Int5
            Node{ .atomic_token = 29 }, // 3477
            Node{ .binary = .{ .operator = .op_multiply, .left = 26, .right = 27 } }, // Int5 * 3477
            Node{ .binary = .{ .operator = .op_indent, .left = 23, .right = 24 } }, // Int5 |> ...
            // [30]:
            Node{ .what = .{ .evaluate = 2, .block = 3 } }, // what
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}
