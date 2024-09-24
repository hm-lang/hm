const common = @import("common.zig");
const DoNothing = @import("do_nothing.zig").DoNothing;
const Node = @import("node.zig").Node;
const Parser = @import("parser.zig").Parser;
const SmallString = @import("string.zig").Small;

const std = @import("std");

// General test structure:
// Organize by topic but within each topic put easier tests near the end.
// This makes easy tests fail last, and they are easier to debug
// if they are at the end of the input.

test "declarations with missing right expressions" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer parser.debug();
        try parser.tokenizer.file.lines.append(try SmallString.init("Esper;"));
        try parser.tokenizer.file.lines.append(try SmallString.init("Jesper."));
        try parser.tokenizer.file.lines.append(try SmallString.init("Esperk:"));

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 3, .next = 4 } },
            Node{ .atomic_token = 1 }, // Esper
            Node{ .postfix = .{ .operator = .op_declare_writable, .node = 2 } },
            Node{ .statement = .{ .node = 6, .next = 7 } },
            // [5]:
            Node{ .atomic_token = 5 }, // Jesper
            Node{ .postfix = .{ .operator = .op_declare_temporary, .node = 5 } },
            Node{ .statement = .{ .node = 9, .next = 0 } },
            Node{ .atomic_token = 9 }, // Esperk
            Node{ .postfix = .{ .operator = .op_declare_readonly, .node = 8 } },
            // [10]:
            .end,
        });
        // No errors in attempts to parse a RHS expression for the infix operators.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "Esper;",
            "Jesper.",
            "Esperk:",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("(Jarok:)"));
        try parser.tokenizer.file.lines.append(try SmallString.init("[Turmeric;]"));
        try parser.tokenizer.file.lines.append(try SmallString.init("{Quinine.}"));

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 2, .next = 6 } },
            Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 5, .next = 0 } },
            Node{ .atomic_token = 3 }, // Jarok
            // [5]:
            Node{ .postfix = .{ .operator = .op_declare_readonly, .node = 4 } },
            Node{ .statement = .{ .node = 7, .next = 11 } },
            Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 8 } },
            Node{ .statement = .{ .node = 10, .next = 0 } },
            Node{ .atomic_token = 11 }, // Turmeric
            // [10]:
            Node{ .postfix = .{ .operator = .op_declare_writable, .node = 9 } },
            Node{ .statement = .{ .node = 12, .next = 0 } },
            Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 13 } },
            Node{ .statement = .{ .node = 15, .next = 0 } },
            Node{ .atomic_token = 19 }, // Quinine
            // [15]:
            Node{ .postfix = .{ .operator = .op_declare_temporary, .node = 14 } },
            .end,
        });
        // No errors in attempts to parse a RHS expression for the infix operators.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "(Jarok:)",
            "[Turmeric;]",
            "{Quinine.}",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("funE1(F2.,G3;,H4:):"));

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 14, .next = 0 } },
            Node{ .callable_token = 1 }, // funE1
            Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
            Node{ .statement = .{ .node = 6, .next = 7 } },
            // [5]:
            Node{ .atomic_token = 5 }, // F2
            Node{ .postfix = .{ .operator = .op_declare_temporary, .node = 5 } },
            Node{ .statement = .{ .node = 9, .next = 10 } },
            Node{ .atomic_token = 11 }, // G3
            Node{ .postfix = .{ .operator = .op_declare_writable, .node = 8 } },
            // [10]:
            Node{ .statement = .{ .node = 12, .next = 0 } },
            Node{ .atomic_token = 17 }, // H4
            Node{ .postfix = .{ .operator = .op_declare_readonly, .node = 11 } },
            Node{ .binary = .{ .operator = .op_access, .left = 2, .right = 3 } },
            Node{ .postfix = .{ .operator = .op_declare_readonly, .node = 13 } },
            // [15]:
            .end,
        });
        // No errors in attempts to parse a RHS expression for the infix operators.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "funE1(F2.,G3;,H4:):",
        });
    }
}

test "declaring a variable with arguments and/or generics" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "Set(543).",
        "Array[element_type](1, 2, 3):",
        "Lot[inner_type, at: index4];",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete(DoNothing{});

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 7, .next = 8 } },
        Node{ .atomic_token = 1 }, // Set
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 5, .next = 0 } },
        // [5]:
        Node{ .atomic_token = 5 }, // 543
        Node{ .binary = .{ .operator = .op_access, .left = 2, .right = 3 } },
        Node{ .postfix = .{ .operator = .op_declare_temporary, .node = 6 } },
        Node{ .statement = .{ .node = 22, .next = 23 } },
        Node{ .atomic_token = 11 }, // Array
        // [10]:
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 11 } },
        Node{ .statement = .{ .node = 12, .next = 0 } },
        Node{ .callable_token = 15 }, // element_type
        Node{ .binary = .{ .operator = .op_access, .left = 9, .right = 10 } },
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 15 } },
        // [15]:
        Node{ .statement = .{ .node = 16, .next = 17 } },
        Node{ .atomic_token = 21 }, // 1
        Node{ .statement = .{ .node = 18, .next = 19 } },
        Node{ .atomic_token = 25 }, // 2
        Node{ .statement = .{ .node = 20, .next = 0 } },
        // [20]:
        Node{ .atomic_token = 29 }, // 3
        Node{ .binary = .{ .operator = .op_access, .left = 13, .right = 14 } },
        Node{ .postfix = .{ .operator = .op_declare_readonly, .node = 21 } },
        Node{ .statement = .{ .node = 33, .next = 0 } },
        Node{ .atomic_token = 35 }, // Lot
        // [25]:
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 26 } },
        Node{ .statement = .{ .node = 27, .next = 28 } },
        Node{ .callable_token = 39 }, // inner_type
        Node{ .statement = .{ .node = 31, .next = 0 } },
        Node{ .callable_token = 43 }, // at
        // [30]:
        Node{ .callable_token = 47 }, // index4
        Node{ .binary = .{ .operator = .op_declare_readonly, .left = 29, .right = 30 } },
        Node{ .binary = .{ .operator = .op_access, .left = 24, .right = 25 } },
        Node{ .postfix = .{ .operator = .op_declare_writable, .node = 32 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "parser declare and nested assigns" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("D1: D2; D3"));

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // D1
            Node{ .atomic_token = 5 }, // D2
            Node{ .binary = .{ .operator = .op_declare_readonly, .left = 2, .right = 6 } },
            // [5]:
            Node{ .atomic_token = 9 }, // D3
            Node{ .binary = .{ .operator = .op_declare_writable, .left = 3, .right = 5 } },
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("X3 = Y4 = 750"));

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // X3
            Node{ .atomic_token = 5 }, // Y4
            Node{ .binary = .{ .operator = .op_assign, .left = 2, .right = 6 } },
            // [5]:
            Node{ .atomic_token = 9 }, // 750
            Node{ .binary = .{ .operator = .op_assign, .left = 3, .right = 5 } },
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("VarQ; i32 = Qu16 = VarU: i16 = 750"));

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // VarQ
            Node{ .callable_token = 5 }, // i32
            Node{ .binary = .{ .operator = .op_declare_writable, .left = 2, .right = 3 } },
            // [5]:
            Node{ .atomic_token = 9 }, // Qu16
            Node{ .binary = .{ .operator = .op_assign, .left = 4, .right = 8 } },
            Node{ .atomic_token = 13 }, // VarU
            Node{ .binary = .{ .operator = .op_assign, .left = 5, .right = 12 } },
            Node{ .callable_token = 17 }, // i16
            // [10]:
            Node{ .binary = .{ .operator = .op_declare_readonly, .left = 7, .right = 9 } },
            Node{ .atomic_token = 21 }, // 750
            Node{ .binary = .{ .operator = .op_assign, .left = 10, .right = 11 } },
            .end,
        });
    }
}

test "parser declare and assign" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("Declassign: type_assign1 = 12345"));

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // Declassign
            Node{ .callable_token = 5 }, // type_assign1
            Node{ .binary = .{ .operator = .op_declare_readonly, .left = 2, .right = 3 } },
            // [5]:
            Node{ .atomic_token = 9 }, // 12345
            Node{ .binary = .{ .operator = .op_assign, .left = 4, .right = 5 } },
            .end,
        });
        // No errors in attempts to parse `callable`.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "Declassign: type_assign1 = 12345",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("Oh_writable; type_assign2 = 7890"));

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // Oh_writable
            Node{ .callable_token = 5 }, // type_assign2
            Node{ .binary = .{ .operator = .op_declare_writable, .left = 2, .right = 3 } },
            // [5]:
            Node{ .atomic_token = 9 },
            Node{ .binary = .{ .operator = .op_assign, .left = 4, .right = 5 } },
            .end,
        });
        // No errors in attempts to parse `callable`.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "Oh_writable; type_assign2 = 7890",
        });
    }
}

test "parser declare" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("Whatever: type1"));

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // Whatever
            Node{ .callable_token = 5 }, // type1
            Node{ .binary = .{ .operator = .op_declare_readonly, .left = 2, .right = 3 } },
            // [5]:
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("Writable_whatever; type2"));

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // Writable_whatever
            Node{ .callable_token = 5 }, // type2
            Node{ .binary = .{ .operator = .op_declare_writable, .left = 2, .right = 3 } },
            // [5]:
            .end,
        });
    }
}
