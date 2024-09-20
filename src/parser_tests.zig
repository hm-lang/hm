const common = @import("common.zig");
const DoNothing = @import("do_nothing.zig").DoNothing;
const Node = @import("node.zig").Node;
const Parser = @import("parser.zig").Parser;
const Operator = @import("operator.zig").Operator;
const Operation = Operator.Operation;
const SmallString = @import("string.zig").Small;

const std = @import("std");

// General test structure:
// Organize by topic but within each topic put easier tests near the end.
// This makes easy tests fail last, and they are easier to debug
// if they are at the end of the input.

test "parser indent nesting" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 30, .next = 0 } }, // first (only) statement in root block
        Node{ .callable_token = 1 }, // greet_thee
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 0 } }, // ()
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } }, // greet_thee ()
        // [5]:
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 6 } }, // {...}
        Node{ .statement = .{ .node = 7, .next = 0 } }, // outer indent statement
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 8 } }, // outer indent
        Node{ .statement = .{ .node = 17, .next = 18 } }, // print statement
        Node{ .callable_token = 11 }, // print
        // [10]:
        Node{ .enclosed = .{ .open = .paren, .tab = 4, .start = 11 } }, // (...) inside print
        Node{ .statement = .{ .node = 12, .next = 0 } }, // statement inside print
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 13 } }, // indent inside print
        Node{ .statement = .{ .node = 14, .next = 15 } }, // first statement in print indent
        Node{ .atomic_token = 15 }, // 99731
        // [15]:
        Node{ .statement = .{ .node = 16, .next = 0 } }, // second statement in print indent
        Node{ .atomic_token = 17 }, // World
        Node{ .binary = .{ .operator = Operator.access, .left = 9, .right = 10 } }, // print (...)
        Node{ .statement = .{ .node = 29, .next = 0 } }, // wow54 statement
        Node{ .callable_token = 21 }, // wow54
        // [20]:
        Node{ .enclosed = .{ .open = .paren, .tab = 4, .start = 21 } }, // wow54 paren
        Node{ .statement = .{ .node = 22, .next = 0 } }, // inside wow54(...) statement
        Node{ .enclosed = .{ .open = .bracket, .tab = 4, .start = 23 } }, // [...]
        Node{ .statement = .{ .node = 24, .next = 0 } }, // inside wow54([...]) statement
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 25 } }, // indent inside ([...])
        // [25]:
        Node{ .statement = .{ .node = 26, .next = 27 } }, // first statement inside ([...])
        Node{ .atomic_token = 27 }, // 57973
        Node{ .statement = .{ .node = 28, .next = 0 } }, // second statement inside ([...])
        Node{ .atomic_token = 29 }, // 67974
        Node{ .binary = .{ .operator = Operator.access, .left = 19, .right = 20 } }, // wow (...)
        // [30]:
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 4, .right = 5 } }, // :
        .end,
    };
    {
        // One-true-brace
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
            common.debugPrint("# nodes:\n", parser.nodes);
        }
        const file_slice = [_][]const u8{
            "greet_thee(): {",
            "    print(",
            "        99731",
            "        World",
            "    )",
            "    wow54([",
            "        57973",
            "        67974",
            "    ])",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        // Horstmann
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
            common.debugPrint("# nodes:\n", parser.nodes);
        }
        const file_slice = [_][]const u8{
            "greet_thee():",
            "{   print",
            "    (   99731",
            "        World",
            "    )",
            "    wow54",
            "    ([  57973",
            "        67974",
            "    ])",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parser simple bracket indent" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 8, .next = 0 } },
        Node{ .atomic_token = 1 }, // Bart4
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 5, .next = 0 } },
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } },
        Node{ .statement = .{ .node = 7, .next = 0 } },
        Node{ .atomic_token = 5 }, // Bented4
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        .end,
    };
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart4",
            "[   Bented4",
            "]",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart4[",
            "    Bented4",
            "]",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parser simple paren indent" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 9, .next = 0 } },
        Node{ .atomic_token = 1 }, // Bart3
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 5, .next = 0 } },
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } },
        Node{ .statement = .{ .node = 7, .next = 0 } },
        Node{ .prefix = .{ .operator = Operator.plus, .node = 8 } },
        Node{ .atomic_token = 7 }, // Bented3
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        // [10]:
        .end,
    };
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart3",
            "(   +Bented3",
            ")",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart3(",
            "    +Bented3",
            ")",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

// TODO: implicit blocks
test "parser simple explicit brace block" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 10, .next = 0 } }, // root block first (only) statement
        Node{ .atomic_token = 1 }, // Bart5
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 4 } }, // {...}
        Node{ .statement = .{ .node = 5, .next = 0 } }, // internal statement
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } }, // indent
        Node{ .statement = .{ .node = 7, .next = 8 } }, // Bented51 statement
        Node{ .atomic_token = 7 }, // Bented51
        Node{ .statement = .{ .node = 9, .next = 0 } }, // Bented52 statement
        Node{ .atomic_token = 9 }, // Bented52
        // [10]:
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 3 } }, // :
        .end,
    };
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart5:",
            "{   Bented51",
            "    Bented52",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Bart5: {",
            "    Bented51",
            "    Bented52",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

// TODO: add `SomeCondition { some_bracket_logic() }`
//      braces need to be lower priority than `access`.
//      same for `open = .none` blocks.
//&|if MyCondition
//&|    do_something()

// TODO: test to error out on spacing indented to +8 or more at start of file
// TODO: test to see what happens when indented to +4 at start of file

//test "parsing quotes" {
//    {
//    var parser: Parser = .{};
//    defer parser.deinit();
//    errdefer {
//        common.debugPrint("# file:\n", parser.tokenizer.file);
//    }
//    const file_slice = [_][]const u8{
//        "'hi quote'",
//    };
//    try parser.tokenizer.file.appendSlice(&file_slice);
//
//    try parser.complete();
//
//    try parser.nodes.expectEqualsSlice(&[_]Node{
//        .end,
//    });
//    // No tampering done with the file, i.e., no errors.
//    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
//    }
//
//}

test "parser simple expressions" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "3.456",
        "    hello_you",
        "+1.234",
        "    -5.678",
        "    $$$Foe",
        "        Fum--",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 6, .next = 7 } }, // statement of 3.456 + indent
        Node{ .atomic_token = 1 }, // 3.456
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } }, // indent with hello_you
        Node{ .statement = .{ .node = 5, .next = 0 } }, // hello_you statement
        // [5]:
        Node{ .callable_token = 3 }, // hello_you
        Node{ .binary = .{ .operator = Operator.indent, .left = 2, .right = 3 } }, // 3.456 indent
        Node{ .statement = .{ .node = 22, .next = 0 } }, // statement of +1.234 + indent
        Node{ .prefix = .{ .operator = Operator.plus, .node = 9 } }, // +1.234
        Node{ .atomic_token = 7 }, // 1.234
        // [10]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 11 } }, // block with -5.678 starting
        Node{ .statement = .{ .node = 12, .next = 14 } }, // -5.678 statement
        Node{ .prefix = .{ .operator = Operator.minus, .node = 13 } }, // -5.678
        Node{ .atomic_token = 11 }, // 5.678
        Node{ .statement = .{ .node = 21, .next = 0 } }, // $$$Foe indent statement
        // [15]:
        Node{ .prefix = .{ .operator = Operator.lambda3, .node = 16 } }, // $$$Foe
        Node{ .atomic_token = 15 }, // Foe
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 18 } },
        Node{ .statement = .{ .node = 20, .next = 0 } },
        Node{ .atomic_token = 17 }, // Fum
        // [20]:
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 19 } },
        Node{ .binary = .{ .operator = Operator.indent, .left = 15, .right = 17 } },
        Node{ .binary = .{ .operator = Operator.indent, .left = 8, .right = 10 } },
        .end,
    });
    // No tampering done with the file, i.e., no errors.
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "parser implied blocks" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Start4",
            "    Indented4",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // Start4
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 0 } },
            // [5]:
            Node{ .atomic_token = 3 }, // Indented4
            Node{ .binary = .{ .operator = Operator.indent, .left = 2, .right = 3 } },
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Start3",
            "    +Indented3",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 7, .next = 0 } },
            Node{ .atomic_token = 1 }, // Start3
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 0 } },
            // [5]:
            Node{ .prefix = .{ .operator = Operator.plus, .node = 6 } },
            Node{ .atomic_token = 5 }, // Indented3
            Node{ .binary = .{ .operator = Operator.indent, .left = 2, .right = 3 } },
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "Start5:",
            "    Indented51",
            "    Indented52",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 8, .next = 0 } },
            Node{ .atomic_token = 1 }, // Start5
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 6 } },
            // [5]:
            Node{ .atomic_token = 5 }, // Indented51
            Node{ .statement = .{ .node = 7, .next = 0 } },
            Node{ .atomic_token = 7 }, // Indented52
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 3 } },
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parser line continuations" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "Random_id3",
        "    +   Other_id4",
        "    -   Other_id5",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 6, .next = 0 } },
        Node{ .atomic_token = 1 }, // Random_id3
        Node{ .atomic_token = 5 }, // Other_id4
        Node{ .binary = .{ .operator = Operator.plus, .left = 2, .right = 3 } },
        // [5]:
        Node{ .atomic_token = 9 }, // Other_id5
        Node{ .binary = .{ .operator = Operator.minus, .left = 4, .right = 5 } },
        .end,
    });
    // No tampering done with the file, i.e., no errors.
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "parser multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Wompus * 3.14"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 4, .next = 0 } },
        Node{ .atomic_token = 1 }, // Wompus
        Node{ .atomic_token = 5 }, // 3.14
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 3 } },
        // [5]:
        .end,
    });
}

test "parser simple (and postfix) implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "Pi Sky",
        "Sci Fi++",
        "Kite Sty Five!",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 4, .next = 5 } },
        Node{ .atomic_token = 1 }, // Pi
        Node{ .atomic_token = 3 }, // Sky
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        // [5]:
        Node{ .statement = .{ .node = 9, .next = 10 } },
        Node{ .atomic_token = 5 }, // Sci
        Node{ .atomic_token = 7 }, // Fi
        Node{ .binary = .{ .operator = Operator.access, .left = 6, .right = 7 } },
        Node{ .postfix = .{ .operator = Operator.increment, .node = 8 } },
        // [10]:
        Node{ .statement = .{ .node = 16, .next = 0 } },
        Node{ .atomic_token = 11 }, // Kite
        Node{ .atomic_token = 13 }, // Sty
        Node{ .binary = .{ .operator = Operator.access, .left = 11, .right = 12 } },
        Node{ .atomic_token = 15 }, // Five
        // [15]:
        Node{ .binary = .{ .operator = Operator.access, .left = 13, .right = 14 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 15 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "parser complicated (and prefix) implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "--Why Shy Spy",
        "!Chai Lie Fry",
        "!Knife Fly Nigh!",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 2, .next = 8 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 7 } },
        Node{ .atomic_token = 3 }, // Why
        Node{ .atomic_token = 5 }, // Shy
        // [5]:
        Node{ .binary = .{ .operator = Operator.access, .left = 3, .right = 4 } },
        Node{ .atomic_token = 7 }, // Spy
        Node{ .binary = .{ .operator = Operator.access, .left = 5, .right = 6 } },
        Node{ .statement = .{ .node = 9, .next = 15 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 14 } },
        // [10]:
        Node{ .atomic_token = 11 }, // Chai
        Node{ .atomic_token = 13 }, // Lie
        Node{ .binary = .{ .operator = Operator.access, .left = 10, .right = 11 } },
        Node{ .atomic_token = 15 }, // Fry
        Node{ .binary = .{ .operator = Operator.access, .left = 12, .right = 13 } },
        // [15]:
        Node{ .statement = .{ .node = 16, .next = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 22 } },
        Node{ .atomic_token = 19 }, // Knife
        Node{ .atomic_token = 21 }, // Fly
        Node{ .binary = .{ .operator = Operator.access, .left = 17, .right = 18 } },
        // [20]:
        Node{ .atomic_token = 23 }, // Nigh
        Node{ .binary = .{ .operator = Operator.access, .left = 19, .right = 20 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 21 } },
        .end,
    });
    // No tampering done with the file, i.e., no errors.
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "simple prefix/postfix operators with multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "++Theta * Beta",
        "Zeta * ++Woga",
        "Yodus-- * Spatula",
        "Wobdash * Flobsmash--",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 5, .next = 6 } },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 3 } },
        Node{ .atomic_token = 3 }, // Theta
        Node{ .atomic_token = 7 }, // Beta
        // [5]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 4 } },
        Node{ .statement = .{ .node = 10, .next = 11 } },
        Node{ .atomic_token = 9 }, // Zeta
        Node{ .prefix = .{ .operator = Operator.increment, .node = 9 } },
        Node{ .atomic_token = 15 }, // Woga
        // [10]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 7, .right = 8 } },
        Node{ .statement = .{ .node = 15, .next = 16 } },
        Node{ .atomic_token = 17 }, // Yodus
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 12 } },
        Node{ .atomic_token = 23 }, // Spatula
        // [15]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 13, .right = 14 } },
        Node{ .statement = .{ .node = 19, .next = 0 } },
        Node{ .atomic_token = 25 }, // Wobdash
        Node{ .atomic_token = 29 }, // Flobsmash
        Node{ .binary = .{ .operator = Operator.multiply, .left = 17, .right = 20 } },
        // [20]:
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 18 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "complicated prefix/postfix operators with addition/multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Apple * !Berry Cantaloupe-- + 500"));
    try parser.tokenizer.file.lines.append(try SmallString.init("--Xeno Yak! - 3000 * Zelda"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 10, .next = 11 } },
        Node{ .atomic_token = 1 }, // Apple
        Node{ .prefix = .{ .operator = Operator.not, .node = 7 } },
        Node{ .atomic_token = 7 }, // Berry
        // [5]:
        Node{ .atomic_token = 9 }, // Cantaloupe
        Node{ .binary = .{ .operator = Operator.access, .left = 4, .right = 5 } },
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 6 } },
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 3 } },
        Node{ .atomic_token = 15 }, // 500
        // [10]:
        Node{ .binary = .{ .operator = Operator.plus, .left = 8, .right = 9 } },
        Node{ .statement = .{ .node = 18, .next = 0 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 15 } },
        Node{ .atomic_token = 19 }, // Xeno
        Node{ .atomic_token = 21 }, // Yak
        // [15]:
        Node{ .binary = .{ .operator = Operator.access, .left = 13, .right = 14 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 12 } },
        Node{ .atomic_token = 27 }, // 3000
        Node{ .binary = .{ .operator = Operator.minus, .left = 16, .right = 20 } },
        Node{ .atomic_token = 31 }, // Zelda
        // [20]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 17, .right = 19 } },
        .end,
    });
}

test "nested prefix/postfix operators" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "Abc Xyz-- !",
        "! ++Def Uvw",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 6, .next = 7 } },
        Node{ .atomic_token = 1 }, // Abc
        Node{ .atomic_token = 3 }, // Xyz
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        // [5]:
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 4 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 5 } },
        Node{ .statement = .{ .node = 8, .next = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 9 } },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 12 } },
        // [10]:
        Node{ .atomic_token = 13 }, // Def
        Node{ .atomic_token = 15 }, // Uvw
        Node{ .binary = .{ .operator = Operator.access, .left = 10, .right = 11 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "deeply nested prefix/postfix operators" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("$$Yammer * Zen++!"));
    try parser.tokenizer.file.lines.append(try SmallString.init("!--$Oh Great * Hessian"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 5, .next = 8 } },
        Node{ .prefix = .{ .operator = Operator.lambda2, .node = 3 } },
        Node{ .atomic_token = 3 }, // Yammer
        Node{ .atomic_token = 7 }, // Zen
        // [5]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 7 } },
        Node{ .postfix = .{ .operator = Operator.increment, .node = 4 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 6 } },
        Node{ .statement = .{ .node = 16, .next = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 10 } },
        // [10]:
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 14 } },
        Node{ .prefix = .{ .operator = Operator.lambda1, .node = 12 } },
        Node{ .atomic_token = 19 }, // Oh
        Node{ .atomic_token = 21 }, // Great
        Node{ .binary = .{ .operator = Operator.access, .left = 11, .right = 13 } },
        // [15]:
        Node{ .atomic_token = 25 }, // Hessian
        Node{ .binary = .{ .operator = Operator.multiply, .left = 9, .right = 15 } },
        .end,
    });
}

test "order of operations with addition and multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "Alpha * Gamma + Epsilon",
        "Panko + K_panko * 1000",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);
    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 6, .next = 7 } },
        Node{ .atomic_token = 1 }, // Alpha
        Node{ .atomic_token = 5 }, // Gamma
        Node{ .binary = .{ .operator = Operator.multiply, .left = 2, .right = 3 } },
        // [5]:
        Node{ .atomic_token = 9 }, // Epsilon
        Node{ .binary = .{ .operator = Operator.plus, .left = 4, .right = 5 } },
        Node{ .statement = .{ .node = 10, .next = 0 } },
        Node{ .atomic_token = 11 }, // Panko
        Node{ .atomic_token = 15 }, // K_panko
        // [10]:
        Node{ .binary = .{ .operator = Operator.plus, .left = 8, .right = 12 } },
        Node{ .atomic_token = 19 }, // 1000
        Node{ .binary = .{ .operator = Operator.multiply, .left = 9, .right = 11 } },
        .end,
    });

    // No tampering done with the file, i.e., no errors.
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "generic types" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "container54[of; i1234, at. str5[qusp], array[dongle]]",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 22, .next = 0 } },
        Node{ .callable_token = 1 }, // container54
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 7, .next = 8 } },
        // [5]:
        Node{ .callable_token = 5 }, // of
        Node{ .callable_token = 9 }, // i1234
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 5, .right = 6 } },
        Node{ .statement = .{ .node = 11, .next = 16 } },
        Node{ .callable_token = 13 }, // at
        // [10]:
        Node{ .callable_token = 17 }, // str5
        Node{ .binary = .{ .operator = Operator.declare_temporary, .left = 9, .right = 15 } },
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 13 } },
        Node{ .statement = .{ .node = 14, .next = 0 } },
        Node{ .callable_token = 21 }, // qusp
        // [15]:
        Node{ .binary = .{ .operator = Operator.access, .left = 10, .right = 12 } },
        Node{ .statement = .{ .node = 21, .next = 0 } },
        Node{ .callable_token = 27 }, // array
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 19 } },
        Node{ .statement = .{ .node = 20, .next = 0 } },
        // [20]:
        Node{ .callable_token = 31 }, // dongle
        Node{ .binary = .{ .operator = Operator.access, .left = 17, .right = 18 } },
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        .end,
    });
    // No errors when parsing:
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "simple function calls" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "superb(Brepus. 161, Canyon; Noynac, Candid)",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 14, .next = 0 } },
        Node{ .callable_token = 1 }, // superb
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 7, .next = 8 } },
        // [5]:
        Node{ .atomic_token = 5 }, // Brepus
        Node{ .atomic_token = 9 }, // 161
        Node{ .binary = .{ .operator = Operator.declare_temporary, .left = 5, .right = 6 } },
        Node{ .statement = .{ .node = 11, .next = 12 } },
        Node{ .atomic_token = 13 }, // Canyon
        // [10]:
        Node{ .atomic_token = 17 }, // Noynac
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 9, .right = 10 } },
        Node{ .statement = .{ .node = 13, .next = 0 } },
        Node{ .atomic_token = 21 }, // Candid
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        // [15]:
        .end,
    });
    // No errors when parsing:
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "generic function calls" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        common.debugPrint("# file:\n", parser.tokenizer.file);
    }
    const file_slice = [_][]const u8{
        "fungus[type1: t_array[str7], type2, type3; i64](Life: 17, Cardio!, Fritz; foo_fritz)",
    };
    try parser.tokenizer.file.appendSlice(&file_slice);

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 31, .next = 0 } },
        Node{ .callable_token = 1 }, // fungus
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 7, .next = 12 } },
        // [5]:
        Node{ .callable_token = 5 }, // type1
        Node{ .callable_token = 9 }, // t_array
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 5, .right = 11 } },
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 9 } },
        Node{ .statement = .{ .node = 10, .next = 0 } },
        // [10]:
        Node{ .callable_token = 13 }, // str7
        Node{ .binary = .{ .operator = Operator.access, .left = 6, .right = 8 } },
        Node{ .statement = .{ .node = 13, .next = 14 } },
        Node{ .callable_token = 19 }, // type2
        Node{ .statement = .{ .node = 17, .next = 0 } },
        // [15]:
        Node{ .callable_token = 23 }, // type3
        Node{ .callable_token = 27 }, // i64
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 15, .right = 16 } },
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 20 } },
        // [20]:
        Node{ .statement = .{ .node = 23, .next = 24 } },
        Node{ .atomic_token = 33 }, // Life
        Node{ .atomic_token = 37 }, // 17
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 21, .right = 22 } },
        Node{ .statement = .{ .node = 26, .next = 27 } },
        // [25]:
        Node{ .atomic_token = 41 }, // Cardio
        Node{ .postfix = .{ .operator = Operator.not, .node = 25 } },
        Node{ .statement = .{ .node = 30, .next = 0 } },
        Node{ .atomic_token = 47 }, // Fritz
        Node{ .callable_token = 51 }, // foo_fritz
        // [30]:
        Node{ .binary = .{ .operator = Operator.declare_writable, .left = 28, .right = 29 } },
        Node{ .binary = .{ .operator = Operator.access, .left = 18, .right = 19 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "declarations with missing right expressions" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
            common.debugPrint("# tokens:\n", parser.tokenizer.tokens);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("Esper;"));
        try parser.tokenizer.file.lines.append(try SmallString.init("Jesper."));
        try parser.tokenizer.file.lines.append(try SmallString.init("Esperk:"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 3, .next = 4 } },
            Node{ .atomic_token = 1 }, // Esper
            Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 2 } },
            Node{ .statement = .{ .node = 6, .next = 7 } },
            // [5]:
            Node{ .atomic_token = 5 }, // Jesper
            Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 5 } },
            Node{ .statement = .{ .node = 9, .next = 0 } },
            Node{ .atomic_token = 9 }, // Esperk
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 8 } },
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

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 2, .next = 6 } },
            Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 5, .next = 0 } },
            Node{ .atomic_token = 3 }, // Jarok
            // [5]:
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 4 } },
            Node{ .statement = .{ .node = 7, .next = 11 } },
            Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 8 } },
            Node{ .statement = .{ .node = 10, .next = 0 } },
            Node{ .atomic_token = 11 }, // Turmeric
            // [10]:
            Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 9 } },
            Node{ .statement = .{ .node = 12, .next = 0 } },
            Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 13 } },
            Node{ .statement = .{ .node = 15, .next = 0 } },
            Node{ .atomic_token = 19 }, // Quinine
            // [15]:
            Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 14 } },
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

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 14, .next = 0 } },
            Node{ .callable_token = 1 }, // funE1
            Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
            Node{ .statement = .{ .node = 6, .next = 7 } },
            // [5]:
            Node{ .atomic_token = 5 }, // F2
            Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 5 } },
            Node{ .statement = .{ .node = 9, .next = 10 } },
            Node{ .atomic_token = 11 }, // G3
            Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 8 } },
            // [10]:
            Node{ .statement = .{ .node = 12, .next = 0 } },
            Node{ .atomic_token = 17 }, // H4
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 11 } },
            Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
            Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 13 } },
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

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 7, .next = 8 } },
        Node{ .atomic_token = 1 }, // Set
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
        Node{ .statement = .{ .node = 5, .next = 0 } },
        // [5]:
        Node{ .atomic_token = 5 }, // 543
        Node{ .binary = .{ .operator = Operator.access, .left = 2, .right = 3 } },
        Node{ .postfix = .{ .operator = Operator.declare_temporary, .node = 6 } },
        Node{ .statement = .{ .node = 22, .next = 23 } },
        Node{ .atomic_token = 11 }, // Array
        // [10]:
        Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 11 } },
        Node{ .statement = .{ .node = 12, .next = 0 } },
        Node{ .callable_token = 15 }, // element_type
        Node{ .binary = .{ .operator = Operator.access, .left = 9, .right = 10 } },
        Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 15 } },
        // [15]:
        Node{ .statement = .{ .node = 16, .next = 17 } },
        Node{ .atomic_token = 21 }, // 1
        Node{ .statement = .{ .node = 18, .next = 19 } },
        Node{ .atomic_token = 25 }, // 2
        Node{ .statement = .{ .node = 20, .next = 0 } },
        // [20]:
        Node{ .atomic_token = 29 }, // 3
        Node{ .binary = .{ .operator = Operator.access, .left = 13, .right = 14 } },
        Node{ .postfix = .{ .operator = Operator.declare_readonly, .node = 21 } },
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
        Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 29, .right = 30 } },
        Node{ .binary = .{ .operator = Operator.access, .left = 24, .right = 25 } },
        Node{ .postfix = .{ .operator = Operator.declare_writable, .node = 32 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
}

test "simple parentheses, brackets, and braces" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("+(Wow, Great)"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 2, .next = 0 } },
            Node{ .prefix = .{ .operator = Operator.plus, .node = 3 } },
            Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 6 } },
            // [5]:
            Node{ .atomic_token = 5 }, // Wow
            Node{ .statement = .{ .node = 7, .next = 0 } },
            Node{ .atomic_token = 9 }, // Great
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("[wow, jam, time]!"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 9, .next = 0 } },
            Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 4, .next = 5 } },
            Node{ .callable_token = 3 }, // wow
            // [5]:
            Node{ .statement = .{ .node = 6, .next = 7 } },
            Node{ .callable_token = 7 }, // jam
            Node{ .statement = .{ .node = 8, .next = 0 } },
            Node{ .callable_token = 11 }, // time
            Node{ .postfix = .{ .operator = Operator.not, .node = 2 } },
            // [10]:
            .end,
        });
        // No errors in attempts to parse `callable`.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "[wow, jam, time]!",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("{Boo: 33, hoo: 123 + 44}-57"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 14, .next = 0 } },
            Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 6, .next = 7 } },
            Node{ .atomic_token = 3 }, // Boo
            // [5]:
            Node{ .atomic_token = 7 }, //33
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 4, .right = 5 } },
            Node{ .statement = .{ .node = 10, .next = 0 } },
            Node{ .callable_token = 11 }, // hoo
            Node{ .atomic_token = 15 }, // 123
            // [10]:
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 8, .right = 12 } },
            Node{ .atomic_token = 19 }, // 44
            Node{ .binary = .{ .operator = Operator.plus, .left = 9, .right = 11 } },
            Node{ .atomic_token = 25 }, // 57
            Node{ .binary = .{ .operator = Operator.minus, .left = 2, .right = 13 } },
            // [15]:
            .end,
        });
        // Attempts to parse generics or arguments for `callable` don't add errors:
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "{Boo: 33, hoo: 123 + 44}-57",
        });
    }
}

test "indent errors" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.appendSlice(&[_][]const u8{
            "H3",
            "   Only3spaces",
        });

        try std.testing.expectError(Parser.Error.syntax, parser.complete());

        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "H3",
            "   Only3spaces",
            "#@!~ indents should be 4-spaces wide",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.appendSlice(&[_][]const u8{
            "H5",
            "     Only5spaces",
        });

        try std.testing.expectError(Parser.Error.syntax, parser.complete());

        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "H5",
            "     Only5spaces",
            "#@!~~~ indents should be 4-spaces wide",
        });
    }
}

test "mixed commas and newlines with block" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 18, .next = 0 } },
        Node{ .callable_token = 1 }, // goober
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 4 } }, // {...}
        Node{ .statement = .{ .node = 5, .next = 0 } }, // first statement in {...}
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } }, // indent in {...}
        Node{ .statement = .{ .node = 7, .next = 8 } }, // first statement in indent
        Node{ .atomic_token = 5 }, // 405
        Node{ .statement = .{ .node = 9, .next = 10 } }, // second statement
        Node{ .atomic_token = 9 }, // 406
        // [10]:
        Node{ .statement = .{ .node = 11, .next = 12 } }, // third statement
        Node{ .atomic_token = 11 }, // 407
        Node{ .statement = .{ .node = 13, .next = 14 } }, // etc.
        Node{ .atomic_token = 13 }, // 408
        Node{ .statement = .{ .node = 15, .next = 16 } },
        // [15]:
        Node{ .atomic_token = 17 }, // 409
        Node{ .statement = .{ .node = 17, .next = 0 } },
        Node{ .atomic_token = 21 }, // 510
        Node{ .binary = .{ .operator = Operator.indent, .left = 2, .right = 3 } }, // goober {}
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
            "goober {",
            "    405, 406",
            "    407",
            "    408, 409,",
            "    510,",
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
            "goober",
            "{   405, 406",
            "    407",
            "    408, 409,",
            "    510,",
            "}",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&expected_nodes);
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
    {
        // implicit brace
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "goober",
            "    405, 406",
            "    407",
            "    408, 409,",
            "    510,",
            "",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete();

        // TODO: do we want to make this exactly like the explicit braces above?
        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 16, .next = 0 } },
            Node{ .callable_token = 1 }, // goober
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 6 } }, // first statement
            // [5]:
            Node{ .atomic_token = 3 }, // 405
            Node{ .statement = .{ .node = 7, .next = 8 } }, // second
            Node{ .atomic_token = 7 }, // 406
            Node{ .statement = .{ .node = 9, .next = 10 } }, // third
            Node{ .atomic_token = 9 }, // 407
            // [10]:
            Node{ .statement = .{ .node = 11, .next = 12 } },
            Node{ .atomic_token = 11 }, // 408
            Node{ .statement = .{ .node = 13, .next = 14 } },
            Node{ .atomic_token = 15 }, // 409
            Node{ .statement = .{ .node = 15, .next = 0 } },
            // [15]:
            Node{ .atomic_token = 19 }, // 510
            Node{ .binary = .{ .operator = Operator.indent, .left = 2, .right = 3 } },
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "trailing commas are ok" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("(C0, C1, C2,)"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 2, .next = 0 } },
            Node{ .enclosed = .{ .open = .paren, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 4, .next = 5 } },
            Node{ .atomic_token = 3 }, // C0
            // [5]:
            Node{ .statement = .{ .node = 6, .next = 7 } },
            Node{ .atomic_token = 7 }, // C1
            Node{ .statement = .{ .node = 8, .next = 0 } },
            Node{ .atomic_token = 11 }, // C2
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("[Bk0, Bk1,]-761"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 8, .next = 0 } },
            Node{ .enclosed = .{ .open = .bracket, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 4, .next = 5 } },
            Node{ .atomic_token = 3 }, // Bk0
            // [5]:
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 7 }, // Bk1
            Node{ .atomic_token = 15 }, // 761
            Node{ .binary = .{ .operator = Operator.minus, .left = 2, .right = 7 } },
            .end,
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("{Bc0,}+Abcdef"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 3 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 3 }, // Bc0
            // [5]:
            Node{ .atomic_token = 11 }, // Abcdef
            Node{ .binary = .{ .operator = Operator.plus, .left = 2, .right = 5 } },
            .end,
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

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // Whatever
            Node{ .callable_token = 5 }, // type1
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 3 } },
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

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // Writable_whatever
            Node{ .callable_token = 5 }, // type2
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 2, .right = 3 } },
            // [5]:
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

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // Declassign
            Node{ .callable_token = 5 }, // type_assign1
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 3 } },
            // [5]:
            Node{ .atomic_token = 9 }, // 12345
            Node{ .binary = .{ .operator = Operator.assign, .left = 4, .right = 5 } },
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

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // Oh_writable
            Node{ .callable_token = 5 }, // type_assign2
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 2, .right = 3 } },
            // [5]:
            Node{ .atomic_token = 9 },
            Node{ .binary = .{ .operator = Operator.assign, .left = 4, .right = 5 } },
            .end,
        });
        // No errors in attempts to parse `callable`.
        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "Oh_writable; type_assign2 = 7890",
        });
    }
}

test "parser declare and nested assigns" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.lines.append(try SmallString.init("D1: D2; D3"));

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // D1
            Node{ .atomic_token = 5 }, // D2
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 2, .right = 6 } },
            // [5]:
            Node{ .atomic_token = 9 }, // D3
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 3, .right = 5 } },
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

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 4, .next = 0 } },
            Node{ .atomic_token = 1 }, // X3
            Node{ .atomic_token = 5 }, // Y4
            Node{ .binary = .{ .operator = Operator.assign, .left = 2, .right = 6 } },
            // [5]:
            Node{ .atomic_token = 9 }, // 750
            Node{ .binary = .{ .operator = Operator.assign, .left = 3, .right = 5 } },
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

        try parser.complete();

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 1 }, // VarQ
            Node{ .callable_token = 5 }, // i32
            Node{ .binary = .{ .operator = Operator.declare_writable, .left = 2, .right = 3 } },
            // [5]:
            Node{ .atomic_token = 9 }, // Qu16
            Node{ .binary = .{ .operator = Operator.assign, .left = 4, .right = 8 } },
            Node{ .atomic_token = 13 }, // VarU
            Node{ .binary = .{ .operator = Operator.assign, .left = 5, .right = 12 } },
            Node{ .callable_token = 17 }, // i16
            // [10]:
            Node{ .binary = .{ .operator = Operator.declare_readonly, .left = 7, .right = 9 } },
            Node{ .atomic_token = 21 }, // 750
            Node{ .binary = .{ .operator = Operator.assign, .left = 10, .right = 11 } },
            .end,
        });
    }
}

// TODO: error tests, e.g., "cannot postfix this"
