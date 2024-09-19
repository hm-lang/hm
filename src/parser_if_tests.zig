const common = @import("common.zig");
const DoNothing = @import("do_nothing.zig").DoNothing;
const Node = @import("node.zig").Node;
const Parser = @import("parser.zig").Parser;

const std = @import("std");

// TODO
// we could introduce a new capture operator (|> or &> or ?>):
// i don't know if i like this as &> or ?> (because ? is a pretty strong operator),
// but it does make the most sense from a sentence standpoint.
//&|if Some_condition |> Then:
//&|    do_stuff()
//&|    Then exit(3)
// you could use it in a function
//&|my_function(X: if Some_condition |> Then: {do_stuff(), Then exit(3)})
// maybe use `?>` if the thing is nullable (or an error), and `|>` for `Then` blocks
//&|if Nullable ?> Non_null:
//&|    doStuff
test "parsing nested if statements" {
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
        Node{ .conditional = .{ .condition = 8, .if_node = 9, .else_node = 15 } }, // inner if statement
        // [15]:
        Node{ .enclosed = .{ .open = .brace, .tab = 4, .start = 16 } }, // inner else brace
        Node{ .statement = .{ .node = 17, .next = 0 } }, // inner else statement
        Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 18 } }, // inner else indent
        Node{ .statement = .{ .node = 19, .next = 20 } }, // inner else indent first statement
        Node{ .atomic_token = 25 }, // Betty
        // [20]:
        Node{ .statement = .{ .node = 21, .next = 0 } }, // inner else indent second statement
        Node{ .atomic_token = 27 }, // Aetty
        Node{ .conditional = .{ .condition = 3, .if_node = 4, .else_node = 0 } }, // root if
        Node{ .binary = .{ .operator = .plus, .left = 2, .right = 25 } }, // 5 + ...
        Node{ .atomic_token = 35 }, // 3
        // [25]:
        Node{ .binary = .{ .operator = .multiply, .left = 22, .right = 24 } }, // root if * 3
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
            "5 + if Skelluton {",
            "    if Brandenborg {",
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
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "5 + if Skelluton",
            "{   if Brandenborg",
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
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        const file_slice = [_][]const u8{
            "5 + 3 * if Skelluton",
            "    if Brandenborg",
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
            Node{ .binary = .{ .operator = .plus, .left = 2, .right = 19 } }, // 5 + ...
            // [5]:
            Node{ .atomic_token = 11 }, // Skelluton
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 7 } }, // root if indent
            Node{ .statement = .{ .node = 12, .next = 0 } }, // root if indent statement
            Node{ .atomic_token = 15 }, // Brandenborg
            Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 10 } }, // inner if indent
            // [10]:
            Node{ .statement = .{ .node = 11, .next = 0 } }, // inner if statement
            Node{ .atomic_token = 17 }, // Chetty
            Node{ .conditional = .{ .condition = 8, .if_node = 9, .else_node = 13 } }, // inner if
            Node{ .enclosed = .{ .open = .none, .tab = 8, .start = 14 } }, // inner else
            Node{ .statement = .{ .node = 15, .next = 16 } }, // inner else first statement
            // [15]:
            Node{ .atomic_token = 21 }, // Betty
            Node{ .statement = .{ .node = 17, .next = 0 } }, // inner else second statement
            Node{ .atomic_token = 23 }, // Aetty
            Node{ .conditional = .{ .condition = 5, .if_node = 6, .else_node = 0 } }, // root if
            Node{ .binary = .{ .operator = .multiply, .left = 3, .right = 18 } }, // 3 * if...
            // [20]:
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parsing if errors" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.appendSlice(&[_][]const u8{
            "    if",
        });

        try std.testing.expectError(Parser.Error.syntax_panic, parser.complete(DoNothing{}));

        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "    if",
            "#@! ^~ need condition for `if` or indented block after",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.appendSlice(&[_][]const u8{
            "    if {}",
        });

        try std.testing.expectError(Parser.Error.syntax_panic, parser.complete(DoNothing{}));

        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "    if {}",
            "#@! ^~ need condition for `if` or indented block after",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.appendSlice(&[_][]const u8{
            "        if Bardor",
        });

        try std.testing.expectError(Parser.Error.syntax_panic, parser.complete(DoNothing{}));

        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "        if Bardor",
            "#@!     ^~ need condition for `if` or indented block after",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.appendSlice(&[_][]const u8{
            "if Spr33 * 3331",
        });

        try std.testing.expectError(Parser.Error.syntax_panic, parser.complete(DoNothing{}));

        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "if Spr33 * 3331",
            "#@! need condition for `if` or indented block after",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.appendSlice(&[_][]const u8{
            "    if Condit10n",
            "    {   ok10",
            "    }",
            "    elif",
        });

        try std.testing.expectError(Parser.Error.syntax_panic, parser.complete(DoNothing{}));

        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "    if Condit10n",
            "    {   ok10",
            "    }",
            "    elif",
            "#@! ^~~~ need condition for `elif` or indented block after",
        });
    }
}

test "parsing elif and else without an if errors" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.appendSlice(&[_][]const u8{
            "{   else + 3",
            "}",
        });

        try std.testing.expectError(Parser.Error.syntax_panic, parser.complete(DoNothing{}));

        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "{   else + 3",
            "#@! ^~~~ need `if` before `else` or `elif`",
            "}",
        });
    }
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.appendSlice(&[_][]const u8{
            "        elif 5 {3}",
        });

        try std.testing.expectError(Parser.Error.syntax_panic, parser.complete(DoNothing{}));

        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "        elif 5 {3}",
            "#@!     ^~~~ need `if` before `else` or `elif`",
        });
    }
}

test "parsing else without a block error" {
    {
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
        }
        try parser.tokenizer.file.appendSlice(&[_][]const u8{
            "    if Zxy {2}",
            "    else 6",
        });

        try std.testing.expectError(Parser.Error.syntax_panic, parser.complete(DoNothing{}));

        try parser.tokenizer.file.expectEqualsSlice(&[_][]const u8{
            "    if Zxy {2}",
            "    else 6",
            "#@! ^~~~ need indented block after `else`",
        });
    }
}

// TODO: if/elif/else
test "parsing if/else statements as part of an expression " {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 15, .next = 0 } }, // root statement
        Node{ .atomic_token = 1 }, // 500
        Node{ .atomic_token = 7 }, // Condition500
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 5 } }, // if brace
        // [5]:
        Node{ .statement = .{ .node = 6, .next = 0 } }, // if statement
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 7 } }, // if indent
        Node{ .statement = .{ .node = 8, .next = 0 } }, // indented if statement
        Node{ .atomic_token = 11 }, // If_block_500
        Node{ .conditional = .{ .condition = 3, .if_node = 4, .else_node = 10 } }, // if/else
        // [10]:
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 11 } }, // else brace
        Node{ .statement = .{ .node = 12, .next = 0 } }, // else statement
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 13 } }, // else indent
        Node{ .statement = .{ .node = 14, .next = 0 } }, // indented else statement
        Node{ .atomic_token = 19 }, // Else_block_500
        // [15]:
        Node{ .binary = .{ .operator = .plus, .left = 2, .right = 17 } }, // 500 + ...
        Node{ .atomic_token = 25 }, // 3500
        Node{ .binary = .{ .operator = .multiply, .left = 9, .right = 16 } }, // (if{}else{}) * 3500
        .end,
    };
    {
        // Explicit brace, one-true-brace style
        var parser: Parser = .{};
        defer parser.deinit();
        errdefer {
            common.debugPrint("# file:\n", parser.tokenizer.file);
            common.debugPrint("# nodes:\n", parser.nodes);
        }
        const file_slice = [_][]const u8{
            "500 + if Condition500 {",
            "    If_block_500",
            "} else {",
            "    Else_block_500",
            "} * 3500",
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
            common.debugPrint("# nodes:\n", parser.nodes);
        }
        const file_slice = [_][]const u8{
            "500 + if Condition500",
            "{   If_block_500",
            "}",
            "else",
            "{   Else_block_500",
            "} * 3500",
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
            common.debugPrint("# nodes:\n", parser.nodes);
        }
        const file_slice = [_][]const u8{
            "500 + if Condition500",
            "    If_block_500",
            "else",
            "    Else_block_500",
            // Notice we're not doing a *3500 here because it would be hard
            // to syntactically do that without parentheses.
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 11, .next = 0 } },
            Node{ .atomic_token = 1 }, // 500
            Node{ .atomic_token = 7 }, // Condition500
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 5 } }, // if indent
            // [5]:
            Node{ .statement = .{ .node = 6, .next = 0 } }, // if indented statement
            Node{ .atomic_token = 9 }, // If_block_500
            Node{ .conditional = .{ .condition = 3, .if_node = 4, .else_node = 8 } }, // if/else
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 9 } }, // else indent
            Node{ .statement = .{ .node = 10, .next = 0 } }, // else indented statement
            // [10]:
            Node{ .atomic_token = 13 }, // Else_block_500
            Node{ .binary = .{ .operator = .plus, .left = 2, .right = 7 } }, // 500 + ...
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parsing if/else statements" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 8, .next = 0 } },
        Node{ .atomic_token = 3 }, // If_else
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 4 } }, // if {...} brace
        Node{ .statement = .{ .node = 5, .next = 0 } }, // inner {...} if statement
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } }, // if indent
        Node{ .statement = .{ .node = 7, .next = 0 } }, // first statement in if indent
        Node{ .atomic_token = 7 }, // If_inner
        Node{ .conditional = .{ .condition = 2, .if_node = 3, .else_node = 9 } }, // if/else
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 10 } }, // else brace
        // [10]:
        Node{ .statement = .{ .node = 11, .next = 0 } }, // inner {...} else statement
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 12 } }, // else indent
        Node{ .statement = .{ .node = 13, .next = 0 } }, // first statement in else indent
        Node{ .callable_token = 15 }, // else_inner
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
            "if If_else {",
            "    If_inner",
            "} else {",
            "    else_inner",
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
            "if If_else",
            "{   If_inner",
            "}",
            "else",
            "{   else_inner",
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
            "if If_else",
            "    If_inner",
            "else",
            "    else_inner",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 3 }, // If_else
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } }, // if indent
            Node{ .statement = .{ .node = 5, .next = 0 } }, // first statement in if indent
            // [5]:
            Node{ .atomic_token = 5 }, // If_inner
            Node{ .conditional = .{ .condition = 2, .if_node = 3, .else_node = 7 } }, // if/else
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 8 } }, // else indent
            Node{ .statement = .{ .node = 9, .next = 0 } }, // first statement in else indent
            Node{ .callable_token = 9 }, // else_inner
            // [10]:
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parsing if/elif statements" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } }, // root block
        Node{ .statement = .{ .node = 8, .next = 0 } }, // first/only root statement
        Node{ .atomic_token = 3 }, // Maybe_true
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 4 } }, // if {...}
        Node{ .statement = .{ .node = 5, .next = 0 } }, // inside if statement
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } }, // if indent
        Node{ .statement = .{ .node = 7, .next = 0 } }, // if indented statement
        Node{ .atomic_token = 7 }, // Go_for_it33
        Node{ .conditional = .{ .condition = 2, .if_node = 3, .else_node = 15 } }, // if/elif
        Node{ .atomic_token = 13 }, // Not_true
        // [10]:
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 11 } }, // elif {...}
        Node{ .statement = .{ .node = 12, .next = 0 } }, // inside elif statement
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 13 } }, // elif indent
        Node{ .statement = .{ .node = 14, .next = 0 } }, // elif indented statement
        Node{ .callable_token = 17 }, // go_for_it22
        // [15]:
        Node{ .conditional = .{ .condition = 9, .if_node = 10, .else_node = 0 } }, // elif
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
            "if Maybe_true {",
            "    Go_for_it33",
            "} elif Not_true {",
            "    go_for_it22",
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
            "if Maybe_true",
            "{   Go_for_it33",
            "}",
            "elif Not_true",
            "{   go_for_it22",
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
            "if Maybe_true",
            "    Go_for_it33",
            "elif Not_true",
            "    go_for_it22",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } },
            Node{ .atomic_token = 3 }, // Maybe_true
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } },
            Node{ .statement = .{ .node = 5, .next = 0 } },
            // [5]:
            Node{ .atomic_token = 5 }, // Go_for_it33
            Node{ .conditional = .{ .condition = 2, .if_node = 3, .else_node = 11 } },
            Node{ .atomic_token = 9 }, // Not_true
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 9 } },
            Node{ .statement = .{ .node = 10, .next = 0 } },
            // [10]:
            Node{ .callable_token = 11 }, // go_for_it22
            Node{ .conditional = .{ .condition = 7, .if_node = 8, .else_node = 0 } },
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}

test "parsing simple if statements" {
    const expected_nodes = [_]Node{
        // [0]:
        Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
        Node{ .statement = .{ .node = 8, .next = 0 } }, // first statement
        Node{ .atomic_token = 3 }, // Really_true
        Node{ .enclosed = .{ .open = .brace, .tab = 0, .start = 4 } }, // {...}
        Node{ .statement = .{ .node = 5, .next = 0 } }, // statement inside {}
        // [5]:
        Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 6 } }, // indent inside {}
        Node{ .statement = .{ .node = 7, .next = 0 } }, // first statement in indent
        Node{ .callable_token = 7 }, // do_something_nice
        Node{ .conditional = .{ .condition = 2, .if_node = 3, .else_node = 0 } },
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
            "if Really_true {",
            "    do_something_nice",
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
            "if Really_true",
            "{   do_something_nice",
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
            "if Really_true",
            "    do_something_nice",
        };
        try parser.tokenizer.file.appendSlice(&file_slice);

        try parser.complete(DoNothing{});

        try parser.nodes.expectEqualsSlice(&[_]Node{
            // [0]:
            Node{ .enclosed = .{ .open = .none, .tab = 0, .start = 1 } },
            Node{ .statement = .{ .node = 6, .next = 0 } }, // if statement
            Node{ .atomic_token = 3 }, // Really_true
            Node{ .enclosed = .{ .open = .none, .tab = 4, .start = 4 } }, // indent
            Node{ .statement = .{ .node = 5, .next = 0 } },
            // [5]:
            Node{ .callable_token = 5 }, // do_something_nice
            Node{ .conditional = .{ .condition = 2, .if_node = 3, .else_node = 0 } },
            .end,
        });
        // No tampering done with the file, i.e., no errors.
        try parser.tokenizer.file.expectEqualsSlice(&file_slice);
    }
}
