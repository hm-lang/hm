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
        Node{ .prefix = .{ .operator = .op_decrement, .node = 7 } },
        Node{ .atomic_token = 3 }, // Why
        Node{ .atomic_token = 5 }, // Shy
        // [5]:
        Node{ .binary = .{ .operator = .op_access, .left = 3, .right = 4 } },
        Node{ .atomic_token = 7 }, // Spy
        Node{ .binary = .{ .operator = .op_access, .left = 5, .right = 6 } },
        Node{ .statement = .{ .node = 9, .next = 15 } },
        Node{ .prefix = .{ .operator = .op_not, .node = 14 } },
        // [10]:
        Node{ .atomic_token = 11 }, // Chai
        Node{ .atomic_token = 13 }, // Lie
        Node{ .binary = .{ .operator = .op_access, .left = 10, .right = 11 } },
        Node{ .atomic_token = 15 }, // Fry
        Node{ .binary = .{ .operator = .op_access, .left = 12, .right = 13 } },
        // [15]:
        Node{ .statement = .{ .node = 16, .next = 0 } },
        Node{ .prefix = .{ .operator = .op_not, .node = 22 } },
        Node{ .atomic_token = 19 }, // Knife
        Node{ .atomic_token = 21 }, // Fly
        Node{ .binary = .{ .operator = .op_access, .left = 17, .right = 18 } },
        // [20]:
        Node{ .atomic_token = 23 }, // Nigh
        Node{ .binary = .{ .operator = .op_access, .left = 19, .right = 20 } },
        Node{ .postfix = .{ .operator = .op_not, .node = 21 } },
        .end,
    });
    // No tampering done with the file, i.e., no errors.
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
        Node{ .prefix = .{ .operator = .op_not, .node = 7 } },
        Node{ .atomic_token = 7 }, // Berry
        // [5]:
        Node{ .atomic_token = 9 }, // Cantaloupe
        Node{ .binary = .{ .operator = .op_access, .left = 4, .right = 5 } },
        Node{ .postfix = .{ .operator = .op_decrement, .node = 6 } },
        Node{ .binary = .{ .operator = .op_multiply, .left = 2, .right = 3 } },
        Node{ .atomic_token = 15 }, // 500
        // [10]:
        Node{ .binary = .{ .operator = .op_plus, .left = 8, .right = 9 } },
        Node{ .statement = .{ .node = 18, .next = 0 } },
        Node{ .prefix = .{ .operator = .op_decrement, .node = 15 } },
        Node{ .atomic_token = 19 }, // Xeno
        Node{ .atomic_token = 21 }, // Yak
        // [15]:
        Node{ .binary = .{ .operator = .op_access, .left = 13, .right = 14 } },
        Node{ .postfix = .{ .operator = .op_not, .node = 12 } },
        Node{ .atomic_token = 27 }, // 3000
        Node{ .binary = .{ .operator = .op_minus, .left = 16, .right = 20 } },
        Node{ .atomic_token = 31 }, // Zelda
        // [20]:
        Node{ .binary = .{ .operator = .op_multiply, .left = 17, .right = 19 } },
        .end,
    });
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
        Node{ .prefix = .{ .operator = .op_increment, .node = 3 } },
        Node{ .atomic_token = 3 }, // Theta
        Node{ .atomic_token = 7 }, // Beta
        // [5]:
        Node{ .binary = .{ .operator = .op_multiply, .left = 2, .right = 4 } },
        Node{ .statement = .{ .node = 10, .next = 11 } },
        Node{ .atomic_token = 9 }, // Zeta
        Node{ .prefix = .{ .operator = .op_increment, .node = 9 } },
        Node{ .atomic_token = 15 }, // Woga
        // [10]:
        Node{ .binary = .{ .operator = .op_multiply, .left = 7, .right = 8 } },
        Node{ .statement = .{ .node = 15, .next = 16 } },
        Node{ .atomic_token = 17 }, // Yodus
        Node{ .postfix = .{ .operator = .op_decrement, .node = 12 } },
        Node{ .atomic_token = 23 }, // Spatula
        // [15]:
        Node{ .binary = .{ .operator = .op_multiply, .left = 13, .right = 14 } },
        Node{ .statement = .{ .node = 19, .next = 0 } },
        Node{ .atomic_token = 25 }, // Wobdash
        Node{ .atomic_token = 29 }, // Flobsmash
        Node{ .binary = .{ .operator = .op_multiply, .left = 17, .right = 20 } },
        // [20]:
        Node{ .postfix = .{ .operator = .op_decrement, .node = 18 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
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
        Node{ .binary = .{ .operator = .op_access, .left = 2, .right = 3 } },
        // [5]:
        Node{ .postfix = .{ .operator = .op_decrement, .node = 4 } },
        Node{ .postfix = .{ .operator = .op_not, .node = 5 } },
        Node{ .statement = .{ .node = 8, .next = 0 } },
        Node{ .prefix = .{ .operator = .op_not, .node = 9 } },
        Node{ .prefix = .{ .operator = .op_increment, .node = 12 } },
        // [10]:
        Node{ .atomic_token = 13 }, // Def
        Node{ .atomic_token = 15 }, // Uvw
        Node{ .binary = .{ .operator = .op_access, .left = 10, .right = 11 } },
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
        Node{ .prefix = .{ .operator = .op_lambda2, .node = 3 } },
        Node{ .atomic_token = 3 }, // Yammer
        Node{ .atomic_token = 7 }, // Zen
        // [5]:
        Node{ .binary = .{ .operator = .op_multiply, .left = 2, .right = 7 } },
        Node{ .postfix = .{ .operator = .op_increment, .node = 4 } },
        Node{ .postfix = .{ .operator = .op_not, .node = 6 } },
        Node{ .statement = .{ .node = 16, .next = 0 } },
        Node{ .prefix = .{ .operator = .op_not, .node = 10 } },
        // [10]:
        Node{ .prefix = .{ .operator = .op_decrement, .node = 14 } },
        Node{ .prefix = .{ .operator = .op_lambda1, .node = 12 } },
        Node{ .atomic_token = 19 }, // Oh
        Node{ .atomic_token = 21 }, // Great
        Node{ .binary = .{ .operator = .op_access, .left = 11, .right = 13 } },
        // [15]:
        Node{ .atomic_token = 25 }, // Hessian
        Node{ .binary = .{ .operator = .op_multiply, .left = 9, .right = 15 } },
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
        Node{ .binary = .{ .operator = .op_access, .left = 2, .right = 3 } },
        // [5]:
        Node{ .statement = .{ .node = 9, .next = 10 } },
        Node{ .atomic_token = 5 }, // Sci
        Node{ .atomic_token = 7 }, // Fi
        Node{ .binary = .{ .operator = .op_access, .left = 6, .right = 7 } },
        Node{ .postfix = .{ .operator = .op_increment, .node = 8 } },
        // [10]:
        Node{ .statement = .{ .node = 16, .next = 0 } },
        Node{ .atomic_token = 11 }, // Kite
        Node{ .atomic_token = 13 }, // Sty
        Node{ .binary = .{ .operator = .op_access, .left = 11, .right = 12 } },
        Node{ .atomic_token = 15 }, // Five
        // [15]:
        Node{ .binary = .{ .operator = .op_access, .left = 13, .right = 14 } },
        Node{ .postfix = .{ .operator = .op_not, .node = 15 } },
        .end,
    });
    try parser.tokenizer.file.expectEqualsSlice(&file_slice);
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
        Node{ .binary = .{ .operator = .op_multiply, .left = 2, .right = 3 } },
        // [5]:
        Node{ .atomic_token = 9 }, // Epsilon
        Node{ .binary = .{ .operator = .op_plus, .left = 4, .right = 5 } },
        Node{ .statement = .{ .node = 10, .next = 0 } },
        Node{ .atomic_token = 11 }, // Panko
        Node{ .atomic_token = 15 }, // K_panko
        // [10]:
        Node{ .binary = .{ .operator = .op_plus, .left = 8, .right = 12 } },
        Node{ .atomic_token = 19 }, // 1000
        Node{ .binary = .{ .operator = .op_multiply, .left = 9, .right = 11 } },
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
        Node{ .binary = .{ .operator = .op_multiply, .left = 2, .right = 3 } },
        // [5]:
        .end,
    });
}
