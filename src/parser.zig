const common = @import("common.zig");
const OwnedList = @import("owned_list.zig").OwnedList;
const SmallString = @import("string.zig").Small;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Token = @import("token.zig").Token;
const node_zig = @import("node.zig");
const Node = node_zig.Node;
const TokenIndex = node_zig.TokenIndex;
const NodeIndex = node_zig.NodeIndex;
const operator_zig = @import("operator.zig");
const Operator = operator_zig.Operator;
const Operation = operator_zig.Operation;

const std = @import("std");

const OwnedNodes = OwnedList(Node);
const OwnedNodeIndices = OwnedList(NodeIndex);

const ParserError = error{
    out_of_memory,
    out_of_statements,
    broken_invariant,
    syntax,
    unimplemented,
};

pub const Parser = struct {
    // The parser will free this at the end.
    tokenizer: Tokenizer = .{},
    nodes: OwnedNodes = OwnedNodes.init(),
    statement_indices: OwnedNodeIndices = OwnedNodeIndices.init(),
    valid_statement_count: usize = std.math.maxInt(usize),
    farthest_token_index: usize = 0,

    pub fn deinit(self: *Self) void {
        self.tokenizer.deinit();
        self.statement_indices.deinit();
        self.nodes.deinit();
    }

    pub fn at(self: *Self, statement_index: usize) ParserError!Node.Statement {
        if (statement_index >= self.valid_statement_count) {
            return ParserError.out_of_statements;
        }
        while (statement_index >= self.statement_indices.count()) {
            try self.addNextStatement();
            if (statement_index >= self.valid_statement_count) {
                return ParserError.out_of_statements;
            }
        }
        const node_index = self.statement_indices.at(statement_index) orelse {
            return ParserError.broken_invariant;
        };
        return switch (self.nodes.inBounds(node_index)) {
            .statement => |statement| statement,
            else => ParserError.broken_invariant,
        };
    }

    pub fn complete(self: *Self) ParserError!void {
        while (self.valid_statement_count > self.statement_indices.count()) {
            self.addNextStatement() catch |err| {
                if (err == ParserError.out_of_statements) {
                    break;
                }
                return err;
            };
        }
    }

    fn addNextStatement(self: *Self) ParserError!void {
        errdefer {
            self.valid_statement_count = self.statement_indices.count();
        }
        // So that `node == 0` appears to be invalid, append the
        // statement first, then its child nodes.  Only update
        // `statement_indices` after success.  Notice that this
        // will make the first statement appear to be invalid,
        // but only if it would be cross referenced in another
        // node, which it shouldn't be since it's the first.
        const statement_node_index = try self.justAppendNode(.end);
        self.nodes.set(statement_node_index, Node{
            .statement = try self.getNextStatement(),
        }) catch unreachable;
        self.statement_indices.append(statement_node_index) catch {
            return ParserError.out_of_memory;
        };
    }

    fn getNextStatement(self: *Self) ParserError!Node.Statement {
        const tab = switch (try self.peekToken(0)) {
            .spacing => |spacing| spacing.absolute,
            .end => return ParserError.out_of_statements,
            else => return ParserError.broken_invariant,
        };

        const node_index = try self.appendNextExpression(tab, Until.no_limit);
        try self.assertAndConsumeNextTokenIf(.newline, "expected newline");

        return .{ .tab = tab, .node = node_index };
    }

    fn appendNextExpression(self: *Self, tab: u16, until: Until) ParserError!NodeIndex {
        // The current left node which can interact with the next operation,
        // is the last element of `hierarchy`, with nesting all the way up
        // to the "root node" (`hierarchy.inBounds(0)`) which should be returned.
        var hierarchy = OwnedNodeIndices.init();
        _ = try self.appendNextStandaloneExpression(&hierarchy, tab);
        defer hierarchy.deinit();

        while (true) {
            const operation = try self.seekNextOperation(tab, until);
            if (operation.operator == .none) {
                return hierarchy.inBounds(0);
            }
            if (operation.type == .postfix) {
                try self.appendPostfixOperation(&hierarchy, operation);
            } else {
                const right_index = try self.appendNextStandaloneExpression(&hierarchy, tab);
                try self.appendInfixOperation(&hierarchy, operation, right_index);
            }
        }
    }

    // Returns the next postfix or infix operation.
    // Prefix operations are taken care of inside of `appendNextStandaloneExpression`.
    fn seekNextOperation(self: *Self, tab: u16, until: Until) ParserError!Operation {
        const restore_index = self.farthest_token_index;

        switch (try self.peekToken(0)) {
            // This was the last atom in the row.
            .newline => {
                // TODO: check if we had a double-indent on the next line and continue parsing if so.
                _ = tab;
                return .{ .operator = .none };
            },
            .end => return .{ .operator = .none },
            else => try self.assertAndConsumeNextTokenIf(.spacing, expected_spacing),
        }

        const operation: Operation = switch (try self.peekToken(0)) {
            .operator => |operator| blk: {
                if (operator.isInfixable()) {
                    self.farthest_token_index += 1;
                    break :blk .{ .operator = operator, .type = .infix };
                } else if (operator.isPostfixable()) {
                    self.farthest_token_index += 1;
                    break :blk .{ .operator = operator, .type = .postfix };
                } else {
                    std.debug.assert(operator.isPrefixable());
                    // Back up so that the next standalone expression starts at the
                    // spacing before this prefix:
                    self.farthest_token_index -= 1;
                    // Pretend that we have an operator before this prefix.
                    break :blk .{ .operator = .implicit_member_access, .type = .infix };
                }
            },
            else => blk: {
                // We encountered another realizable token, back up so that
                // we maintain the invariant that there's a space before the next real element.
                self.farthest_token_index -= 1;
                break :blk .{ .operator = .implicit_member_access, .type = .infix };
            },
        };

        if (until.shouldBreakBeforeOperation(operation)) {
            self.farthest_token_index = restore_index;
            return .{ .operator = .none };
        }
        return operation;
    }

    /// Adds an atom with possible prefix (but NOT postfix) operators.
    /// Includes things like `1.234`, `My_variable`, `+4.56`, `-7.89`,
    /// `++Index` or `!Countdown` as well.  For member access like
    /// `First_identifier Second_identifier`, just grab the first one.
    /// NOTE: do NOT add the returned index into `hierarchy`, we'll do that for you.
    // TODO: also include operations like `my_function(...)`
    fn appendNextStandaloneExpression(self: *Self, hierarchy: *OwnedNodeIndices, tab: u16) ParserError!NodeIndex {
        try self.assertAndConsumeNextTokenIf(.spacing, expected_spacing);

        switch (try self.peekToken(0)) {
            .starts_lower, .starts_upper, .number => {
                const atomic_index = try self.justAppendNode(Node{
                    .atomic_token = self.farthest_token_index,
                });
                self.farthest_token_index += 1;

                hierarchy.append(atomic_index) catch return ParserError.out_of_memory;
                return atomic_index;
            },
            .operator => |operator| {
                if (!operator.isPrefixable()) {
                    self.addTokenizerError("not a prefix operator");
                    return ParserError.syntax;
                }
                self.farthest_token_index += 1;
                // We need to parse a different way because we can't break the hierarchy invariant here.
                // Start with the prefix to maintain a rough left-to-right direction inside `self.nodes`.
                const prefix_index = try self.justAppendNode(Node{
                    .prefix = .{
                        .operator = operator,
                        .node = 0, // break the invariant here
                    },
                });
                // We need every operation *stronger* than this prefix to be attached to this prefix.
                const inner_index = try self.appendNextExpression(tab, Until.prefix_strength_wins(operator));
                switch (self.nodes.items()[prefix_index]) {
                    // restore the invariant:
                    .prefix => |*prefix| {
                        prefix.node = inner_index;
                    },
                    else => return ParserError.broken_invariant,
                }
                // We don't need to append `inner_index` because we know it will reappear.
                // It was stronger than `prefix_index` and so should never be split out.
                hierarchy.append(prefix_index) catch return ParserError.out_of_memory;
                return prefix_index;
            },
            else => {
                self.addTokenizerError("expected an expression");
                return ParserError.syntax;
            },
        }
    }

    fn appendPostfixOperation(self: *Self, hierarchy: *OwnedNodeIndices, operation: Operation) ParserError!void {
        const operation_precedence = operation.precedence(Operation.Compare.on_right);
        var hierarchy_index = hierarchy.count();
        var left_index: NodeIndex = 0;
        while (hierarchy_index > 0) {
            hierarchy_index -= 1;
            left_index = hierarchy.inBounds(hierarchy_index);
            const left_operation = self.nodeInBounds(left_index).operation();
            // lower precedence means higher priority.
            if (left_operation.isPostfix() or left_operation.precedence(Operation.Compare.on_left) <= operation_precedence) {
                // `left` has higher priority; we should continue up the hierarchy
                // until we find the spot that this new operation should take.
                _ = hierarchy.pop();
            } else {
                // `operation` has higher priority, so we need to invert the nodes a bit.
                // break an invariant here:
                const inner_index = self.nodeInBounds(left_index).swapRight(0) catch {
                    self.addTokenizerError("cannot postfix this");
                    return ParserError.syntax;
                };
                const next_index = try self.justAppendNode(.{ .postfix = .{
                    .operator = operation.operator,
                    .node = inner_index,
                } });
                // Restore the invariant.  Don't hold onto a reference to the `left_index` node
                // because it can be invalidated by appending (i.e., in the previous statement).
                _ = self.nodeInBounds(left_index).swapRight(next_index) catch unreachable;
                // Fix up the hierarchy at the end:
                hierarchy.append(next_index) catch return ParserError.out_of_memory;
                hierarchy.append(inner_index) catch return ParserError.out_of_memory;
                return;
            }
        }
        if (left_index == 0) {
            return ParserError.broken_invariant;
        }
        hierarchy.append(try self.justAppendNode(.{ .postfix = .{
            .operator = operation.operator,
            .node = left_index,
        } })) catch return ParserError.out_of_memory;
    }

    fn appendInfixOperation(self: *Self, hierarchy: *OwnedNodeIndices, operation: Operation, right_index: NodeIndex) ParserError!void {
        const operation_precedence = operation.precedence(Operation.Compare.on_right);
        var hierarchy_index = hierarchy.count();
        var left_index: NodeIndex = 0;
        while (hierarchy_index > 0) {
            hierarchy_index -= 1;
            left_index = hierarchy.inBounds(hierarchy_index);
            const left_operation = self.nodeInBounds(left_index).operation();
            // lower precedence means higher priority.
            if (left_operation.isPostfix() or left_operation.precedence(Operation.Compare.on_left) <= operation_precedence) {
                // `left` has higher priority; we should continue up the hierarchy
                // until we find the spot that this new operation should take.
                _ = hierarchy.pop();
            } else {
                // `operation` has higher priority, so we need to invert the nodes a bit.
                // break an invariant here:
                const inner_index = self.nodeInBounds(left_index).swapRight(0) catch {
                    self.addTokenizerError("cannot right-operate on this");
                    return ParserError.syntax;
                };
                const next_index = try self.justAppendNode(.{ .binary = .{
                    .operator = operation.operator,
                    .left = inner_index,
                    .right = right_index,
                } });
                // Restore the invariant.  Don't hold onto a reference to the `left_index` node
                // because it can be invalidated by appending (i.e., in the previous statement).
                _ = self.nodeInBounds(left_index).swapRight(next_index) catch unreachable;
                // Fix up the hierarchy at the end:
                hierarchy.append(next_index) catch return ParserError.out_of_memory;
                hierarchy.append(inner_index) catch return ParserError.out_of_memory;
                return;
            }
        }
        if (left_index == 0) {
            return ParserError.broken_invariant;
        }
        hierarchy.append(try self.justAppendNode(.{ .binary = .{
            .operator = operation.operator,
            .left = left_index,
            .right = right_index,
        } })) catch return ParserError.out_of_memory;
    }

    fn justAppendNode(self: *Self, node: Node) ParserError!NodeIndex {
        const index = self.nodes.count();
        self.nodes.append(node) catch {
            return ParserError.out_of_memory;
        };
        return index;
    }

    fn nextToken(self: *Self) ParserError!Token {
        const token = try self.peekToken(0);
        self.farthest_token_index += 1;
        return token;
    }

    fn peekToken(self: *Self, delta: usize) ParserError!Token {
        return self.tokenizer.at(self.farthest_token_index + delta) catch {
            // TODO: probably should distinguish between out of memory (rethrow)
            // and out of tokens => out_of_statements
            return ParserError.out_of_statements;
        };
    }

    fn assertAndConsumeNextTokenIf(self: *Self, expected_tag: Token.Tag, error_message: []const u8) ParserError!void {
        const next_token = try self.peekToken(0);
        if (next_token.tag() == expected_tag) {
            self.farthest_token_index += 1;
            return;
        }
        common.debugStderr.print("actual tag is {d}\n", .{@intFromEnum(next_token.tag())}) catch {};
        self.addTokenizerError(error_message);
        return ParserError.syntax;
    }

    fn addTokenizerError(self: *Self, error_message: []const u8) void {
        self.tokenizer.addErrorAt(self.farthest_token_index, error_message);
    }

    pub fn printTokenDebugInfo(self: *Self) void {
        self.tokenizer.printDebugInfoAt(self.farthest_token_index);
    }

    fn nodeInBounds(self: *Self, index: usize) *Node {
        return &self.nodes.items()[index];
    }

    const Self = @This();
};

const UntilTag = enum {
    precedence,
};

/// Necessary for prefix operations.
const Until = union(UntilTag) {
    precedence: u8,

    pub const no_limit: Self = .{ .precedence = 255 };

    /// Will keep going until this prefix operator should win.
    pub fn prefix_strength_wins(operator: Operator) Self {
        const operation = Operation{ .operator = operator, .type = .prefix };
        return .{ .precedence = operation.precedence(Operation.Compare.on_left) };
    }

    pub fn shouldBreakBeforeOperation(self: Self, on_right: Operation) bool {
        switch (self) {
            .precedence => |left_precedence| {
                const right_precedence = on_right.precedence(Operation.Compare.on_right);
                // TODO: this should maybe be <= ??
                return left_precedence < right_precedence;
            },
        }
    }

    pub fn equals(a: Self, b: Self) bool {
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        if (tag_a != tag_b) return false;

        const info = switch (@typeInfo(Self)) {
            .Union => |info| info,
            else => unreachable,
        };
        inline for (info.fields) |field_info| {
            if (@field(Tag, field_info.name) == tag_a) {
                const SubField = @TypeOf(@field(a, field_info.name));
                if (std.meta.hasMethod(SubField, "equals")) {
                    return @field(a, field_info.name).equals(@field(b, field_info.name));
                } else {
                    return @field(a, field_info.name) == @field(b, field_info.name);
                }
            }
        }
        return false;
    }

    pub fn printLine(self: Self, writer: anytype) !void {
        try self.print(writer);
        try writer.print("\n", .{});
    }

    pub fn print(self: Self, writer: anytype) !void {
        switch (self) {
            .precedence => |precedence| {
                try writer.print("Until{{ .precedence = {d} }}", .{precedence});
            },
        }
    }

    pub const Tag = UntilTag;
    const Self = @This();
};

const expected_spacing = "expected spacing between each identifier";

test "parser simple expressions" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("3.456"));
    try parser.tokenizer.file.lines.append(try SmallString.init("    hello_you"));
    try parser.tokenizer.file.lines.append(try SmallString.init("+1.234"));
    try parser.tokenizer.file.lines.append(try SmallString.init("  -5.678"));
    try parser.tokenizer.file.lines.append(try SmallString.init("    $$$Foe"));
    try parser.tokenizer.file.lines.append(try SmallString.init("        Fum--"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 1, .tab = 0 } },
        Node{ .atomic_token = 1 }, // 3.456
        Node{ .statement = .{ .node = 3, .tab = 4 } },
        Node{ .atomic_token = 4 }, // hello_you
        Node{ .statement = .{ .node = 5, .tab = 0 } },
        // [5]:
        Node{ .prefix = .{ .operator = Operator.plus, .node = 6 } },
        Node{ .atomic_token = 9 }, // 1.234
        Node{ .statement = .{ .node = 8, .tab = 2 } },
        Node{ .prefix = .{ .operator = Operator.minus, .node = 9 } },
        Node{ .atomic_token = 14 }, // 5.678
        // [10]:
        Node{ .statement = .{ .node = 11, .tab = 4 } },
        Node{ .prefix = .{ .operator = Operator.lambda3, .node = 12 } },
        Node{ .atomic_token = 19 }, // Foe
        Node{ .statement = .{ .node = 15, .tab = 8 } },
        Node{ .atomic_token = 22 }, // Fum
        // [15]:
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 14 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        2,
        4,
        7,
        10,
        13,
    });
    try std.testing.expectEqual(Node.Statement{ .node = 1, .tab = 0 }, try parser.at(0));
    try std.testing.expectEqual(Node.Statement{ .node = 3, .tab = 4 }, try parser.at(1));
    try std.testing.expectEqual(Node.Statement{ .node = 5, .tab = 0 }, try parser.at(2));
    try std.testing.expectEqual(Node.Statement{ .node = 8, .tab = 2 }, try parser.at(3));
    try std.testing.expectEqual(Node.Statement{ .node = 11, .tab = 4 }, try parser.at(4));
    try std.testing.expectEqual(Node.Statement{ .node = 15, .tab = 8 }, try parser.at(5));
}

test "parser multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Wompus * 3.14"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        Node{ .statement = .{ .node = 3, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Wompus
        Node{ .atomic_token = 5 }, // 3.14
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 2 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
    });
}

test "parser simple (and postfix) implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Pi Sky"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Sci Fi++"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Kite Sty Five!"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 3, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Pi
        Node{ .atomic_token = 3 }, // Sky
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 1, .right = 2 } },
        Node{ .statement = .{ .node = 8, .tab = 0 } },
        // [5]:
        Node{ .atomic_token = 6 }, // Sci
        Node{ .atomic_token = 8 }, // Fi
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 5, .right = 6 } },
        Node{ .postfix = .{ .operator = Operator.increment, .node = 7 } },
        Node{ .statement = .{ .node = 15, .tab = 0 } },
        // [10]:
        Node{ .atomic_token = 13 }, // Kite
        Node{ .atomic_token = 15 }, // Sty
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 10, .right = 11 } },
        Node{ .atomic_token = 17 }, // Five
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 12, .right = 13 } },
        // [15]:
        Node{ .postfix = .{ .operator = Operator.not, .node = 14 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        4,
        9,
    });
}

test "parser complicated (and prefix) implicit member access" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("--Why Shy Spy"));
    try parser.tokenizer.file.lines.append(try SmallString.init("!Chai Lie Fry"));
    try parser.tokenizer.file.lines.append(try SmallString.init("!Knife Fly Nigh!"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 1, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 6 } },
        Node{ .atomic_token = 3 }, // Why
        Node{ .atomic_token = 5 }, // Shy
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 2, .right = 3 } },
        // [5]:
        Node{ .atomic_token = 7 }, // Spy
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 4, .right = 5 } },
        Node{ .statement = .{ .node = 8, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 13 } },
        Node{ .atomic_token = 12 }, // Chai
        // [10]:
        Node{ .atomic_token = 14 }, // Lie
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 9, .right = 10 } },
        Node{ .atomic_token = 16 }, // Fry
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 11, .right = 12 } },
        Node{ .statement = .{ .node = 15, .tab = 0 } },
        // [15]:
        Node{ .prefix = .{ .operator = Operator.not, .node = 21 } },
        Node{ .atomic_token = 21 }, // Knife
        Node{ .atomic_token = 23 }, // Fly
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 16, .right = 17 } },
        Node{ .atomic_token = 25 }, // Nigh
        // [20]:
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 18, .right = 19 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 20 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        7,
        14,
    });
}

test "simple prefix/postfix operators with multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("++Theta * Beta"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Zeta * ++Woga"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Yodus-- * Spatula"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Wobdash * Flobsmash--"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 4, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 2 } },
        Node{ .atomic_token = 3 }, // Theta
        Node{ .atomic_token = 7 }, // Beta
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 3 } },
        // [5]:
        Node{ .statement = .{ .node = 9, .tab = 0 } },
        Node{ .atomic_token = 10 }, // Zeta
        Node{ .prefix = .{ .operator = Operator.increment, .node = 8 } },
        Node{ .atomic_token = 16 }, // Woga
        Node{ .binary = .{ .operator = Operator.multiply, .left = 6, .right = 7 } },
        // [10]:
        Node{ .statement = .{ .node = 14, .tab = 0 } },
        Node{ .atomic_token = 19 }, // Yodus
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 11 } },
        Node{ .atomic_token = 25 }, // Spatula
        Node{ .binary = .{ .operator = Operator.multiply, .left = 12, .right = 13 } },
        // [15]:
        Node{ .statement = .{ .node = 18, .tab = 0 } },
        Node{ .atomic_token = 28 }, // Wobdash
        Node{ .atomic_token = 32 }, // Flobsmash
        Node{ .binary = .{ .operator = Operator.multiply, .left = 16, .right = 19 } },
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 17 } },
        // [20]:
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        5,
        10,
        15,
    });
}

test "complicated prefix/postfix operators with addition/multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Apple * !Berry Cantaloupe-- + 500"));
    try parser.tokenizer.file.lines.append(try SmallString.init("--Xeno Yak! - 3000 * Zelda"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 9, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Apple
        Node{ .prefix = .{ .operator = Operator.not, .node = 6 } },
        Node{ .atomic_token = 7 }, // Berry
        Node{ .atomic_token = 9 }, // Cantaloupe
        // [5]:
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 3, .right = 4 } },
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 5 } },
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 2 } },
        Node{ .atomic_token = 15 }, // 500
        Node{ .binary = .{ .operator = Operator.plus, .left = 7, .right = 8 } },
        // [10]:
        Node{ .statement = .{ .node = 17, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 14 } },
        Node{ .atomic_token = 20 }, // Xeno
        Node{ .atomic_token = 22 }, // Yak
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 12, .right = 13 } },
        // [15]:
        Node{ .postfix = .{ .operator = Operator.not, .node = 11 } },
        Node{ .atomic_token = 28 }, // 3000
        Node{ .binary = .{ .operator = Operator.minus, .left = 15, .right = 19 } },
        Node{ .atomic_token = 32 }, // Zelda
        Node{ .binary = .{ .operator = Operator.multiply, .left = 16, .right = 18 } },
        // [20]:
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        10,
    });
}

test "nested prefix/postfix operators" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Abc Xyz-- !"));
    try parser.tokenizer.file.lines.append(try SmallString.init("! ++Def Uvw"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 5, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Abc
        Node{ .atomic_token = 3 }, // Xyz
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 1, .right = 2 } },
        Node{ .postfix = .{ .operator = Operator.decrement, .node = 3 } },
        // [5]:
        Node{ .postfix = .{ .operator = Operator.not, .node = 4 } },
        Node{ .statement = .{ .node = 7, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 8 } },
        Node{ .prefix = .{ .operator = Operator.increment, .node = 11 } },
        Node{ .atomic_token = 14 }, // Def
        // [10]:
        Node{ .atomic_token = 16 }, // Uvw
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 9, .right = 10 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        6,
    });
}

test "deeply nested prefix/postfix operators" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("$$Yammer * Zen++!"));
    try parser.tokenizer.file.lines.append(try SmallString.init("!--$Oh Great * Hessian"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 4, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.lambda2, .node = 2 } },
        Node{ .atomic_token = 3 }, // Yammer
        Node{ .atomic_token = 7 }, // Zen
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 6 } },
        // [5]:
        Node{ .postfix = .{ .operator = Operator.increment, .node = 3 } },
        Node{ .postfix = .{ .operator = Operator.not, .node = 5 } },
        Node{ .statement = .{ .node = 15, .tab = 0 } },
        Node{ .prefix = .{ .operator = Operator.not, .node = 9 } },
        Node{ .prefix = .{ .operator = Operator.decrement, .node = 13 } },
        // [10]:
        Node{ .prefix = .{ .operator = Operator.lambda1, .node = 11 } },
        Node{ .atomic_token = 20 }, // Oh
        Node{ .atomic_token = 22 }, // Great
        Node{ .binary = .{ .operator = Operator.implicit_member_access, .left = 10, .right = 12 } },
        Node{ .atomic_token = 26 }, // Hessian
        // [15]:
        Node{ .binary = .{ .operator = Operator.multiply, .left = 8, .right = 14 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        7,
    });
}

test "order of operations with addition and multiplication" {
    var parser: Parser = .{};
    defer parser.deinit();
    errdefer {
        parser.tokenizer.file.print(common.debugStderr) catch {};
    }
    try parser.tokenizer.file.lines.append(try SmallString.init("Alpha * Gamma + Epsilon"));
    try parser.tokenizer.file.lines.append(try SmallString.init("Panko + K_panko * 1000"));

    try parser.complete();

    try parser.nodes.expectEqualsSlice(&[_]Node{
        // [0]:
        Node{ .statement = .{ .node = 5, .tab = 0 } },
        Node{ .atomic_token = 1 }, // Alpha
        Node{ .atomic_token = 5 }, // Gamma
        Node{ .binary = .{ .operator = Operator.multiply, .left = 1, .right = 2 } },
        Node{ .atomic_token = 9 }, // Epsilon
        // [5]:
        Node{ .binary = .{ .operator = Operator.plus, .left = 3, .right = 4 } },
        Node{ .statement = .{ .node = 9, .tab = 0 } },
        Node{ .atomic_token = 12 }, // Panko
        Node{ .atomic_token = 16 }, // K_panko
        Node{ .binary = .{ .operator = Operator.plus, .left = 7, .right = 11 } },
        // [10]:
        Node{ .atomic_token = 20 }, // 1000
        Node{ .binary = .{ .operator = Operator.multiply, .left = 8, .right = 10 } },
        .end,
    });
    try parser.statement_indices.expectEqualsSlice(&[_]NodeIndex{
        0,
        6,
    });
}

// TODO: error tests, e.g., "cannot postfix this"
