const Offset = @import("offset.zig").Offset;

const NodeTag = enum {
    atomic,
    left_operation,
    right_operation,
    binary_operation,
};

pub const Node = union(NodeTag) {
    atomic: Offset,
    left_operation: BinaryOperationNode,
    right_operation: LeftOperationNode,
    binary_operation,
    // TODO: function_call: {function_name: Offset, first_argument: Argument}
    // TODO: Argument: {offset: Offset, next_argument: Offset}

    pub const Tag = NodeTag;
    pub const BinaryOperation = BinaryOperationNode;
    pub const LeftOperation = LeftOperationNode;
    pub const RightOperation = RightOperationNode;
};

const BinaryOperationNode = struct {
    operation: u64 = 0,
    left: Offset = 0,
    right: Offset = 0,
};

const LeftOperationNode = struct {
    operation: u64 = 0,
    left: Offset = 0,
};

const RightOperationNode = struct {
    operation: u64 = 0,
    right: Offset = 0,
};
