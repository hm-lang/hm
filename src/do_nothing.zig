const Node = @import("node.zig").Node;
const Operator = @import("operator.zig").Operator;
const RunContext = @import("run_context.zig").RunContext;

const Declare = RunContext.Declare;
const Error = RunContext.Error;
const Value = RunContext.Value;

pub const DoNothing = struct {
    pub fn evaluatePrefix(self: *Self, operator: Operator, right: Value) Error!Value {
        _ = self;
        _ = operator;
        _ = right;
        return .{ .intermediate = 0 };
    }

    pub fn evaluateInfix(self: *Self, left: Value, operator: Operator, right: Value) Error!Value {
        _ = self;
        _ = left;
        _ = operator;
        _ = right;
        return .{ .intermediate = 0 };
    }

    pub fn evaluatePostfix(self: *Self, left: Value, operator: Operator) Error!Value {
        _ = self;
        _ = left;
        _ = operator;
        return .{ .intermediate = 0 };
    }

    pub fn evaluateCondition(self: *Self, value: Value) bool {
        _ = self;
        _ = value;
        return false;
    }

    pub fn declareVariable(self: *Self, name: Value, declare: Declare, variable_type: Value) bool {
        _ = self;
        _ = name;
        _ = declare;
        _ = variable_type;
        return false;
    }

    const Self = @This();
};
