const Node = @import("node.zig").Node;
const Operator = @import("operator.zig").Operator;
const Run = @import("run.zig").Run;
const RunContext = @import("run_context.zig").RunContext;

const Declare = Run.Declare;
const Error = Run.Error;
const Value = Run.Value;
const Condition = Run.Condition;

pub const DoNothing = struct {
    pub fn evaluatePrefix(self: *Self, context: RunContext, operator: Operator, right: Value) Error!Value {
        _ = context;
        _ = self;
        _ = operator;
        _ = right;
        return .{ .intermediate = 0 };
    }

    pub fn evaluateInfix(self: *Self, context: RunContext, left: Value, operator: Operator, right: Value) Error!Value {
        _ = context;
        _ = self;
        _ = left;
        _ = operator;
        _ = right;
        return .{ .intermediate = 0 };
    }

    pub fn evaluatePostfix(self: *Self, context: RunContext, left: Value, operator: Operator) Error!Value {
        _ = context;
        _ = self;
        _ = left;
        _ = operator;
        return .{ .intermediate = 0 };
    }

    pub fn evaluateCondition(self: *Self, context: RunContext, value: Value) Condition {
        _ = context;
        _ = self;
        _ = value;
        return .unevaluated;
    }

    /// Returns a handle that should be used to `descopeVariable`.
    pub fn declareVariable(self: *Self, context: RunContext, name: Value, declare: Declare, variable_type: Value) Error!Value {
        _ = context;
        _ = self;
        _ = name;
        _ = declare;
        _ = variable_type;
        return .{ .intermediate = 0 };
    }

    pub fn descopeVariable(self: *Self, context: RunContext, handle: Value) Error!void {
        _ = context;
        _ = self;
        _ = handle;
    }

    const Self = @This();
};
