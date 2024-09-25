const SmallString = @import("string.zig").Small;

const RunContextTag = enum {
    statement,
    if_condition,
    if_block,
    elif_block,
    else_block,
    // TODO: function calls, definitions, etc.
};

pub const RunContext = union(RunContextTag) {
    statement: Id,
    if_condition: Id,
    if_block: Id,
    elif_block: Id,
    else_block: Id,

    pub const Id = u64;
    pub const Tag = RunContextTag;
    const Self = @This();
};
