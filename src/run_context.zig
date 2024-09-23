const SmallString = @import("string.zig").Small;

const RunContextTag = enum {
    block,
    if_condition,
    if_truthy,
    if_falsey,
    // TODO: function calls, definitions, etc.
};

pub const RunContext = union(RunContextTag) {
    block: void,
    if_condition: Id,
    if_truthy: Id,
    if_falsey: Id,

    pub const Id = u64;
    pub const Tag = RunContextTag;
};
