const SmallString = @import("string.zig").Small;

const RunContextError = error{
    unknown,
};

pub const RunContext = struct {
    // TODO

    pub const Error = RunContextError;
    pub const Value = RunContextValue;
    pub const Declare = RunContextDeclare;
    pub const Handle = u64;
};

const RunContextDeclare = enum {
    generic, // `
    readonly, // :
    writable, // ;
    temporary, // .
    // TODO: either add these combinations OR redo entire declarations
    //      with single declare values.  theoretically we want to be able
    //      to write hm-lang with the hm-lang transpiler, though, so that
    //      would require defining all these.  but parser is at a different stage...
    //    rr, // ::
    //    rw, // :;
    //    rt, // :.
    //    wr, // ;:
    //    ww, // ;;
    //    wt, // ;.
    //    tr, // .:
    //    tw, // .;
    //    tt, // ..
};

const RunContextValueTag = enum {
    identifier,
    number,
    // A value that is stored in the RunContext,
    // i.e., is not a compile-time constant.
    intermediate,
};

const RunContextValue = union(RunContextValueTag) {
    identifier: SmallString,
    number: SmallString,
    intermediate: u64,

    pub const Tag = RunContextValueTag;
    const Self = @This();
};
