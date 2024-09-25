const common = @import("common.zig");
const OwnedList = @import("owned_list.zig").OwnedList;
const SmallString = @import("string.zig").Small;

const OwnedSmalls = OwnedList(SmallString);

const std = @import("std");

pub const Vargs = struct {
    args: OwnedSmalls,

    pub fn init() !Self {
        const process_args = try std.process.argsAlloc(common.allocator);
        defer std.process.argsFree(common.allocator, process_args);

        var new_args = OwnedSmalls.init();
        for (0..process_args.len) |i| {
            // This likely does one more allocation than I'd like to create `new_args[i]`,
            // but I'd rather not redo the internals of `argsAlloc`.
            try new_args.append(try SmallString.init(process_args[i]));
        }
        return .{ .args = new_args };
    }

    pub fn deinit(self: *Self) void {
        self.args.deinit();
    }

    pub fn shift(self: *Self) ?SmallString {
        return self.args.shift();
    }

    pub fn pop(self: *Self) ?SmallString {
        return self.args.pop();
    }

    pub fn count(self: *const Self) usize {
        return self.args.count();
    }

    const Self = @This();
};
