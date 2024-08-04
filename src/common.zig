const std = @import("std");
const builtin = @import("builtin");

pub const debug = builtin.mode == .Debug;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub const allocator: std.mem.Allocator = if (debug)
    std.testing.allocator
else
    gpa.allocator();
