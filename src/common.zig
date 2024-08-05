const std = @import("std");
const builtin = @import("builtin");

pub const debug = builtin.mode == .Debug;
pub const testing = builtin.is_test;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub const allocator: std.mem.Allocator = if (testing)
    std.testing.allocator
else
    gpa.allocator();
