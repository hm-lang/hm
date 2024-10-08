const common = @import("common.zig");
const owned_list = @import("owned_list.zig");
const SmallString = @import("string.zig").Small;

const OwnedSmalls = owned_list.OwnedList(SmallString);

const std = @import("std");

pub const FileError = error{
    not_found,
    line_too_long,
    out_of_memory,
    write,
    other,
};

pub const File = struct {
    /// I hope your files aren't bigger than `u16`.
    pub const Range = common.Range(usize);

    /// File will take this into ownership.
    path: SmallString = .{},
    lines: OwnedSmalls = OwnedSmalls.init(),

    pub fn deinit(self: *File) void {
        self.path.deinit();
        self.lines.deinit();
    }

    pub fn count(self: *const File) usize {
        return self.lines.count();
    }

    pub fn read(self: *File) FileError!void {
        const lines = try self.readInternal();
        self.lines.deinit();
        self.lines = lines;
    }

    pub inline fn printLine(self: *const File, writer: anytype) !void {
        return self.print(writer);
    }

    pub fn print(self: *const File, writer: anytype) !void {
        for (self.lines.items()) |line| {
            writer.print("{s}\n", .{line.slice()}) catch {};
        }
    }

    pub fn write(self: *const File) FileError!void {
        const file = try self.openForWrite();
        defer file.close();

        for (self.lines.items()) |line| {
            // TODO: can we somehow use `line.printLine(file)`??
            file.writeAll(line.slice()) catch {
                return FileError.write;
            };
            const result = file.write("\n") catch {
                return FileError.write;
            };
            std.debug.assert(result == 1); // should have written 1 byte
        }
    }

    fn readInternal(self: *const File) FileError!OwnedSmalls {
        var result = OwnedSmalls.init();
        errdefer result.deinit();

        var buffer: [SmallString.max_count]u8 = undefined;

        const file = try self.openForRead();
        defer file.close();

        var buffered_reader = std.io.bufferedReader(file.reader());
        var file_stream = buffered_reader.reader();

        while (true) {
            const line_buffer = file_stream.readUntilDelimiterOrEof(&buffer, '\n') catch |e| {
                if (e == error.StreamTooLong) {
                    std.debug.print("line {d} starting with {s} too long\n", .{
                        result.count() + 1,
                        buffer[0..32],
                    });
                    return FileError.line_too_long;
                }
                return FileError.other;
            } orelse break;
            const line = SmallString.init(line_buffer) catch {
                return FileError.out_of_memory;
            };
            result.append(line) catch {
                return FileError.out_of_memory;
            };
        }

        return result;
    }

    fn openForRead(self: *const File) FileError!std.fs.File {
        return std.fs.cwd().openFile(self.path.slice(), .{}) catch {
            return FileError.not_found;
        };
    }

    fn openForWrite(self: *const File) FileError!std.fs.File {
        return std.fs.cwd().createFile(self.path.slice(), .{}) catch {
            return FileError.not_found;
        };
    }

    pub fn appendSlice(self: *File, slice: []const []const u8) FileError!void {
        for (slice) |line_u8| {
            if (line_u8.len > SmallString.max_count) {
                return FileError.line_too_long;
            }
            const line = SmallString.init(line_u8) catch {
                return FileError.out_of_memory;
            };
            self.lines.append(line) catch {
                return FileError.out_of_memory;
            };
        }
    }

    pub fn expectEqualsSlice(self: *const File, other: []const []const u8) !void {
        const stderr = common.debugStderr;
        errdefer {
            stderr.print("expected:\n", .{}) catch {};
            for (other) |line| {
                stderr.print("{s}\n", .{line}) catch {};
            }

            stderr.print("\ngot:\n", .{}) catch {};
            self.print(stderr) catch {};
        }
        for (0..@min(self.count(), other.len)) |index| {
            const self_line = self.lines.inBounds(index);
            const other_line = other[index];
            std.testing.expectEqualStrings(other_line, self_line.slice()) catch |e| {
                stderr.print("\nnot equal at index {d}\n\n", .{index}) catch {};
                return e;
            };
        }
        try std.testing.expectEqual(other.len, self.count());
    }
};

test "reading this file works" {
    var file: File = .{ .path = SmallString.noAlloc("src/file.zig") };
    defer file.deinit();

    try file.read();

    var line = file.lines.at(0).?;
    try std.testing.expectEqualStrings(line.slice(), "const common = @import(\"common.zig\");");

    line = file.lines.at(1).?;
    try std.testing.expectEqualStrings(line.slice(), "const owned_list = @import(\"owned_list.zig\");");

    line = file.lines.at(-1).?;
    try std.testing.expectEqualStrings(line.slice(), "// last line of file");
}

test "writing a tmp-file works" {
    var file: File = .{ .path = SmallString.noAlloc("zig-out/z.tmp") };
    defer file.deinit();

    try file.lines.append(SmallString.noAlloc("hello world"));
    try file.lines.append(SmallString.noAlloc("second line"));
    try file.lines.append(try SmallString.init("third line is long and needs an allocation"));
    try file.write();

    var previous_lines = OwnedSmalls.init(); // After swap, will become previous.
    defer previous_lines.deinit();
    common.swap(&file.lines, &previous_lines);

    try file.read();

    try file.lines.expectEquals(previous_lines);
}

// last line of file
