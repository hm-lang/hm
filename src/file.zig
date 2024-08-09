const common = @import("common.zig");
const owned_list = @import("owned_list.zig");
const SmallString = @import("string.zig").Small;

const OwnedSmalls = owned_list.OwnedList(SmallString);

const std = @import("std");

pub const FileError = error{
    FileNotFound,
    LineTooLong,
    OutOfMemory,
    OtherError,
    WriteError,
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

    pub fn read(self: *File) FileError!void {
        const lines = try self.readInternal();
        self.lines.deinit();
        self.lines = lines;
    }

    pub fn write(self: *const File) FileError!void {
        const file = try self.openForWrite();
        defer file.close();

        for (self.lines.items()) |line| {
            // TODO: can we somehow use `line.printLine(file)`??
            file.writeAll(line.slice()) catch {
                return FileError.WriteError;
            };
            const result = file.write("\n") catch {
                return FileError.WriteError;
            };
            std.debug.assert(result == 1); // should have written 1 byte
        }
    }

    fn readInternal(self: *const File) FileError!OwnedSmalls {
        var result = OwnedSmalls.init();
        errdefer result.deinit();

        var buffer: [SmallString.max_size]u8 = undefined;

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
                    return FileError.LineTooLong;
                }
                return FileError.OtherError;
            } orelse break;
            const line = SmallString.init(line_buffer) catch {
                return FileError.OutOfMemory;
            };
            result.append(line) catch {
                return FileError.OutOfMemory;
            };
        }

        return result;
    }

    fn openForRead(self: *const File) FileError!std.fs.File {
        return std.fs.cwd().openFile(self.path.slice(), .{}) catch {
            return FileError.FileNotFound;
        };
    }

    fn openForWrite(self: *const File) FileError!std.fs.File {
        return std.fs.cwd().createFile(self.path.slice(), .{}) catch {
            return FileError.FileNotFound;
        };
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
