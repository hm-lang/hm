const common = @import("common.zig");
const owned_list = @import("owned_list.zig");
const string = @import("string.zig");

const OwnedSmalls = owned_list.OwnedList(string.Small);

const std = @import("std");

const FileError = error{
    FileNotFound,
    LineTooLong,
    OutOfMemory,
    OtherError,
};

const File = struct {
    /// File will take this into ownership.
    path: string.Small = .{},
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

    fn readInternal(self: *const File) FileError!OwnedSmalls {
        var result = OwnedSmalls.init();
        errdefer result.deinit();

        var buffer: [string.Small.max_size]u8 = undefined;

        const file = std.fs.cwd().openFile(self.path.slice(), .{}) catch {
            return FileError.FileNotFound;
        };
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
            const line = string.Small.init(line_buffer) catch {
                return FileError.OutOfMemory;
            };
            result.append(line) catch {
                return FileError.OutOfMemory;
            };
        }

        return result;
    }
};

test "reading this file works" {
    var file: File = .{ .path = string.Small.noAlloc("src/file.zig") };
    defer file.deinit();

    try file.read();

    var line = try file.lines.at(0);
    try std.testing.expectEqualStrings(line.slice(), "const common = @import(\"common.zig\");");

    line = try file.lines.at(-1);
    try std.testing.expectEqualStrings(line.slice(), "// last line of file");
}

// last line of file
