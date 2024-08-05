const common = @import("common.zig");
const owned_list = @import("owned_list.zig");
const string = @import("string.zig");

const OwnedSmalls = owned_list.OwnedList(string.Small);

const std = @import("std");

const FileError = error{
    FileNotFound,
    LineTooLong,
};

const File = struct {
    /// You need to free this yourself if it's allocated;
    /// `File` will not do so.
    path: []u8 = .{},

    pub fn lines(self: *const File) FileError!OwnedSmalls {
        var result = OwnedSmalls.init();

        var buffer: [65535]u8 = undefined;

        const file = std.fs.cwd().openFile(self.path, .{}) catch { 
            result.deinit();
            return FileError.FileNotFound; 
        };
        defer file.close();

        var buffered_reader = std.io.bufferedReader(file.reader());
        var file_stream = buffered_reader.reader();

        while (try file_stream.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
            std.debug.print("got line: {s}\n", .{line});
        }

        return result;
    }
};
