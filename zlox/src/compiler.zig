const std = @import("std");
const scn = @import("scanner.zig");

pub fn compile(source: []const u8) void {
    var scanner = scn.Scanner.default;
    scanner.init(source);
    var line: isize = -1;
    while (true) {
        const token = scanner.scanToken();
        if (token.line != line) {
            std.debug.print("{d:4} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print("{d:2} '{s}'\n", .{ @intFromEnum(token.type), token.lexeme });
        if (token.type == .TOKEN_EOF) {
            break;
        }
    }
}
