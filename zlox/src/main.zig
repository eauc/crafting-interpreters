const std = @import("std");
const chk = @import("chunk.zig");
const dbg = @import("debug.zig");
const InterpretError = @import("vm.zig").InterpretError;
const VM = @import("vm.zig").VM;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var vm = VM.default;
    const args = try std.process.argsAlloc(allocator);
    if (args.len == 1) {
        try repl(&vm, allocator);
    } else if (args.len == 2) {
        try runFile(&vm, args[1], allocator);
    } else {
        std.debug.print("Usage: zlox [path]\n", .{});
    }
}

fn repl(vm: *VM, allocator: std.mem.Allocator) !void {
    var readBuffer: [1024]u8 = .{0} ** 1024;
    var stdin = std.io.getStdIn().reader();
    var stdout = std.io.getStdOut().writer();
    while (true) {
        try stdout.print("> ", .{});
        const readResult = try stdin.readUntilDelimiterOrEof(&readBuffer, '\n');
        if (readResult) |line| {
            vm.interpret(line, allocator) catch |err| {
                switch (err) {
                    InterpretError.CompileError => std.debug.print("Compile error.\n", .{}),
                    InterpretError.RuntimeError => std.debug.print("Runtime error.\n", .{}),
                    else => return err,
                }
            };
        } else {
            try stdout.print("\nBye bye.\n", .{});
            break;
        }
    }
}

fn runFile(vm: *VM, filePath: []const u8, allocator: std.mem.Allocator) !void {
    const source = try readFile(filePath, allocator);
    try vm.interpret(source, allocator);
}

fn readFile(filePath: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const cwd: std.fs.Dir = std.fs.cwd();
    var file: std.fs.File = try cwd.openFile(filePath, .{});
    defer file.close();
    return file.readToEndAlloc(allocator, std.math.maxInt(usize));
}
