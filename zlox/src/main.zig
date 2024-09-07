const std = @import("std");
const chk = @import("chunk.zig");
const dbg = @import("debug.zig");
const VM = @import("vm.zig").VM;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var vm = VM.default;
    var chunk = chk.Chunk.default;
    chunk.init(allocator);
    var constant = try chunk.addConstant(1.2);
    try chunk.write(.{ .instruction = chk.Instruction.OP_CONSTANT }, 123);
    try chunk.write(.{ .constant = constant }, 123);

    constant = try chunk.addConstant(3.4);
    try chunk.write(.{ .instruction = chk.Instruction.OP_CONSTANT }, 123);
    try chunk.write(.{ .constant = constant }, 123);

    try chunk.write(.{ .instruction = chk.Instruction.OP_ADD }, 123);

    constant = try chunk.addConstant(5.6);
    try chunk.write(.{ .instruction = chk.Instruction.OP_CONSTANT }, 123);
    try chunk.write(.{ .constant = constant }, 123);

    try chunk.write(.{ .instruction = chk.Instruction.OP_DIVIDE }, 123);
    try chunk.write(.{ .instruction = chk.Instruction.OP_NEGATE }, 123);
    try chunk.write(.{ .instruction = chk.Instruction.OP_RETURN }, 123);

    try vm.interpret(&chunk);
}
