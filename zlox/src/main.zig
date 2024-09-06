const std = @import("std");
const chk = @import("chunk.zig");
const dbg = @import("debug.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var chunk = chk.Chunk.default;
    chunk.init(allocator);
    const constant = try chunk.addConstant(1.2);
    try chunk.write(.{ .instruction = chk.Instruction.OP_CONSTANT }, 123);
    try chunk.write(.{ .constant = constant }, 123);
    try chunk.write(.{ .instruction = chk.Instruction.OP_RETURN }, 123);
    dbg.disassembleChunk(chunk, "test chunk");
}
