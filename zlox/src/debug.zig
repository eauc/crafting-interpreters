const std = @import("std");
const chk = @import("chunk.zig");
const val = @import("value.zig");

pub fn disassembleChunk(chunk: chk.Chunk, name: []const u8) void {
    std.debug.print("=={s}==\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.count) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: chk.Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines[offset] == chunk.lines[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{chunk.lines[offset]});
    }
    switch (chunk.code[offset].instruction) {
        .OP_RETURN => {
            return simpleInstruction("OP_RETURN", offset);
        },
        .OP_CONSTANT => {
            return constantInstruction("OP_CONSTANT", chunk, offset);
        },
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: chk.Chunk, offset: usize) usize {
    const constant = chunk.code[offset + 1].constant;
    std.debug.print("{s: <16} {d: >4} '", .{ name, constant });
    val.printValue(chunk.constants.values[constant]);
    std.debug.print("'\n", .{});
    return offset + 2;
}
