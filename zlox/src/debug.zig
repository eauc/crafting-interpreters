const std = @import("std");
const chk = @import("chunk.zig");
const val = @import("value.zig");

pub const PRINT_CODE = true;
const TRACE_EXECUTION = true;

pub fn traceExecution(comptime format: []const u8, args: anytype) void {
    if (TRACE_EXECUTION) {
        std.debug.print(format, args);
    }
}

pub fn disassembleChunk(chunk: chk.Chunk, name: []const u8) void {
    if (TRACE_EXECUTION) {
        std.debug.print("=={s}==\n", .{name});
        var offset: usize = 0;
        while (offset < chunk.count) {
            offset = disassembleInstruction(chunk, offset);
        }
    }
}

pub fn disassembleInstruction(chunk: chk.Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines[offset] == chunk.lines[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{chunk.lines[offset]});
    }
    switch (chunk.code[offset].instruction) {
        .OP_CONSTANT => {
            return constantInstruction("OP_CONSTANT", chunk, offset);
        },
        .OP_NIL => {
            return simpleInstruction("OP_NIL", offset);
        },
        .OP_TRUE => {
            return simpleInstruction("OP_TRUE", offset);
        },
        .OP_FALSE => {
            return simpleInstruction("OP_FALSE", offset);
        },
        .OP_NEGATE => {
            return simpleInstruction("OP_NEGATE", offset);
        },
        .OP_NOT => {
            return simpleInstruction("OP_NOT", offset);
        },
        .OP_EQUAL => {
            return simpleInstruction("OP_EQUAL", offset);
        },
        .OP_GREATER => {
            return simpleInstruction("OP_GREATER", offset);
        },
        .OP_LESS => {
            return simpleInstruction("OP_LESS", offset);
        },
        .OP_ADD => {
            return simpleInstruction("OP_ADD", offset);
        },
        .OP_SUBTRACT => {
            return simpleInstruction("OP_SUBTRACT", offset);
        },
        .OP_MULTIPLY => {
            return simpleInstruction("OP_MULTIPLY", offset);
        },
        .OP_DIVIDE => {
            return simpleInstruction("OP_DIVIDE", offset);
        },
        .OP_RETURN => {
            return simpleInstruction("OP_RETURN", offset);
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
