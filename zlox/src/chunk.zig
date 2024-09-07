const std = @import("std");
const mem = @import("memory.zig");
const val = @import("value.zig");

pub const Instruction = enum(u8) {
    OP_CONSTANT,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_RETURN,
};

pub const OpCode = union(enum) {
    instruction: Instruction,
    constant: u8,
};

pub const Chunk = struct {
    allocator: std.mem.Allocator,
    count: usize,
    code: []OpCode,
    lines: []usize,
    constants: val.ValueArray,
    pub const default: Chunk = .{
        .allocator = undefined,
        .count = 0,
        .code = &[_]OpCode{},
        .lines = &[_]usize{},
        .constants = val.ValueArray.default,
    };
    pub fn init(self: *Chunk, allocator: std.mem.Allocator) void {
        self.allocator = allocator;
        self.count = 0;
        self.code = &[_]OpCode{};
        self.lines = &[_]usize{};
        self.constants.init(allocator);
    }
    pub fn free(self: *Chunk) void {
        self.constants.free();
        self.allocator.free(self.code);
        self.allocator.free(self.lines);
        self.init(self.allocator);
    }
    pub fn write(self: *Chunk, opCode: OpCode, line: usize) !void {
        if (self.code.len < self.count + 1) {
            const oldCapacity = self.code.len;
            const newCapacity = mem.growCapacity(oldCapacity);
            self.code = try mem.growArray(OpCode, self.code, oldCapacity, newCapacity, self.allocator);
            self.lines = try mem.growArray(usize, self.lines, oldCapacity, newCapacity, self.allocator);
        }
        self.code[self.count] = opCode;
        self.lines[self.count] = line;
        self.count += 1;
    }
    pub fn addConstant(self: *Chunk, value: val.Value) !u8 {
        try self.constants.write(value);
        return self.constants.count - 1;
    }
};
