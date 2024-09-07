const std = @import("std");
const chk = @import("chunk.zig");
const dbg = @import("debug.zig");
const val = @import("value.zig");

const InterpretError = error{
    CompileError,
    RuntimeError,
};

const STACK_MAX = 256;

const Stack = struct {
    stack: [STACK_MAX]val.Value,
    stackTop: usize,
    const default = Stack{ .stack = .{0} ** STACK_MAX, .stackTop = 0 };
    pub fn reset(self: *Stack) void {
        self.stackTop = 0;
    }
    pub fn push(self: *Stack, value: val.Value) void {
        self.stack[self.stackTop] = value;
        self.stackTop += 1;
    }
    pub fn pop(self: *Stack) val.Value {
        self.stackTop -= 1;
        return self.stack[self.stackTop];
    }
    pub fn trace(self: *Stack) void {
        dbg.traceExecution("          ", .{});
        for (0..self.stackTop) |i| {
            dbg.traceExecution("[ ", .{});
            val.printValue(self.stack[i]);
            dbg.traceExecution(" ]", .{});
        }
        dbg.traceExecution("\n", .{});
    }
};

pub const VM = struct {
    chunk: *const chk.Chunk,
    ip: [*]const chk.OpCode,
    stack: Stack,
    pub const default = VM{
        .chunk = undefined,
        .ip = undefined,
        .stack = Stack.default,
    };
    pub fn interpret(self: *VM, chunk: *const chk.Chunk) InterpretError!void {
        self.chunk = chunk;
        self.ip = chunk.code.ptr;
        try self.run();
    }
    pub fn run(self: *VM) InterpretError!void {
        while (true) {
            self.stack.trace();
            _ = dbg.disassembleInstruction(self.chunk.*, self.ip - self.chunk.code.ptr);
            const opCode = self.readByte();
            switch (opCode.instruction) {
                chk.Instruction.OP_CONSTANT => {
                    const constant = self.readConstant();
                    self.stack.push(constant);
                },
                chk.Instruction.OP_NEGATE => {
                    self.stack.push(-self.stack.pop());
                },
                chk.Instruction.OP_ADD => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    self.stack.push(a + b);
                },
                chk.Instruction.OP_SUBTRACT => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    self.stack.push(a - b);
                },
                chk.Instruction.OP_MULTIPLY => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    self.stack.push(a * b);
                },
                chk.Instruction.OP_DIVIDE => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    self.stack.push(a / b);
                },
                chk.Instruction.OP_RETURN => {
                    val.printValue(self.stack.pop());
                    std.debug.print("\n", .{});
                    return;
                },
            }
        }
    }
    fn readByte(self: *VM) chk.OpCode {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }
    fn readConstant(self: *VM) val.Value {
        return self.chunk.constants.values[self.readByte().constant];
    }
};
