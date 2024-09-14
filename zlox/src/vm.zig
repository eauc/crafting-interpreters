const std = @import("std");
const chk = @import("chunk.zig");
const cmp = @import("compiler.zig");
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
    const default = Stack{ .stack = .{val.Value.nilVal()} ** STACK_MAX, .stackTop = 0 };
    pub fn reset(self: *Stack) void {
        self.stackTop = 0;
    }
    pub fn push(self: *Stack, value: val.Value) void {
        self.stack[self.stackTop] = value;
        self.stackTop += 1;
    }
    pub fn peek(self: *Stack, depth: usize) val.Value {
        return self.stack[self.stackTop - depth - 1];
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
    pub fn interpret(self: *VM, source: []const u8, allocator: std.mem.Allocator) InterpretError!void {
        var chunk = chk.Chunk.default;
        chunk.init(allocator);
        cmp.compile(source, &chunk) catch {
            chunk.free();
            return InterpretError.CompileError;
        };
        defer chunk.free();

        self.chunk = &chunk;
        self.ip = chunk.code.ptr;
        try self.run();
    }
    pub fn run(self: *VM) InterpretError!void {
        while (true) {
            self.stack.trace();
            _ = dbg.disassembleInstruction(self.chunk.*, self.ip - self.chunk.code.ptr);
            const opCode = self.readByte();
            switch (opCode.instruction) {
                .OP_CONSTANT => {
                    const constant = self.readConstant();
                    self.stack.push(constant);
                },
                .OP_NIL => {
                    self.stack.push(val.Value.nilVal());
                },
                .OP_TRUE => {
                    self.stack.push(val.Value.boolVal(true));
                },
                .OP_FALSE => {
                    self.stack.push(val.Value.boolVal(false));
                },
                .OP_NEGATE => {
                    if (!self.stack.peek(0).isNumber()) {
                        self.runtimeError("Operand must be a number.", .{});
                        return InterpretError.RuntimeError;
                    }
                    self.stack.push(val.Value.numberVal(-self.stack.pop().asNumber()));
                },
                .OP_NOT => {
                    self.stack.push(val.Value.boolVal(self.stack.pop().isFalsey()));
                },
                .OP_EQUAL => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    self.stack.push(val.Value.boolVal(a.equals(b)));
                },
                .OP_GREATER => {
                    try self.binaryOp(bool, val.Value.boolVal, greater);
                },
                .OP_LESS => {
                    try self.binaryOp(bool, val.Value.boolVal, less);
                },
                .OP_ADD => {
                    try self.binaryOp(val.Number, val.Value.numberVal, add);
                },
                .OP_SUBTRACT => {
                    try self.binaryOp(val.Number, val.Value.numberVal, subtract);
                },
                .OP_MULTIPLY => {
                    try self.binaryOp(val.Number, val.Value.numberVal, multiply);
                },
                .OP_DIVIDE => {
                    try self.binaryOp(val.Number, val.Value.numberVal, divide);
                },
                .OP_RETURN => {
                    val.printValue(self.stack.pop());
                    std.debug.print("\n", .{});
                    return;
                },
            }
        }
    }
    fn binaryOp(self: *VM, comptime T: type, valueType: fn (T) val.Value, op: fn (f64, f64) T) InterpretError!void {
        if (!self.stack.peek(0).isNumber() or !self.stack.peek(1).isNumber()) {
            self.runtimeError("Operands must be numbers.", .{});
            return InterpretError.RuntimeError;
        }
        const b = self.stack.pop().asNumber();
        const a = self.stack.pop().asNumber();
        self.stack.push(valueType(op(a, b)));
    }
    fn readByte(self: *VM) chk.OpCode {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }
    fn readConstant(self: *VM) val.Value {
        return self.chunk.constants.values[self.readByte().constant];
    }
    fn runtimeError(self: *VM, comptime message: []const u8, args: anytype) void {
        std.debug.print(message, args);
        std.debug.print("\n", .{});
        const instruction = self.ip - self.chunk.code.ptr - 1;
        const line = self.chunk.lines[instruction];
        std.debug.print("[line {}] in script\n", .{line});
        self.stack.reset();
    }
};

fn add(a: f64, b: f64) f64 {
    return a + b;
}
fn subtract(a: f64, b: f64) f64 {
    return a - b;
}
fn multiply(a: f64, b: f64) f64 {
    return a * b;
}
fn divide(a: f64, b: f64) f64 {
    return a / b;
}
fn greater(a: f64, b: f64) bool {
    return a > b;
}
fn less(a: f64, b: f64) bool {
    return a < b;
}
