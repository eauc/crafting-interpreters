const std = @import("std");
const chk = @import("chunk.zig");
const cmp = @import("compiler.zig");
const dbg = @import("debug.zig");
const obj = @import("object.zig");
const tbl = @import("table.zig");
const val = @import("value.zig");

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

const FRAME_MAX = 64;
const STACK_MAX = FRAME_MAX * 256;

const CallFrame = struct {
    function: *obj.ObjFunction,
    ip: [*]const chk.OpCode,
    slots: []val.Value,
    returnStackTop: usize,
};

const Stack = struct {
    stack: [STACK_MAX]val.Value,
    stackTop: usize,
    const default = Stack{
        .stack = .{val.Value.nilVal()} ** STACK_MAX,
        .stackTop = 0,
    };
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
    frames: [FRAME_MAX]CallFrame,
    frameCount: usize,
    stack: Stack,
    globals: tbl.Table,
    strings: tbl.Table,
    objects: obj.ObjsList,
    pub const default = VM{
        .frames = undefined,
        .frameCount = 0,
        .stack = Stack.default,
        .globals = tbl.Table.default,
        .strings = tbl.Table.default,
        .objects = obj.ObjsList.default,
    };
    pub fn init(self: *VM, allocator: std.mem.Allocator) !void {
        self.globals.init(allocator, &self.objects);
        self.strings.init(allocator, &self.objects);
        try self.defineNative("clock", clockNative);
    }
    pub fn free(self: *VM) void {
        self.globals.free();
        self.strings.free();
        self.objects.free();
    }
    pub fn interpret(self: *VM, source: []const u8) !void {
        const function = cmp.compile(source, &self.strings) catch {
            return InterpretError.CompileError;
        };
        defer function.obj.free();
        self.stack.push(val.Value.objVal(&function.obj));
        try self.call(function, 0);
        try self.run();
    }
    pub fn run(self: *VM) !void {
        while (true) {
            var frame = &self.frames[self.frameCount - 1];
            self.stack.trace();
            _ = dbg.disassembleInstruction(frame.function.chunk, frame.ip - frame.function.chunk.code.ptr);
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
                .OP_POP => {
                    _ = self.stack.pop();
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
                    if (self.stack.peek(0).isString() and self.stack.peek(1).isString()) {
                        const b = self.stack.pop().asString();
                        const a = self.stack.pop().asString();
                        const object = try self.strings.concatenateStrings(a, b);
                        self.stack.push(val.Value.objVal(object));
                    } else if (self.stack.peek(0).isNumber() and self.stack.peek(1).isNumber()) {
                        const b = self.stack.pop().asNumber();
                        const a = self.stack.pop().asNumber();
                        self.stack.push(val.Value.numberVal(a + b));
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings.", .{});
                        return InterpretError.RuntimeError;
                    }
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
                .OP_GET_GLOBAL => {
                    const name = self.readString();
                    var value = val.Value.nilVal();
                    if (!self.getGlobal(name, &value)) {
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RuntimeError;
                    }
                    self.stack.push(value);
                },
                .OP_SET_GLOBAL => {
                    const name = self.readString();
                    if (try self.setGlobal(name, self.stack.peek(0))) {
                        self.deleteGlobal(name);
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RuntimeError;
                    }
                },
                .OP_DEFINE_GLOBAL => {
                    const name = self.readString();
                    try self.defineGlobal(name, self.stack.peek(0));
                    _ = self.stack.pop();
                },
                .OP_GET_LOCAL => {
                    const slot = self.readByte().constant;
                    self.stack.push(frame.slots[slot]);
                },
                .OP_SET_LOCAL => {
                    const slot = self.readByte().constant;
                    frame.slots[slot] = self.stack.peek(0);
                },
                .OP_PRINT => {
                    val.printValue(self.stack.pop());
                    std.debug.print("\n", .{});
                },
                .OP_JUMP_IF_FALSE => {
                    const offset = self.readShort();
                    if (self.stack.peek(0).isFalsey()) {
                        frame.ip += offset;
                    }
                },
                .OP_JUMP => {
                    const offset = self.readShort();
                    frame.ip += offset;
                },
                .OP_LOOP => {
                    const offset = self.readShort();
                    frame.ip -= offset;
                },
                .OP_CALL => {
                    const argCount = self.readByte().constant;
                    try self.callValue(self.stack.peek(argCount), argCount);
                },
                .OP_RETURN => {
                    const result = self.stack.pop();
                    self.frameCount -= 1;
                    if (self.frameCount == 0) {
                        _ = self.stack.pop();
                        return;
                    }
                    self.stack.stackTop = frame.returnStackTop;
                    self.stack.push(result);
                },
            }
        }
    }
    pub fn defineGlobal(self: *VM, name: *obj.ObjString, value: val.Value) !void {
        _ = try self.globals.set(name, value);
    }
    pub fn getGlobal(self: *VM, name: *obj.ObjString, value: *val.Value) bool {
        return self.globals.get(name, value);
    }
    pub fn setGlobal(self: *VM, name: *obj.ObjString, value: val.Value) !bool {
        return self.globals.set(name, value);
    }
    pub fn deleteGlobal(self: *VM, name: *obj.ObjString) void {
        _ = self.globals.delete(name);
    }
    pub fn defineNative(self: *VM, name: []const u8, function: *const obj.NativeFn) !void {
        self.stack.push(val.Value.objVal(try self.strings.copyString(name)));
        self.stack.push(val.Value.objVal(&(try obj.ObjNative.create(self.strings.allocator, function)).obj));
        _ = try self.globals.set(self.stack.stack[0].asString(), self.stack.stack[1]);
        _ = self.stack.pop();
        _ = self.stack.pop();
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
    fn callValue(self: *VM, callee: val.Value, argCount: u8) !void {
        if (callee.isObj()) {
            switch (callee.objType()) {
                .FUNCTION => {
                    return self.call(callee.asFunction(), argCount);
                },
                .NATIVE => {
                    const result = callee.asNative().function(
                        self.stack.stack[self.stack.stackTop - argCount .. self.stack.stackTop],
                    );
                    self.stack.stackTop -= argCount + 1;
                    self.stack.push(result);
                    return;
                },
                else => {},
            }
        }
        self.runtimeError("Can only call functions and classes.", .{});
        return InterpretError.RuntimeError;
    }
    fn call(self: *VM, function: *obj.ObjFunction, argCount: u8) !void {
        if (argCount != function.arity) {
            self.runtimeError("Expected {} arguments but got {}.", .{ function.arity, argCount });
            return InterpretError.RuntimeError;
        }
        if (self.frameCount == FRAME_MAX) {
            self.runtimeError("Stack overflow.", .{});
            return InterpretError.RuntimeError;
        }
        self.frames[self.frameCount] = .{
            .function = function,
            .ip = function.chunk.code.ptr,
            .slots = self.stack.stack[self.stack.stackTop - argCount - 1 ..],
            .returnStackTop = self.stack.stackTop - argCount - 1,
        };
        self.frameCount += 1;
    }
    fn readString(self: *VM) *obj.ObjString {
        return self.readConstant().asString();
    }
    fn readConstant(self: *VM) val.Value {
        const frame = &self.frames[self.frameCount - 1];
        return frame.function.chunk.constants.values[self.readByte().constant];
    }
    fn readShort(self: *VM) u16 {
        var frame = &self.frames[self.frameCount - 1];
        const byte1 = frame.ip[0].constant;
        const byte2 = frame.ip[1].constant;
        const short: u16 = std.math.shl(u16, byte1, 8) | byte2;
        frame.ip += 2;
        return short;
    }
    fn readByte(self: *VM) chk.OpCode {
        var frame = &self.frames[self.frameCount - 1];
        const byte = frame.ip[0];
        frame.ip += 1;
        return byte;
    }
    fn runtimeError(self: *VM, comptime message: []const u8, args: anytype) void {
        std.debug.print(message, args);
        std.debug.print("\n", .{});
        for (0..self.frameCount) |i| {
            const frame = self.frames[self.frameCount - i - 1];
            const function = frame.function;
            const instruction = frame.ip - function.chunk.code.ptr - 1;
            std.debug.print("[line {d}] in ", .{function.chunk.lines[instruction]});
            if (function.name) |name| {
                std.debug.print("{s}\n", .{name.chars});
            } else {
                std.debug.print("script\n", .{});
            }
        }
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

fn clockNative(args: []val.Value) val.Value {
    _ = args;
    const clock: f64 = @floatFromInt(std.time.milliTimestamp());
    return val.Value.numberVal(clock / 1000);
}
