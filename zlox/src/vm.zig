const std = @import("std");
const chk = @import("chunk.zig");
const cmp = @import("compiler.zig");
const dbg = @import("debug.zig");
const mem = @import("memory.zig");
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
    closure: *obj.ObjClosure,
    ip: [*]const chk.OpCode,
    slots: []val.Value,
    returnStackTop: usize,
};

pub const Stack = struct {
    stack: [STACK_MAX]val.Value,
    stackTop: usize,
    openUpvalues: ?*obj.ObjUpvalue,

    const default = Stack{
        .stack = .{val.Value.nilVal()} ** STACK_MAX,
        .stackTop = 0,
        .openUpvalues = null,
    };
    pub fn reset(self: *Stack) void {
        self.openUpvalues = null;
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
    pub fn closeUpvalues(self: *Stack, last: *val.Value) void {
        while (self.openUpvalues) |upvalue| {
            if (@intFromPtr(upvalue.location) < @intFromPtr(last)) {
                return;
            }
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.openUpvalues = upvalue.next;
        }
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
    pub fn markValues(self: *Stack) void {
        for (0..self.stackTop) |i| {
            self.stack[i].mark();
        }
    }
    pub fn markUpvalues(self: *Stack) void {
        var currentUpvalue = self.openUpvalues;
        while (currentUpvalue) |upvalue| {
            upvalue.obj.mark();
            currentUpvalue = upvalue.next;
        }
    }
};

pub const VM = struct {
    allocator: *mem.Allocator,
    frames: [FRAME_MAX]CallFrame,
    frameCount: usize,
    stack: Stack,
    globals: tbl.Table,
    strings: tbl.Table,
    initString: ?*obj.ObjString,
    objects: obj.ObjsList,

    pub const default = VM{
        .allocator = undefined,
        .frames = undefined,
        .frameCount = 0,
        .stack = Stack.default,
        .globals = tbl.Table.default,
        .strings = tbl.Table.default,
        .initString = null,
        .objects = obj.ObjsList.default,
    };
    pub fn init(self: *VM, allocator: *mem.Allocator) !void {
        self.allocator = allocator;
        self.allocator.vm = self;
        self.globals.init(allocator, &self.stack);
        self.strings.init(allocator, &self.stack);
        self.initString = @fieldParentPtr("obj", try self.strings.copyString("init"));
        try self.defineNative("clock", clockNative);
    }
    pub fn free(self: *VM) void {
        self.globals.free();
        self.strings.free();
        self.objects.free();
        if (self.initString) |str| str.obj.free();
    }
    pub fn interpret(self: *VM, source: []const u8) !void {
        const function = cmp.compile(source, self.allocator, &self.stack, &self.strings) catch {
            return InterpretError.CompileError;
        };
        defer function.obj.free();
        self.stack.push(val.Value.objVal(&function.obj));
        const closure = try obj.ObjClosure.create(self.allocator, function);
        _ = self.stack.pop();
        self.stack.push(val.Value.objVal(&closure.obj));
        try self.call(closure, 0);
        try self.run();
    }
    pub fn run(self: *VM) !void {
        while (true) {
            var frame = &self.frames[self.frameCount - 1];
            self.stack.trace();
            _ = dbg.disassembleInstruction(
                frame.closure.function.chunk,
                frame.ip - frame.closure.function.chunk.code.ptr,
            );
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
                        const b = self.stack.peek(0).asString();
                        const a = self.stack.peek(1).asString();
                        const object = try self.strings.concatenateStrings(a, b);
                        _ = self.stack.pop();
                        _ = self.stack.pop();
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
                .OP_GET_UPVALUE => {
                    const slot = self.readByte().constant;
                    self.stack.push(frame.closure.upvalues[slot].location.*);
                },
                .OP_SET_UPVALUE => {
                    const slot = self.readByte().constant;
                    frame.closure.upvalues[slot].location.* = self.stack.peek(0);
                },
                .OP_GET_PROPERTY => {
                    if (!self.stack.peek(0).isInstance()) {
                        self.runtimeError("Only instances have properties.", .{});
                        return InterpretError.RuntimeError;
                    }
                    const instance = self.stack.peek(0).asInstance();
                    const name = self.readString();

                    var value = val.Value.nilVal();
                    if (instance.fields.get(name, &value)) {
                        _ = self.stack.pop();
                        self.stack.push(value);
                    } else {
                        try self.bindMethod(instance.class, name);
                    }
                },
                .OP_SET_PROPERTY => {
                    if (!self.stack.peek(1).isInstance()) {
                        self.runtimeError("Only instances have properties.", .{});
                        return InterpretError.RuntimeError;
                    }
                    const instance = self.stack.peek(1).asInstance();
                    const name = self.readString();
                    _ = try instance.fields.set(name, self.stack.peek(0));
                    const value = self.stack.pop();
                    _ = self.stack.pop();
                    self.stack.push(value);
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
                .OP_INVOKE => {
                    const method = self.readString();
                    const argCount = self.readByte().constant;
                    try self.invoke(method, argCount);
                },
                .OP_SUPER_INVOKE => {
                    const method = self.readString();
                    const argCount = self.readByte().constant;
                    const superclass = self.stack.pop().asClass();
                    try self.invokeFromClass(superclass, method, argCount);
                },
                .OP_CLOSURE => {
                    const function = self.readConstant().asFunction();
                    var closure = try obj.ObjClosure.create(self.allocator, function);
                    self.stack.push(val.Value.objVal(&closure.obj));
                    for (closure.upvalues) |*upvalue| {
                        const isLocal = if (self.readByte().constant == 1) cmp.Upvalue.Type.LOCAL else cmp.Upvalue.Type.UPVALUE;
                        const index = self.readByte().constant;
                        switch (isLocal) {
                            .LOCAL => {
                                upvalue.* = try self.captureUpvalue(&frame.slots[index]);
                            },
                            .UPVALUE => {
                                upvalue.* = frame.closure.upvalues[index];
                            },
                        }
                        closure.upvalueCount += 1;
                    }
                },
                .OP_CLOSE_UPVALUE => {
                    self.stack.closeUpvalues(&self.stack.stack[self.stack.stackTop - 1]);
                    _ = self.stack.pop();
                },
                .OP_CLASS => {
                    const class = try obj.ObjClass.create(self.allocator, self.readString(), &self.stack);
                    self.stack.push(val.Value.objVal(&class.obj));
                },
                .OP_INHERIT => {
                    const superclass = self.stack.peek(1);
                    if (!superclass.isClass()) {
                        self.runtimeError("Superclass must be a class.", .{});
                        return InterpretError.RuntimeError;
                    }
                    const subclass = self.stack.peek(0).asClass();
                    try subclass.methods.addAll(&superclass.asClass().methods);
                    _ = self.stack.pop();
                },
                .OP_GET_SUPER => {
                    const name = self.readString();
                    const superclass = self.stack.pop().asClass();
                    try self.bindMethod(superclass, name);
                },
                .OP_METHOD => {
                    try self.defineMethod(self.readString());
                },
                .OP_RETURN => {
                    const result = self.stack.pop();
                    if (frame.returnStackTop > 0) {
                        self.stack.closeUpvalues(&self.stack.stack[frame.returnStackTop - 1]);
                    }
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
        self.stack.push(val.Value.objVal(&(try obj.ObjNative.create(self.allocator, function)).obj));
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
    fn captureUpvalue(self: *VM, local: *val.Value) !*obj.ObjUpvalue {
        var prevUpvalue: ?*obj.ObjUpvalue = null;
        var currentUpvalue = self.stack.openUpvalues;
        while (currentUpvalue) |upvalue| {
            if (@intFromPtr(upvalue.location) <= @intFromPtr(local)) break;
            prevUpvalue = upvalue;
            currentUpvalue = upvalue.next;
        }
        if (currentUpvalue) |upvalue| {
            if (upvalue.location == local) {
                return upvalue;
            }
        }
        const createdUpvalue = try obj.ObjUpvalue.create(self.allocator, local);
        createdUpvalue.next = currentUpvalue;
        if (prevUpvalue) |upvalue| {
            upvalue.next = createdUpvalue;
        } else {
            self.stack.openUpvalues = createdUpvalue;
        }
        return createdUpvalue;
    }
    fn callValue(self: *VM, callee: val.Value, argCount: u8) !void {
        if (callee.isObj()) {
            switch (callee.objType()) {
                .BOUND_METHOD => {
                    const bound = callee.asBoundMethod();
                    self.stack.stack[self.stack.stackTop - argCount - 1] = bound.receiver;
                    return self.call(bound.method, argCount);
                },
                .CLOSURE => {
                    return self.call(callee.asClosure(), argCount);
                },
                .CLASS => {
                    const class = callee.asClass();
                    const instance = try obj.ObjInstance.create(self.allocator, class, &self.stack);
                    self.stack.stack[self.stack.stackTop - argCount - 1] = val.Value.objVal(&instance.obj);
                    var initializer = val.Value.nilVal();
                    if (class.methods.get(self.initString.?, &initializer)) {
                        return self.call(initializer.asClosure(), argCount);
                    } else if (argCount != 0) {
                        self.runtimeError("Expected 0 arguments but got {}.", .{argCount});
                        return InterpretError.RuntimeError;
                    }
                    return;
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
    fn call(self: *VM, closure: *obj.ObjClosure, argCount: u8) !void {
        if (argCount != closure.function.arity) {
            self.runtimeError("Expected {} arguments but got {}.", .{ closure.function.arity, argCount });
            return InterpretError.RuntimeError;
        }
        if (self.frameCount == FRAME_MAX) {
            self.runtimeError("Stack overflow.", .{});
            return InterpretError.RuntimeError;
        }
        self.frames[self.frameCount] = .{
            .closure = closure,
            .ip = closure.function.chunk.code.ptr,
            .slots = self.stack.stack[self.stack.stackTop - argCount - 1 ..],
            .returnStackTop = self.stack.stackTop - argCount - 1,
        };
        self.frameCount += 1;
    }
    fn invoke(self: *VM, name: *obj.ObjString, argCount: u8) !void {
        const receiver = self.stack.peek(argCount);
        if (!receiver.isInstance()) {
            self.runtimeError("Only instances have methods.", .{});
            return InterpretError.RuntimeError;
        }
        const instance = receiver.asInstance();
        var value = val.Value.nilVal();
        if (instance.fields.get(name, &value)) {
            self.stack.stack[self.stack.stackTop - argCount - 1] = value;
            return self.callValue(value, argCount);
        }
        return self.invokeFromClass(instance.class, name, argCount);
    }
    fn invokeFromClass(self: *VM, class: *obj.ObjClass, name: *obj.ObjString, argCount: u8) !void {
        var method = val.Value.nilVal();
        if (!class.methods.get(name, &method)) {
            self.runtimeError("Undefined property '{s}'.", .{name.chars});
            return InterpretError.RuntimeError;
        }
        return self.call(method.asClosure(), argCount);
    }
    fn defineMethod(self: *VM, name: *obj.ObjString) !void {
        const method = self.stack.peek(0);
        const class = self.stack.peek(1).asClass();
        _ = try class.methods.set(name, method);
        _ = self.stack.pop();
    }
    fn bindMethod(self: *VM, class: *obj.ObjClass, name: *obj.ObjString) !void {
        var method = val.Value.nilVal();
        if (!class.methods.get(name, &method)) {
            self.runtimeError("Undefined property '{s}'.", .{name.chars});
            return InterpretError.RuntimeError;
        }
        const receiver = self.stack.peek(0);
        const bound = try obj.ObjBoundMethod.create(self.allocator, receiver, method.asClosure());
        _ = self.stack.pop();
        self.stack.push(val.Value.objVal(&bound.obj));
    }
    fn markFrames(self: *VM) void {
        for (0..self.frameCount) |i| {
            self.frames[i].closure.obj.mark();
        }
    }
    pub fn markRoots(self: *VM) void {
        self.stack.markValues();
        self.stack.markUpvalues();
        self.globals.markEntries();
        self.markFrames();
        if (self.initString) |str| str.obj.mark();
    }
    fn readString(self: *VM) *obj.ObjString {
        return self.readConstant().asString();
    }
    fn readConstant(self: *VM) val.Value {
        const frame = &self.frames[self.frameCount - 1];
        return frame.closure.function.chunk.constants.values[self.readByte().constant];
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
            const function = frame.closure.function;
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
