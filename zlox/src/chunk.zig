const std = @import("std");
const mem = @import("memory.zig");
const obj = @import("object.zig");
const tbl = @import("table.zig");
const val = @import("value.zig");

pub const Instruction = enum(u8) {
    OP_CONSTANT,
    OP_NIL,
    OP_NOT,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_NEGATE,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_DEFINE_GLOBAL,
    OP_PRINT,
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
    lines: []isize,
    constants: val.ValueArray,
    globals: tbl.Table,
    strings: tbl.Table,
    objects: ?*obj.Obj,
    pub const default: Chunk = .{
        .allocator = undefined,
        .count = 0,
        .code = &[_]OpCode{},
        .lines = &[_]isize{},
        .constants = val.ValueArray.default,
        .globals = tbl.Table.default,
        .strings = tbl.Table.default,
        .objects = null,
    };
    pub fn init(self: *Chunk, allocator: std.mem.Allocator) void {
        self.allocator = allocator;
        self.count = 0;
        self.code = &[_]OpCode{};
        self.lines = &[_]isize{};
        self.constants.init(allocator);
        self.globals.init(allocator);
        self.strings.init(allocator);
    }
    pub fn free(self: *Chunk) void {
        self.freeObjects();
        self.globals.free();
        self.strings.free();
        self.constants.free();
        self.allocator.free(self.code);
        self.allocator.free(self.lines);
        self.init(self.allocator);
    }
    fn freeObjects(self: *Chunk) void {
        var o = self.objects;
        while (o) |object| {
            const next = object.next;
            object.free(self.allocator);
            o = next;
        }
    }
    pub fn write(self: *Chunk, opCode: OpCode, line: isize) !void {
        if (self.code.len < self.count + 1) {
            const oldCapacity = self.code.len;
            const newCapacity = mem.growCapacity(oldCapacity);
            self.code = try mem.growArray(OpCode, self.code, oldCapacity, newCapacity, self.allocator);
            self.lines = try mem.growArray(isize, self.lines, oldCapacity, newCapacity, self.allocator);
        }
        self.code[self.count] = opCode;
        self.lines[self.count] = line;
        self.count += 1;
    }
    pub fn defineGlobal(self: *Chunk, name: *obj.ObjString, value: val.Value) !void {
        _ = try self.globals.set(name, value);
    }
    pub fn getGlobal(self: *Chunk, name: *obj.ObjString, value: *val.Value) bool {
        return self.globals.get(name, value);
    }
    pub fn setGlobal(self: *Chunk, name: *obj.ObjString, value: val.Value) !bool {
        return self.globals.set(name, value);
    }
    pub fn deleteGlobal(self: *Chunk, name: *obj.ObjString) void {
        _ = self.globals.delete(name);
    }
    pub fn addConstant(self: *Chunk, value: val.Value) !usize {
        try self.constants.write(value);
        return self.constants.count - 1;
    }
    pub fn addObject(self: *Chunk, object: *obj.Obj) void {
        object.next = self.objects;
        self.objects = object;
    }
};
