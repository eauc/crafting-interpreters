const std = @import("std");
const mem = @import("memory.zig");
const obj = @import("object.zig");

pub const Number = f64;

const ValueType = enum {
    BOOL,
    NIL,
    NUMBER,
    OBJ,
};

pub const Value = struct {
    type: ValueType,
    as: union(ValueType) {
        BOOL: bool,
        NIL: void,
        NUMBER: Number,
        OBJ: *obj.Obj,
    },
    pub fn boolVal(value: bool) Value {
        return Value{
            .type = .BOOL,
            .as = .{ .BOOL = value },
        };
    }
    pub fn nilVal() Value {
        return Value{
            .type = .NIL,
            .as = .{ .NIL = {} },
        };
    }
    pub fn numberVal(value: Number) Value {
        return Value{
            .type = .NUMBER,
            .as = .{ .NUMBER = value },
        };
    }
    pub fn objVal(value: *obj.Obj) Value {
        return Value{
            .type = .OBJ,
            .as = .{ .OBJ = value },
        };
    }
    pub fn isBool(self: Value) bool {
        return self.type == .BOOL;
    }
    pub fn isNil(self: Value) bool {
        return self.type == .NIL;
    }
    pub fn isNumber(self: Value) bool {
        return self.type == .NUMBER;
    }
    pub fn isObj(self: Value) bool {
        return self.type == .OBJ;
    }
    pub fn isObjType(self: Value, expectedObjType: obj.ObjType) bool {
        return self.isObj() and self.objType() == expectedObjType;
    }
    pub fn isString(self: Value) bool {
        return self.isObjType(.STRING);
    }
    pub fn asBool(self: Value) bool {
        return self.as.BOOL;
    }
    pub fn asNumber(self: Value) Number {
        return self.as.NUMBER;
    }
    pub fn asObj(self: Value) *obj.Obj {
        return self.as.OBJ;
    }
    pub fn asString(self: Value) *obj.ObjString {
        return @fieldParentPtr("obj", self.asObj());
    }
    pub fn objType(self: Value) obj.ObjType {
        return self.asObj().type;
    }
    pub fn isFalsey(self: Value) bool {
        switch (self.type) {
            .NIL => return true,
            .BOOL => return !self.as.BOOL,
            else => return false,
        }
    }
    pub fn equals(self: Value, other: Value) bool {
        if (self.type != other.type) return false;
        switch (self.type) {
            .BOOL => return self.as.BOOL == other.as.BOOL,
            .NIL => return true,
            .NUMBER => return self.as.NUMBER == other.as.NUMBER,
            .OBJ => return self.as.OBJ == other.as.OBJ,
        }
    }
};

pub fn printValue(value: Value) void {
    switch (value.type) {
        .BOOL => std.debug.print("{}", .{value.as.BOOL}),
        .NIL => std.debug.print("nil", .{}),
        .NUMBER => std.debug.print("{d}", .{value.as.NUMBER}),
        .OBJ => obj.printObject(value.asObj()),
    }
}

pub const ValueArray = struct {
    allocator: std.mem.Allocator,
    count: usize,
    values: []Value,
    pub const default: ValueArray = .{
        .allocator = undefined,
        .count = 0,
        .values = &[_]Value{},
    };
    pub fn init(self: *ValueArray, allocator: std.mem.Allocator) void {
        self.allocator = allocator;
        self.count = 0;
        self.values = &[_]Value{};
    }
    pub fn free(self: *ValueArray) void {
        self.allocator.free(self.values);
    }
    pub fn write(self: *ValueArray, value: Value) !void {
        if (self.values.len < self.count + 1) {
            const oldCapacity = self.values.len;
            const newCapacity = mem.growCapacity(oldCapacity);
            self.values = try mem.growArray(Value, self.values, oldCapacity, newCapacity, self.allocator);
        }
        self.values[self.count] = value;
        self.count += 1;
    }
};
