const std = @import("std");
const chk = @import("chunk.zig");
const val = @import("value.zig");

pub const ObjType = enum {
    CLOSURE,
    FUNCTION,
    NATIVE,
    STRING,
    UPVALUE,
};

pub const Obj = struct {
    allocator: std.mem.Allocator,
    type: ObjType,
    next: ?*Obj,
    pub fn init(self: *Obj, allocator: std.mem.Allocator, objType: ObjType) void {
        self.allocator = allocator;
        self.type = objType;
        self.next = null;
    }
    pub fn free(self: *Obj) void {
        switch (self.type) {
            .CLOSURE => {
                var closure: *ObjClosure = @fieldParentPtr("obj", self);
                closure.free();
            },
            .FUNCTION => {
                var function: *ObjFunction = @fieldParentPtr("obj", self);
                function.free();
            },
            .NATIVE => {
                var native: *ObjNative = @fieldParentPtr("obj", self);
                native.free();
            },
            .STRING => {
                var string: *ObjString = @fieldParentPtr("obj", self);
                string.free();
            },
            .UPVALUE => {
                var upvalue: *ObjUpvalue = @fieldParentPtr("obj", self);
                upvalue.free();
            },
        }
    }
    pub fn print(self: *Obj) void {
        switch (self.type) {
            .CLOSURE => {
                const closure: *ObjClosure = @fieldParentPtr("obj", self);
                closure.print();
            },
            .FUNCTION => {
                const function: *ObjFunction = @fieldParentPtr("obj", self);
                function.print();
            },
            .NATIVE => {
                const native: *ObjNative = @fieldParentPtr("obj", self);
                native.print();
            },
            .STRING => {
                const string: *ObjString = @fieldParentPtr("obj", self);
                string.print();
            },
            .UPVALUE => {
                const upvalue: *ObjUpvalue = @fieldParentPtr("obj", self);
                upvalue.print();
            },
        }
    }
};

pub const ObjsList = struct {
    objects: ?*Obj,
    pub const default = ObjsList{
        .objects = null,
    };
    pub fn add(self: *ObjsList, object: *Obj) void {
        object.next = self.objects;
        self.objects = object;
    }
    pub fn free(self: *ObjsList) void {
        var o = self.objects;
        while (o) |object| {
            const next = object.next;
            object.free();
            o = next;
        }
    }
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: u8,
    upvalueCount: usize,
    chunk: chk.Chunk,
    name: ?*ObjString,

    pub fn create(allocator: std.mem.Allocator) !*ObjFunction {
        const function = try allocator.create(ObjFunction);
        function.obj.init(allocator, .FUNCTION);
        function.arity = 0;
        function.upvalueCount = 0;
        function.name = null;
        function.chunk.init(allocator);
        return function;
    }
    pub fn free(self: *ObjFunction) void {
        self.chunk.free();
        self.obj.allocator.destroy(self);
    }
    pub fn print(self: *const ObjFunction) void {
        if (self.name) |name| {
            std.debug.print("<fn {s}>", .{name.chars});
        } else {
            std.debug.print("<script>", .{});
        }
    }
};

pub const ObjClosure = struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: []*ObjUpvalue,
    upvalueCount: usize,

    pub fn create(allocator: std.mem.Allocator, function: *ObjFunction) !*ObjClosure {
        const closure = try allocator.create(ObjClosure);
        closure.obj.init(allocator, .CLOSURE);
        closure.function = function;
        closure.upvalues = try allocator.alloc(*ObjUpvalue, function.upvalueCount);
        closure.upvalueCount = function.upvalueCount;
        return closure;
    }
    pub fn free(self: *ObjClosure) void {
        self.obj.allocator.free(self.upvalues);
        self.obj.allocator.destroy(self);
    }
    pub fn print(self: *const ObjClosure) void {
        self.function.print();
    }
};

pub const ObjUpvalue = struct {
    obj: Obj,
    location: *val.Value,
    closed: val.Value,
    next: ?*ObjUpvalue,

    pub fn create(allocator: std.mem.Allocator, location: *val.Value) !*ObjUpvalue {
        const upvalue = try allocator.create(ObjUpvalue);
        upvalue.obj.init(allocator, .UPVALUE);
        upvalue.location = location;
        upvalue.closed = val.Value.nilVal();
        upvalue.next = null;
        return upvalue;
    }
    pub fn free(self: *ObjUpvalue) void {
        self.obj.allocator.destroy(self);
    }
    pub fn print(self: *const ObjUpvalue) void {
        _ = self;
        std.debug.print("upvalue", .{});
    }
};

pub const NativeFn = fn (args: []val.Value) val.Value;

pub const ObjNative = struct {
    obj: Obj,
    function: *const NativeFn,
    pub fn create(allocator: std.mem.Allocator, function: *const NativeFn) !*ObjNative {
        const native = try allocator.create(ObjNative);
        native.obj.init(allocator, .NATIVE);
        native.function = function;
        return native;
    }
    pub fn free(self: *ObjNative) void {
        self.obj.allocator.destroy(self);
    }
    pub fn print(self: *const ObjNative) void {
        _ = self;
        std.debug.print("<native fn>", .{});
    }
};

pub const ObjString = struct {
    obj: Obj,
    length: usize,
    hash: usize,
    chars: []const u8,

    pub fn create(allocator: std.mem.Allocator, chars: []const u8) !*ObjString {
        const string = try allocator.create(ObjString);
        string.obj.init(allocator, .STRING);
        string.length = chars.len - 1;
        string.chars = chars;
        string.hash = hashString(chars);
        return string;
    }
    pub fn free(self: *ObjString) void {
        self.obj.allocator.free(self.chars);
        self.obj.allocator.destroy(self);
    }
    pub fn print(self: *const ObjString) void {
        std.debug.print("{s}", .{self.chars});
    }
};

fn hashString(chars: []const u8) usize {
    var hash: usize = 2166136261;
    for (chars) |char| {
        hash ^= char;
        hash *%= 16777619;
    }
    return hash;
}
