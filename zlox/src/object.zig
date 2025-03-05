const std = @import("std");
const chk = @import("chunk.zig");
const mem = @import("memory.zig");
const tbl = @import("table.zig");
const val = @import("value.zig");
const vm = @import("vm.zig");

pub const ObjType = enum {
    CLASS,
    CLOSURE,
    FUNCTION,
    INSTANCE,
    NATIVE,
    STRING,
    UPVALUE,
};

pub const Obj = struct {
    allocator: *mem.Allocator,
    type: ObjType,
    isMarked: bool,
    next: ?*Obj,
    pub fn init(self: *Obj, allocator: *mem.Allocator, objType: ObjType) void {
        self.allocator = allocator;
        self.type = objType;
        self.isMarked = false;
        self.next = null;
        if (self.allocator.vm) |v| {
            v.objects.add(self);
        }
    }
    pub fn free(self: *Obj) void {
        if (mem.LOG_GC) {
            std.debug.print("0x{x} free {s}\n", .{
                @intFromPtr(self),
                @tagName(self.type),
            });
        }
        switch (self.type) {
            .CLASS => {
                var class: *ObjClass = @fieldParentPtr("obj", self);
                class.free();
            },
            .CLOSURE => {
                var closure: *ObjClosure = @fieldParentPtr("obj", self);
                closure.free();
            },
            .FUNCTION => {
                var function: *ObjFunction = @fieldParentPtr("obj", self);
                function.free();
            },
            .INSTANCE => {
                var instance: *ObjInstance = @fieldParentPtr("obj", self);
                instance.free();
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
    pub fn mark(self: *Obj) void {
        if (self.isMarked) {
            // if (mem.LOG_GC) {
            //     std.debug.print("0x{x} already marked ", .{@intFromPtr(self)});
            //     val.printValue(val.Value.objVal(self));
            //     std.debug.print("\n", .{});
            // }
            return;
        }
        if (mem.LOG_GC) {
            std.debug.print("0x{x} mark ", .{@intFromPtr(self)});
            val.printValue(val.Value.objVal(self));
            std.debug.print("\n", .{});
        }
        self.isMarked = true;
        self.allocator.addGray(self);
    }
    pub fn blacken(self: *Obj) void {
        if (mem.LOG_GC) {
            std.debug.print("0x{x} blacken ", .{@intFromPtr(self)});
            val.printValue(val.Value.objVal(self));
            std.debug.print("\n", .{});
        }
        switch (self.type) {
            .CLASS => {
                const class: *ObjClass = @fieldParentPtr("obj", self);
                class.name.obj.mark();
            },
            .CLOSURE => {
                const closure: *ObjClosure = @fieldParentPtr("obj", self);
                closure.function.obj.mark();
                std.debug.print("{d} upvalues\n", .{closure.upvalueCount});
                for (0..closure.upvalueCount) |i| {
                    std.debug.print("{d}\n", .{i});
                    closure.upvalues[i].obj.mark();
                    std.debug.print("{d}\n", .{i});
                }
            },
            .FUNCTION => {
                const function: *ObjFunction = @fieldParentPtr("obj", self);
                if (function.name) |name| {
                    name.obj.mark();
                }
                function.chunk.constants.mark();
            },
            .INSTANCE => {
                const instance: *ObjInstance = @fieldParentPtr("obj", self);
                instance.class.obj.mark();
                instance.fields.markEntries();
            },
            .NATIVE, .STRING => {},
            .UPVALUE => {
                const upvalue: *ObjUpvalue = @fieldParentPtr("obj", self);
                upvalue.closed.mark();
            },
        }
    }
    pub fn print(self: *Obj) void {
        switch (self.type) {
            .CLASS => {
                const class: *ObjClass = @fieldParentPtr("obj", self);
                class.print();
            },
            .CLOSURE => {
                const closure: *ObjClosure = @fieldParentPtr("obj", self);
                closure.print();
            },
            .FUNCTION => {
                const function: *ObjFunction = @fieldParentPtr("obj", self);
                function.print();
            },
            .INSTANCE => {
                const instance: *ObjInstance = @fieldParentPtr("obj", self);
                instance.print();
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
        if (mem.LOG_GC) {
            std.debug.print("0x{x} allocate {s}\n", .{
                @intFromPtr(object),
                @tagName(object.type),
            });
        }
    }
    pub fn free(self: *ObjsList) void {
        var o = self.objects;
        while (o) |object| {
            const next = object.next;
            object.free();
            o = next;
        }
    }
    pub fn sweep(self: *ObjsList) void {
        var previous: ?*Obj = null;
        var object = self.objects;
        while (object) |o| {
            // if (mem.LOG_GC) {
            //     std.debug.print("0x{x} sweep isMarked={} ", .{
            //         @intFromPtr(o),
            //         o.isMarked,
            //     });
            //     val.printValue(val.Value.objVal(o));
            //     std.debug.print("\n", .{});
            // }
            if (o.isMarked) {
                o.isMarked = false;
                previous = o;
                object = o.next;
            } else {
                var unreached = o;
                object = o.next;
                if (previous) |p| {
                    p.next = object;
                } else {
                    self.objects = object;
                }
                unreached.free();
            }
        }
    }
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: u8,
    upvalueCount: usize,
    chunk: chk.Chunk,
    name: ?*ObjString,

    pub fn create(allocator: *mem.Allocator) !*ObjFunction {
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
        self.obj.allocator.destroy(ObjFunction, self);
    }
    pub fn print(self: *const ObjFunction) void {
        if (self.name) |name| {
            std.debug.print("<fn {s}>", .{name.chars});
        } else {
            std.debug.print("<script>", .{});
        }
    }
};

pub const ObjClass = struct {
    obj: Obj,
    name: *ObjString,

    pub fn create(allocator: *mem.Allocator, name: *ObjString) !*ObjClass {
        const class = try allocator.create(ObjClass);
        class.obj.init(allocator, .CLASS);
        class.name = name;
        return class;
    }
    pub fn free(self: *ObjClass) void {
        self.obj.allocator.destroy(ObjClass, self);
    }
    pub fn print(self: *const ObjClass) void {
        std.debug.print("<class {s}>", .{self.name.chars});
    }
};

pub const ObjInstance = struct {
    obj: Obj,
    class: *ObjClass,
    fields: tbl.Table,

    pub fn create(allocator: *mem.Allocator, class: *ObjClass, stack: *vm.Stack) !*ObjInstance {
        const instance = try allocator.create(ObjInstance);
        instance.obj.init(allocator, .INSTANCE);
        instance.class = class;
        instance.fields.init(allocator, stack);
        return instance;
    }
    pub fn free(self: *ObjInstance) void {
        self.fields.free();
        self.obj.allocator.destroy(ObjInstance, self);
    }
    pub fn print(self: *const ObjInstance) void {
        std.debug.print("<instance of {s}>", .{self.class.name.chars});
    }
};

pub const ObjClosure = struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: []*ObjUpvalue,
    upvalueCount: usize,

    pub fn create(allocator: *mem.Allocator, function: *ObjFunction) !*ObjClosure {
        const upvalues = try allocator.alloc(*ObjUpvalue, function.upvalueCount);
        const closure = try allocator.create(ObjClosure);
        closure.obj.init(allocator, .CLOSURE);
        closure.function = function;
        closure.upvalues = upvalues;
        closure.upvalueCount = 0;
        return closure;
    }
    pub fn free(self: *ObjClosure) void {
        self.obj.allocator.free(*ObjUpvalue, self.upvalues);
        self.obj.allocator.destroy(ObjClosure, self);
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

    pub fn create(allocator: *mem.Allocator, location: *val.Value) !*ObjUpvalue {
        const upvalue = try allocator.create(ObjUpvalue);
        upvalue.obj.init(allocator, .UPVALUE);
        upvalue.location = location;
        upvalue.closed = val.Value.nilVal();
        upvalue.next = null;
        return upvalue;
    }
    pub fn free(self: *ObjUpvalue) void {
        self.obj.allocator.destroy(ObjUpvalue, self);
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

    pub fn create(allocator: *mem.Allocator, function: *const NativeFn) !*ObjNative {
        const native = try allocator.create(ObjNative);
        native.obj.init(allocator, .NATIVE);
        native.function = function;
        return native;
    }
    pub fn free(self: *ObjNative) void {
        self.obj.allocator.destroy(ObjNative, self);
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
    chars: []u8,

    pub fn create(allocator: *mem.Allocator, chars: []u8, hash: usize) !*ObjString {
        const string = try allocator.create(ObjString);
        string.obj.init(allocator, .STRING);
        string.length = chars.len - 1;
        string.chars = chars;
        string.hash = hash;
        return string;
    }
    pub fn free(self: *ObjString) void {
        self.obj.allocator.free(u8, self.chars);
        self.obj.allocator.destroy(ObjString, self);
    }
    pub fn print(self: *const ObjString) void {
        std.debug.print("{s}", .{self.chars});
    }
};
