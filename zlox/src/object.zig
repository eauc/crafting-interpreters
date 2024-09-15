const std = @import("std");

pub const ObjType = enum {
    STRING,
};

pub const Obj = struct {
    type: ObjType,
    next: ?*Obj,
    pub fn free(self: *Obj, allocator: std.mem.Allocator) void {
        switch (self.type) {
            .STRING => {
                const objString: *ObjString = @fieldParentPtr("obj", self);
                allocator.free(objString.chars);
                allocator.destroy(objString);
            },
        }
    }
};

pub const ObjString = struct {
    obj: Obj,
    length: usize,
    chars: [:0]const u8,
};

pub fn printObject(object: *Obj) void {
    switch (object.type) {
        .STRING => {
            const string: *ObjString = @fieldParentPtr("obj", object);
            std.debug.print("{s}", .{string.chars});
        },
    }
}

pub fn concatenateStrings(a: *ObjString, b: *ObjString, allocator: std.mem.Allocator) !*Obj {
    const length = a.length + b.length;
    const chars: [:0]u8 = @ptrCast(try allocator.alloc(u8, length + 1));

    std.mem.copyForwards(u8, chars, a.chars);
    std.mem.copyForwards(u8, chars[a.length..], b.chars);
    chars[length] = 0;

    return allocateString(chars, allocator);
}

pub fn copyString(chars: []const u8, allocator: std.mem.Allocator) !*Obj {
    const heapChars: [:0]u8 = @ptrCast(try allocator.alloc(u8, chars.len + 1));
    std.mem.copyForwards(u8, heapChars, chars);
    heapChars[chars.len] = 0;
    return allocateString(heapChars, allocator);
}

fn allocateString(chars: [:0]const u8, allocator: std.mem.Allocator) !*Obj {
    const string = try allocator.create(ObjString);
    string.* = .{
        .obj = .{
            .type = .STRING,
            .next = null,
        },
        .length = chars.len - 1,
        .chars = chars,
    };
    return &string.obj;
}
