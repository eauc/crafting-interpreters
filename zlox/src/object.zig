const std = @import("std");
const chk = @import("chunk.zig");
const val = @import("value.zig");

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
    hash: usize,
};

pub fn printObject(object: *Obj) void {
    switch (object.type) {
        .STRING => {
            const string: *ObjString = @fieldParentPtr("obj", object);
            std.debug.print("{s}", .{string.chars});
        },
    }
}

pub fn concatenateStrings(a: *ObjString, b: *ObjString, chunk: *chk.Chunk) !*Obj {
    const length = a.length + b.length;
    const chars: [:0]u8 = @ptrCast(try chunk.allocator.alloc(u8, length + 1));

    std.mem.copyForwards(u8, chars, a.chars);
    std.mem.copyForwards(u8, chars[a.length..], b.chars);
    chars[length] = 0;

    const hash = hashString(chars);
    const interned = chunk.strings.findString(chars, hash);
    if (interned) |string| {
        chunk.allocator.free(chars);
        return &string.obj;
    }
    return allocateString(chars, hash, chunk);
}

pub fn copyString(chars: []const u8, chunk: *chk.Chunk) !*Obj {
    const heapChars: [:0]u8 = @ptrCast(try chunk.allocator.alloc(u8, chars.len + 1));
    std.mem.copyForwards(u8, heapChars, chars);
    heapChars[chars.len] = 0;

    const hash = hashString(heapChars);
    const interned = chunk.strings.findString(heapChars, hash);
    if (interned) |string| {
        chunk.allocator.free(chars);
        return &string.obj;
    }
    return allocateString(heapChars, hash, chunk);
}

fn allocateString(chars: [:0]const u8, hash: usize, chunk: *chk.Chunk) !*Obj {
    const string = try chunk.allocator.create(ObjString);
    chunk.addObject(&string.obj);
    string.* = .{
        .obj = .{
            .type = .STRING,
            .next = null,
        },
        .length = chars.len - 1,
        .chars = chars,
        .hash = hash,
    };
    _ = try chunk.strings.set(string, val.Value.nilVal());
    return &string.obj;
}

fn hashString(chars: [:0]const u8) usize {
    var hash: usize = 2166136261;
    for (chars) |char| {
        if (char == 0) break;
        hash ^= char;
        hash *%= 16777619;
    }
    return hash;
}
