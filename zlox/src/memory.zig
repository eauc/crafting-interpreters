const std = @import("std");
const cmp = @import("compiler.zig");
const obj = @import("object.zig");
const vm = @import("vm.zig");

const STRESS_GC = true;
pub const LOG_GC = true;
const GC_HEAP_GROWTH_FACTOR = 2;

pub const Allocator = struct {
    allocator: std.mem.Allocator,
    bytesAllocated: usize = 0,
    nextGC: usize = 1024 * 1024,
    vm: ?*vm.VM = null,
    compiler: ?*cmp.Compiler = null,
    grayStack: []*obj.Obj = &[_]*obj.Obj{},
    grayCount: usize = 0,
    grayCapacity: usize = 0,

    pub fn init(allocator: std.mem.Allocator) Allocator {
        return Allocator{
            .allocator = allocator,
        };
    }
    pub fn create(self: *Allocator, comptime T: type) !*T {
        const slice = try self.realloc(T, null, 0, 1);
        return &slice[0];
    }
    pub fn alloc(self: *Allocator, comptime T: type, count: usize) ![]T {
        return self.realloc(T, null, 0, count);
    }
    pub fn realloc(self: *Allocator, comptime T: type, oldmem: ?[]T, old_n: usize, new_n: usize) ![]T {
        self.bytesAllocated += (new_n - old_n) * @sizeOf(T);
        if (STRESS_GC and new_n > old_n) {
            self.collectGarbage();
        } else if (self.bytesAllocated > self.nextGC) {
            self.collectGarbage();
        }
        if (new_n == 0) {
            if (oldmem) |mem| {
                self.allocator.free(mem);
            }
            return &[_]T{};
        }
        if (oldmem) |mem| {
            return self.allocator.realloc(mem, new_n);
        }
        return self.allocator.alloc(T, new_n);
    }
    pub fn destroy(self: *Allocator, comptime T: type, ptr: *T) void {
        if (self.bytesAllocated > @sizeOf(T)) {
            self.bytesAllocated -= @sizeOf(T);
        } else {
            self.bytesAllocated = 0;
        }
        self.allocator.destroy(ptr);
    }
    pub fn free(self: *Allocator, comptime T: type, mem: []T) void {
        if (self.bytesAllocated > mem.len * @sizeOf(T)) {
            self.bytesAllocated -= mem.len * @sizeOf(T);
        } else {
            self.bytesAllocated = 0;
        }
        self.allocator.free(mem);
    }
    pub fn collectGarbage(self: *Allocator) void {
        const before = self.bytesAllocated;
        if (LOG_GC) {
            std.debug.print("-- gc begin\n", .{});
        }
        self.markRoots();
        self.traceReferences();
        self.removeWhite();
        self.sweep();
        self.nextGC = self.bytesAllocated * GC_HEAP_GROWTH_FACTOR;
        if (LOG_GC) {
            std.debug.print("-- gc end\n", .{});
            std.debug.print("   collected {d} bytes (from {d} to {d}) next as {d}\n", .{
                before - self.bytesAllocated,
                before,
                self.bytesAllocated,
                self.nextGC,
            });
        }
    }
    fn markRoots(self: *Allocator) void {
        if (self.vm) |v| {
            v.markRoots();
        }
        if (self.compiler) |c| {
            c.markRoots();
        }
    }
    fn traceReferences(self: *Allocator) void {
        while (self.grayCount > 0) {
            self.grayCount -= 1;
            self.grayStack[self.grayCount].blacken();
        }
    }
    fn removeWhite(self: *Allocator) void {
        if (self.vm) |v| {
            v.strings.removeWhite();
        }
    }
    fn sweep(self: *Allocator) void {
        if (self.vm) |v| {
            v.objects.sweep();
        }
    }
    pub fn addGray(self: *Allocator, object: *obj.Obj) void {
        if (self.grayCapacity < self.grayCount + 1) {
            self.grayCapacity = growCapacity(self.grayCapacity);
            self.grayStack = if (self.grayCapacity == 0)
                self.allocator.alloc(*obj.Obj, self.grayCapacity) catch unreachable
            else
                self.allocator.realloc(self.grayStack, self.grayCapacity) catch unreachable;
        }
        self.grayStack[self.grayCount] = object;
        self.grayCount += 1;
    }
};

pub fn growCapacity(capacity: usize) usize {
    if (capacity < 8) {
        return 8;
    } else {
        return capacity * 2;
    }
}

pub fn growArray(comptime T: type, array: []T, oldCount: usize, newCount: usize, allocator: *Allocator) ![]T {
    if (oldCount == 0) {
        return allocator.alloc(T, newCount);
    } else {
        return allocator.realloc(T, array, oldCount, newCount);
    }
}
