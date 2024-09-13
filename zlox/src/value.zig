const std = @import("std");
const mem = @import("memory.zig");

pub const Value = f64;

pub fn printValue(value: Value) void {
    std.debug.print("{d}", .{value});
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
