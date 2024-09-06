const std = @import("std");

pub fn growCapacity(capacity: usize) usize {
    if (capacity < 8) {
        return 8;
    } else {
        return capacity * 2;
    }
}

pub fn growArray(comptime T: type, array: []T, oldCount: usize, newCount: usize, allocator: std.mem.Allocator) ![]T {
    if (oldCount == 0) {
        return allocator.alloc(T, newCount);
    } else {
        return allocator.realloc(array, newCount);
    }
}
