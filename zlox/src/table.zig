const std = @import("std");
const mem = @import("memory.zig");
const obj = @import("object.zig");
const val = @import("value.zig");

const TABLE_MAX_LOAD = 0.75;

const Entry = struct {
    key: ?*obj.ObjString,
    value: val.Value,
};

pub const Table = struct {
    allocator: std.mem.Allocator,
    count: usize,
    capacity: usize,
    entries: []Entry,
    pub const default = .{
        .allocator = undefined,
        .count = 0,
        .capacity = 0,
        .entries = &[_]Entry{},
    };
    pub fn init(self: *Table, allocator: std.mem.Allocator) void {
        self.allocator = allocator;
        self.count = 0;
        self.capacity = 0;
        self.entries = &[_]Entry{};
    }
    pub fn free(self: *Table) void {
        self.allocator.free(self.entries);
        self.init(self.allocator);
    }
    pub fn get(self: *Table, key: *obj.ObjString, value: *val.Value) bool {
        if (self.count == 0) return false;
        const entry = findEntry(self.entries, self.capacity, key);
        if (entry.key) {
            value.* = entry.value;
            return true;
        } else {
            return false;
        }
    }
    pub fn findString(self: *Table, chars: []const u8, hash: usize) ?*obj.ObjString {
        if (self.count == 0) return null;
        var index = hash % self.capacity;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key) |key| {
                if (key.hash == hash and std.mem.eql(u8, key.chars, chars)) {
                    return key;
                }
            } else {
                if (entry.value.isNil()) {
                    return null;
                }
            }
            index = (index + 1) % self.capacity;
        }
    }
    pub fn set(self: *Table, key: *obj.ObjString, value: val.Value) !bool {
        if (@as(f32, @floatFromInt(self.count + 1)) > @as(f32, @floatFromInt(self.capacity)) * TABLE_MAX_LOAD) {
            const capacity = mem.growCapacity(self.capacity);
            try self.adjustCapacity(capacity);
        }
        const entry = findEntry(self.entries, self.capacity, key);
        const isNewKey = entry.key == null;
        if (isNewKey and entry.value.isNil()) {
            self.count += 1;
        }
        entry.key = key;
        entry.value = value;
        return isNewKey;
    }
    pub fn delete(self: *Table, key: *obj.ObjString) bool {
        if (self.count == 0) return false;
        const entry = findEntry(self.entries, self.capacity, key);
        if (entry.key) {
            entry.key = null;
            entry.value = val.Value.nilVal();
            return true;
        } else {
            return false;
        }
    }
    pub fn addAll(self: *Table, other: *Table) void {
        for (other.entries) |entry| {
            if (entry.key) |key| {
                self.set(key, entry.value);
            }
        }
    }
    fn adjustCapacity(self: *Table, capacity: usize) !void {
        const entries = try self.allocator.alloc(Entry, capacity);
        for (entries) |*entry| {
            entry.key = null;
            entry.value = val.Value.nilVal();
        }
        self.count = 0;
        for (self.entries) |oldEntry| {
            if (oldEntry.key) |key| {
                const dest = findEntry(entries, capacity, key);
                dest.key = key;
                dest.value = oldEntry.value;
                self.count += 1;
            }
        }
        self.allocator.free(self.entries);
        self.entries = entries;
        self.capacity = capacity;
    }
};

fn findEntry(entries: []Entry, capacity: usize, key: *obj.ObjString) *Entry {
    var index = key.hash % capacity;
    var tombstone: ?*Entry = null;
    while (true) {
        const entry = &entries[index];
        if (entry.key) |entryKey| {
            if (entryKey == key) {
                return entry;
            }
        } else {
            if (val.Value.isNil(entry.value)) {
                return tombstone orelse entry;
            } else {
                tombstone = tombstone orelse entry;
            }
        }
        index = (index + 1) % capacity;
    }
}
