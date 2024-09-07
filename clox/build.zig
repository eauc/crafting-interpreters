const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "clox",
        .target = b.host,
    });
    exe.addCSourceFiles(.{
        .files = &.{ "src/main.c", "src/chunk.c", "src/memory.c", "src/debug.c", "src/value.c", "src/vm.c" },
        .flags = &.{
            "-std=c11",
            // "-gen-cdb-fragment-path", ".zig-cache/cdb"
        },
    });
    exe.linkSystemLibrary("c");

    b.installArtifact(exe);
    const run_exe = b.addRunArtifact(exe);

    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);
}
