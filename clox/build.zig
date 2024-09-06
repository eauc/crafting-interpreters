const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "clox",
        .target = b.host,
    });
    exe.addCSourceFiles(.{ .files = &.{ "src/main.c", "src/chunk.c", "src/memory.c", "src/debug.c", "src/value.c" }, .flags = &.{"-std=c11"} });
    exe.linkSystemLibrary("c");
    b.installArtifact(exe);

    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);
}
