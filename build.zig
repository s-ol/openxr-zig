const std = @import("std");
const xrgen = @import("generator/index.zig");
const Step = std.build.Step;
const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    var test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addTest("generator/index.zig").step);

    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const generator_exe = b.addExecutable("openxr-zig-generator", "generator/main.zig");
    generator_exe.setTarget(target);
    generator_exe.setBuildMode(mode);
    generator_exe.install();
}
