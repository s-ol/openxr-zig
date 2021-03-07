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
    
    const example_exe = b.addExecutable("example", "examples/test.zig");
    example_exe.setTarget(target);
    example_exe.setBuildMode(mode);
    example_exe.install();
    example_exe.linkSystemLibrary("c");
    example_exe.linkSystemLibrary("openxr_loader");

    const xr_xml_path = b.option([]const u8, "vulkan-registry", "Override the to the Vulkan registry") orelse "examples/xr.xml";

    const gen = xrgen.XrGenerateStep.init(b, xr_xml_path, "xr.zig");
    example_exe.step.dependOn(&gen.step);
    example_exe.addPackage(gen.package);

    const example_run_cmd = example_exe.run();
    example_run_cmd.step.dependOn(b.getInstallStep());
    const example_run_step = b.step("run-example", "Run the example example");
    example_run_step.dependOn(&example_run_cmd.step);
}
