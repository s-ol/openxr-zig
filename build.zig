const std = @import("std");
const xrgen = @import("generator/index.zig");

const XrGenerateStep = xrgen.XrGenerateStep;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const xr_xml_path: ?[]const u8 = b.option([]const u8, "registry", "Override the path to the OpenXR registry");

    // using the package manager, this artifact can be obtained by the user
    // through `b.dependency(<name in build.zig.zon>, .{}).artifact("generator")`.
    // with that, the user need only `.addArg("path/to/xr.xml")`, and then obtain
    // a file source to the generated code with `.addOutputArg("xr.zig")`
    const generator_exe = b.addExecutable(.{
        .name = "generator",
        .root_source_file = .{ .path = "generator/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(generator_exe);

    // or they can skip all that, and just make sure to pass `.registry = "path/to/xr.xml"` to `b.dependency`,
    // and then obtain the module directly via `.module("openxr-zig")`.
    if (xr_xml_path) |path| {
        const generate_cmd = b.addRunArtifact(generator_exe);

        if (!std.fs.path.isAbsolute(path)) @panic("Make sure to assign an absolute path to the `registry` option (see: std.Build.pathFromRoot).\n");
        generate_cmd.addArg(path);

        _ = b.addModule("openxr-zig", .{
            .source_file = generate_cmd.addOutputFileArg("xr.zig"),
        });
    }

    // remainder of the script is for examples/testing

    const example_exe = b.addExecutable(.{
        .name = "example",
        .root_source_file = .{ .path = "examples/test.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(example_exe);
    example_exe.linkLibC();
    example_exe.linkSystemLibrary("openxr_loader");

    const example_registry = b.option([]const u8, "example-registry", "Override the path to the OpenXR registry used for the examples") orelse "examples/xr.xml";
    const gen = XrGenerateStep.create(b, example_registry);
    example_exe.addModule("openxr", gen.getModule());

    const xr_zig_install_step = b.addInstallFile(gen.getSource(), "src/xr.zig");
    b.getInstallStep().dependOn(&xr_zig_install_step.step);

    const example_run_cmd = b.addRunArtifact(example_exe);
    example_run_cmd.step.dependOn(b.getInstallStep());

    const example_run_step = b.step("run-example", "Run the example");
    example_run_step.dependOn(&example_run_cmd.step);

    var test_target = b.addTest(.{
        .root_source_file = .{ .path = "generator/index.zig" },
    });

    var test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&test_target.step);
}
