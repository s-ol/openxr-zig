const std = @import("std");
const xrgen = @import("generator/index.zig");

const XrGenerateStep = xrgen.XrGenerateStep;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const xr_xml_path: ?[]const u8 = b.option([]const u8, "registry", "Override the path to the OpenXR registry");
    const test_step = b.step("test", "Run all the tests");

    const root_module = b.createModule(.{
        .root_source_file = b.path("generator/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // using the package manager, this artifact can be obtained by the user
    // through `b.dependency(<name in build.zig.zon>, .{}).artifact("openxr-zig-generator")`.
    // with that, the user need only `.addArg("path/to/xr.xml")`, and then obtain
    // a file source to the generated code with `.addOutputArg("xr.zig")`
    const generator_exe = b.addExecutable(.{
        .name = "openxr-zig-generator",
        .root_module = root_module,
    });
    b.installArtifact(generator_exe);

    // or they can skip all that, and just make sure to pass `.registry = "path/to/xr.xml"` to `b.dependency`,
    // and then obtain the module directly via `.module("openxr-zig")`.
    if (xr_xml_path) |path| {
        const generate_cmd = b.addRunArtifact(generator_exe);

        if (!std.fs.path.isAbsolute(path)) @panic("Make sure to assign an absolute path to the `registry` option (see: std.Build.pathFromRoot).\n");
        generate_cmd.addArg(path);

        const xr_zig = generate_cmd.addOutputFileArg("xr.zig");
        _ = b.addModule("openxr-zig", .{
            .root_source_file = xr_zig,
        });

        // Also install xr.zig, if passed.

        const xr_zig_install_step = b.addInstallFile(xr_zig, "src/xr.zig");
        b.getInstallStep().dependOn(&xr_zig_install_step.step);
    }

    const test_target = b.addTest(.{ .root_module = root_module });
    test_step.dependOn(&b.addRunArtifact(test_target).step);
}
