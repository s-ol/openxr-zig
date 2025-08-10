const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const maybe_override_registry = b.option([]const u8, "override-registry", "Override the path to the Vulkan registry used for the examples");

    const exe = b.addExecutable(.{
        .name = "openxr_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test.zig"),
            .target = target,
            .link_libc = true,
            .optimize = optimize,
        }),
    });
    b.installArtifact(exe);
    if (b.option([]const u8, "libdir", "The directory where openxr_loader.lib is located")) |libdir| {
        exe.addLibraryPath(.{ .cwd_relative = libdir });
    }
    exe.linkSystemLibrary("openxr_loader");

    const registry_path: std.Build.LazyPath = if (maybe_override_registry) |override_registry|
        .{ .cwd_relative = override_registry }
    else
        b.path("xr.xml");

    const openxr = b.dependency("openxr_zig", .{
        .registry = registry_path.getPath(b),
    }).module("openxr-zig");

    exe.root_module.addImport("openxr", openxr);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run-openxr", "Run the openxr example");
    run_step.dependOn(&run_cmd.step);
}
