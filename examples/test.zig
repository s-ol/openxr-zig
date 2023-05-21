const std = @import("std");
const xr = @import("openxr");
const c = @import("c.zig");
const Allocator = std.mem.Allocator;

const BaseDispatch = xr.BaseWrapper(.{
    .createInstance = true,
});

const InstanceDispatch = xr.InstanceWrapper(.{
    .destroyInstance = true,
    .getSystem = true,
    .getSystemProperties = true,
    .createSession = true,
    .pollEvent = true,
});

fn getProcAddr(instance: xr.Instance, name: [*:0]const u8) !xr.PfnVoidFunction {
    var out: xr.PfnVoidFunction = undefined;
    const result = c.xrGetInstanceProcAddr(instance, name, &out);
    return switch (result) {
        .success => out,
        .error_handle_invalid => error.HandleInvalid,
        .error_instance_lost => error.InstanceLost,
        .error_runtime_failure => error.RuntimeFailure,
        .error_out_of_memory => error.OutOfMemory,
        .error_function_unsupported => error.FunctionUnsupported,
        .error_validation_failure => error.ValidationFailure,
        else => error.Unknown,
    };
}

pub fn main() !void {
    var name: [128]u8 = undefined;
    std.mem.copy(u8, name[0..], "openxr-zig-test" ++ [_]u8{0});

    const xrb = try BaseDispatch.load(c.xrGetInstanceProcAddr);

    const inst = try xrb.createInstance(&.{
        .application_info = .{
            .application_name = name,
            .application_version = 0,
            .engine_name = name,
            .engine_version = 0,
            .api_version = xr.makeVersion(1, 0, 0),
        },
    });

    const xri = try InstanceDispatch.load(inst, c.xrGetInstanceProcAddr);
    defer xri.destroyInstance(inst) catch unreachable;

    const system = try xri.getSystem(inst, &.{ .form_factor = .head_mounted_display });

    var system_properties = xr.SystemProperties.empty();
    try xri.getSystemProperties(inst, system, &system_properties);

    std.debug.print(
        \\system {}:
        \\  vendor Id: {}
        \\  systemName: {s}
        \\  gfx
        \\    max swapchain image resolution: {}x{}
        \\    max layer count: {}
        \\  tracking
        \\    orientation tracking: {}
        \\    positional tracking: {}
    , .{
        system,
        system_properties.vendor_id,
        system_properties.system_name,
        system_properties.graphics_properties.max_swapchain_image_width,
        system_properties.graphics_properties.max_swapchain_image_height,
        system_properties.graphics_properties.max_layer_count,
        system_properties.tracking_properties.orientation_tracking,
        system_properties.tracking_properties.position_tracking,
    });

    _ = try xri.createSession(inst, &.{
        .system_id = system,
    });
}
