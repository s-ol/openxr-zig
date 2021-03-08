const std = @import("std");
const xr = @import("openxr");
const c = @import("c.zig");
const Allocator = std.mem.Allocator;

const BaseDispatch = struct {
    xrCreateInstance: xr.PfnCreateInstance,
    usingnamespace xr.BaseWrapper(@This());
};

const InstanceDispatch = struct {
    xrDestroyInstance: xr.PfnDestroyInstance,
    xrGetSystem: xr.PfnGetSystem,
    xrGetSystemProperties: xr.PfnGetSystemProperties,
    xrCreateSession: xr.PfnCreateSession,
    xrPollEvent: xr.PfnPollEvent,
    usingnamespace xr.InstanceWrapper(@This());
};

const SessionDispatch = struct {
    xrBeginSession: xr.PfnBeginSession,
    xrEndSession: xr.PfnEndSession,
    usingnamespace xr.SessionWrapper(@This());
};

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
    const zero = [_:0]u8{0};

    const xrb = try BaseDispatch.load(getProcAddr);
    const inst = try xrb.createInstance(.{
        .create_flags = .{},
        .application_info = .{
            .application_name = name,
            .application_version = 0,
            .engine_name = name,
            .engine_version = 0,
            .api_version = xr.makeVersion(1, 0, 0),
        },
        .enabled_api_layer_count = 0,
        .enabled_api_layer_names = @ptrCast([*]const [*:0]const u8, &zero),
        .enabled_extension_count = 0,
        .enabled_extension_names = @ptrCast([*]const [*:0]const u8, &zero),
    });

    const xri = try InstanceDispatch.load(inst, getProcAddr);
    defer xri.destroyInstance(inst) catch unreachable;

    const system = try xri.getSystem(inst, .{ .form_factor = .head_mounted_display });

    var system_properties: xr.SystemProperties = undefined;
    system_properties.type = .system_properties;
    system_properties.next = null;
    try xri.getSystemProperties(inst, system, &system_properties);

    std.debug.print(
        \\system {}:
        \\  vendor Id: {}
        \\  systemName: {}
        \\  gfx
        \\	  max swapchain image resolution: {}x{}
        \\	  max layer count: {}
        \\  tracking
        \\	  orientation tracking: {}
        \\	  positional tracking: {}
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

    const session = try xri.createSession(inst, .{
        .create_flags = .{},
        .system_id = system,
    });
}
