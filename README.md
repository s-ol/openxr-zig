# openxr-zig

A OpenXR binding generator for Zig.
The generator is almost a verbatim copy of Snektron's [vulkan-zig][vulkan-zig], and all the hard work was done there.

## Overview

openxr-zig attempts to provide a better experience to programming OpenXR applications in Zig, by providing features such as integration of openxr errors with Zig's error system, function pointer loading, renaming fields to standard Zig style, better bitfield handling, turning out parameters into return values and more.

openxr-zig is automatically tested daily against the latest xr.xml and zig, and supports xr.xml from version 1.x.163.

### Zig versions

openxr-zig aims to be always compatible with the ever-changing Zig master branch (however, development may lag a few days behind). Sometimes, the Zig master branch breaks a bunch of functionality however, which may make the latest version openxr-zig incompatible with older releases of Zig. Versions compatible with older versions of zig are marked with the tag `zig-<version>`.

## Differences from Spec
* `XR_SESSION_STATE_LOSS_PENDING` results are treated as errorcodes, contrary to the spec. This way, the API wrapper returns them as part of the error union, so that the actual return type can be allocated more usefully.

## Features
### CLI-interface
A CLI-interface is provided to generate xr.zig from the [OpenXR XML registry](https://github.com/KhronosGroup/OpenXR-Docs/blob/master/xml), which is built by default when invoking `zig build` in the project root. To generate xr.zig, simply invoke the program as follows:
```
$ zig-cache/bin/openxr-zig-generator path/to/xr.xml output/path/to/xr.zig
```
This reads the xml file, parses its contents, renders the OpenXR bindings, and formats file, before writing the result to the output path. While the intended usage of openxr-zig is through direct generation from build.zig (see below), the CLI-interface can be used for one-off generation and vendoring the result.

### Generation from build.zig
OpenXR bindings can be generated from the OpenXR XML registry at compile time with build.zig, by using the provided OpenXR generation step:
```zig
const xrgen = @import("openxr-zig/generator/index.zig");

pub fn build(b: *Builder) void {
    ...
    const exe = b.addExecutable("my-executable", "src/main.zig");

    // Create a step that generates xr.zig (stored in zig-cache) from the provided openxr registry.
    const gen = xrgen.XrGenerateStep.init(b, "path/to/xr.xml", "xr.zig");
    exe.step.dependOn(&gen.step);

    // Add the generated file as package to the final executable
    exe.addPackagePath("openxr", gen.full_out_path);
}
```
This reads xr.xml, parses its contents, and renders the OpenXR bindings to "xr.zig", which is then formatted and placed in `zig-cache`. The resulting file can then be added to an executable by using `addPackagePath`.

### Function & field renaming
Functions and fields are renamed to be more or less in line with [Zig's standard library style](https://ziglang.org/documentation/master/#Style-Guide):
* The xr prefix is removed everywhere
  * Structs like `XrInstanceCreateInfo` are renamed to `InstanceCreateInfo`.
  * Handles like `XrSwapchainKHR` are renamed to `SwapchainKHR` (note that the tag is retained in caps).
  * Functions like `xrCreateInstance` are generated as `createInstance` as wrapper and as `PfnCreateInstance` as function pointer.
  * API constants like `XR_WHOLE_SIZE` retain screaming snake case, and are generates as `WHOLE_SIZE`.
* The type name is stripped from enumeration fields and bitflags, and they are generated in (lower) snake case. For example, `XR_ACTION_TYPE_BOOLEAN_INPUT` is generated as just `boolean_input`. Note that author tags are also generated to lower case: `XR_ANDROID_THREAD_TYPE_APPLICATION_MAIN_KHR` is translated to `application_main_khr`.
* Container fields and function parameter names are generated in (lower) snake case in a similar manner: `viewConfigurationType` becomes `view_configuration_type`.
* Any name which is either an illegal Zig name or a reserved identifier is rendered using `@"name"` syntax. For example, `XR_ENVIRONMENT_BLEND_MODE_OPAQUE` is translated to `@"opaque"`.

### Function pointers & Wrappers
openxr-zig provides no integration for statically linking libopenxr, and these symbols are not generated at all. Instead, openxr functions are to be loaded dynamically. For each OpenXR function, a function pointer type is generated using the exact parameters and return types as defined by the OpenXR specification:

```zig
pub const PfnCreateInstance = fn (
    create_info: *const InstanceCreateInfo,
    instance: *Instance,
) callconv(openxr_call_conv) Result;
```

fn getProcAddr(instance: xr.Instance, name: [*:0]const u8) xr.PfnVoidFunction {
    var out: xr.PfnVoidFunction = undefined;
    _ = c.xrGetInstanceProcAddr(instance, name, &out);
    return out;
}

For each function, a wrapper is generated into one of three structs:
* BaseWrapper. This contains wrappers for functions which are loaded by `xrGetInstanceProcAddr` without an instance, such as `xrCreateInstance`, `xrEnumerateApiLayerProperties`, etc.
* InstanceWrapper. This contains wrappers for functions which are otherwise loaded by `xrGetInstanceProcAddr`.

Each wrapper struct is to be used as a mixin on a struct containing **just** function pointers as members:
```zig
const xr = @import("openxr");
const BaseDispatch = struct {
    xrCreateInstance: xr.PfnCreateInstance,
    usingnamespace xr.BaseWrapper(@This());
};
```
The wrapper struct then provides wrapper functions for each function pointer in the dispatch struct:
```zig
pub const BaseWrapper(comptime Self: type) type {
    return struct {
        pub fn createInstance(
            self: Self,
            create_info: InstanceCreateInfo,
        ) error{
            OutOfMemory,
            LimitReached,
            InstanceLost,
            RuntimeFailure,
            InitializationFailed,
            ApiVersionUnsupported,
            ApiLayerNotPresent,
            ExtensionNotPresent,
            ValidationFailure,
            NameInvalid,
            Unknown,
        }!Instance {
            var instance: Instance = undefined;
            const result = self.xrCreateInstance(
                &create_info,
                &instance,
            );
            switch (result) {
                .success => {},
                .error_out_of_memory => return error.OutOfMemory,
                .error_limit_reached => return error.LimitReached,
                .error_instance_lost => return error.InstanceLost,
                .error_runtime_failure => return error.RuntimeFailure,
                .error_initialization_failed => return error.InitializationFailed,
                .error_api_version_unsupported => return error.ApiVersionUnsupported,
                .error_api_layer_not_present => return error.ApiLayerNotPresent,
                .error_extension_not_present => return error.ExtensionNotPresent,
                .error_validation_failure => return error.ValidationFailure,
                .error_name_invalid => return error.NameInvalid,
                else => return error.Unknown,
            }
            return instance;
        }

        ...
    }
}
```
Wrappers are generated according to the following rules:
* The return type is determined from the original return type and the parameters.
  * Any non-const, non-optional single-item pointer is interpreted as an out parameter.
  * If a command returns a non-error `XrResult` other than `XR_SUCCESS` it is also returned.
  * If there are multiple return values selected, an additional struct is generated. The original call's return value is called `return_value`, `XrResult` is named `result`, and the out parameters are called the same. They are generated in this order.
* Any const non-optional single-item pointer is interpreted as an in-parameter. For these, one level of indirection is removed so that create info structure pointers can now be passed as values, enabling the ability to use struct literals for these parameters.
* Error codes are translated into Zig errors.
* As of yet, there is no specific handling of enumeration style commands or other commands which accept slices.

Furthermore, each wrapper contains a function to load each function pointer member when passed `PfnGetInstanceProcAddr`, which attempts to load each member as function pointer and casts it to the appropriate type. These functions are loaded literally, and any wrongly named member or member with a wrong function pointer type will result in problems.
* For `BaseWrapper`, this function has signature `fn load(loader: PfnGetInstanceProcAddr) !Self`.
* For `InstanceWrapper`, this function has signature `fn load(instance: Instance, loader: PfnGetInstanceProcAddr) !Self`.

#### `openxr-loader`

By linking against `openxr_loader` and including `openxr.h`, `xrGetInstanceProcAddr` can be obtained and wrapped like so:

```zig
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
    }
}

const BaseDispatch = struct {
    xrCreateInstance: xr.PfnCreateInstance,
    usingnamespace xr.BaseWrapper(@This());
};

...

const xrb = try BaseDispatch.load(getProcAddr);
```

### Bitflags
Packed structs of bools are used for bit flags in openxr-zig, instead of both a `FlagBits` and `Flags64` variant. Places where either of these variants are used are both replaced by this packed struct instead. This means that even in places where just one flag would normally be accepted, the packed struct is accepted. The programmer is responsible for only enabling a single bit.

Each bit is defaulted to `false`, and the first `bool` is aligned to guarantee the overal alignment
of each Flags type to guarantee ABI compatibility when passing bitfields through structs:
```zig
pub const ViewStateFlags = packed struct {
    orientation_valid_bit: bool align(@alignOf(Flags64)) = false,
    position_valid_bit: bool = false,
    orientation_tracked_bit: bool = false,
    position_tracked_bit: bool = false,
    _reserved_bit_4: bool = false,
    _reserved_bit_5: bool = false,
    ...
    pub usingnamespace FlagsMixin(ViewStateFlags);
};
```
Note that on function call ABI boundaries, this alignment trick is not sufficient. Instead, the flags
are reinterpreted as an integer which is passed instead. Each flags type is augmented by a mixin which provides `IntType`, an integer which represents the flags on function ABI boundaries. This mixin also provides some common set operation on bitflags:
```zig
pub fn FlagsMixin(comptime FlagsType: type) type {
    return struct {
        pub const IntType = Flags64;

        // Return the integer representation of these flags
        pub fn toInt(self: FlagsType) IntType {...}

        // Turn an integer representation back into a flags type
        pub fn fromInt(flags: IntType) FlagsType { ... }

        // Return the set-union of `lhs` and `rhs.
        pub fn merge(lhs: FlagsType, rhs: FlagsType) FlagsType { ... }

        // Return the set-intersection of `lhs` and `rhs`.
        pub fn intersect(lhs: FlagsType, rhs: FlagsType) FlagsType { ... }

        // Return the set-complement of `lhs` and `rhs`. Note: this also inverses reserved bits.
        pub fn complement(self: FlagsType) FlagsType { ... }

        // Return the set-subtraction of `lhs` and `rhs`: All fields set in `rhs` are cleared in `lhs`.
        pub fn subtract(lhs: FlagsType, rhs: FlagsType) FlagsType { ... }

        // Returns whether all bits set in `rhs` are also set in `lhs`.
        pub fn contains(lhs: FlagsType, rhs: FlagsType) bool { ... }
    };
}
```

### Handles
Handles are generated to a non-exhaustive enum, backed by a `u64` for non-dispatchable handles and `usize` for dispatchable ones:
```zig
const Instance = extern enum(usize) { null_handle = 0, _ };
```
This means that handles are type-safe even when compiling for a 32-bit target.

### Structs
Defaults are generated for certain fields of structs:
* `type` is defaulted to the appropriate value.
* `next` is defaulted to `null`.
* for math primitives (`Vector*`, `Color*`, `Quaternionf`, `Offset*`, `Extent*`, `Posef`, `Rect*`), all fields are zero-initialized by default.
* No other fields have default values.

All structs contain an `empty()` function that returns an instance with only `type` and `next` set, which can be used whenever OpenXR requires an uninitialized (but typed) structure to output into.

```zig
pub const InstanceCreateInfo = extern struct {
    type: StructureType = .instance_create_info,
    next: ?*const c_void = null,
    create_flags: InstanceCreateFlags,
    application_info: ApplicationInfo,
    enabled_api_layer_count: u32,
    enabled_api_layer_names: [*]const [*:0]const u8,
    enabled_extension_count: u32,
    enabled_extension_names: [*]const [*:0]const u8,
    pub fn empty() @This() {
        var value: @This() = undefined;
        value.type = .instance_create_info;
        value.next = null;
        return value;
    }
};
```

### Pointer types
Pointer types in both commands (wrapped and function pointers) and struct fields are augmented with the following information, where available in the registry:
* Pointer optional-ness.
* Pointer const-ness.
* Pointer size: Either single-item, null-terminated or many-items.

Some of these are detected wrong, most notably `next`, which has been overriden to be optional in all cases.

## Limitations
* Currently, the self-hosted version of Zig's cache-hash API is not yet ready for usage, which means that the bindings are regenerated every time an executable is built.

* openxr-zig has as of yet no functionality for selecting feature levels and extensions when generating bindings. This is because when an extension is promoted to OpenXR core, its fields and commands are renamed to lose the extensions author tag. This leads to inconsistencies when only items from up to a certain feature level is included, as these promoted items then need to re-gain a tag.

[vulkan-zig]: https://github.com/Snektron/vulkan-zig
