const std = @import("std");
const reg = @import("registry.zig");
const id_render = @import("../id_render.zig");
const cparse = @import("c_parse.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const CaseStyle = id_render.CaseStyle;
const IdRenderer = id_render.IdRenderer;

const preamble =
    \\// This file is generated from the Khronos OpenXR XML API registry by openxr-zig
    \\
    \\const std = @import("std");
    \\const builtin = @import("builtin");
    \\const root = @import("root");
    \\
    \\pub const openxr_call_conv: std.builtin.CallingConvention = if (builtin.os.tag == .windows and builtin.cpu.arch == .x86)
    \\        .Stdcall
    \\    else if (builtin.abi == .android and (builtin.cpu.arch.isARM() or builtin.cpu.arch.isThumb()) and builtin.Target.arm.featureSetHas(builtin.cpu.features, .has_v7) and builtin.cpu.arch.ptrBitWidth() == 32)
    \\        // On Android 32-bit ARM targets, OpenXR functions use the "hardfloat"
    \\        // calling convention, i.e. float parameters are passed in registers. This
    \\        // is true even if the rest of the application passes floats on the stack,
    \\        // as it does by default when compiling for the armeabi-v7a NDK ABI.
    \\        .AAPCSVFP
    \\    else
    \\        .C;
    \\pub fn FlagsMixin(comptime FlagsType: type) type {
    \\    return struct {
    \\        pub const IntType = Flags64;
    \\        pub fn toInt(self: FlagsType) IntType {
    \\            return @bitCast(self);
    \\        }
    \\        pub fn fromInt(flags: IntType) FlagsType {
    \\            return @bitCast(flags);
    \\        }
    \\        pub fn merge(lhs: FlagsType, rhs: FlagsType) FlagsType {
    \\            return fromInt(toInt(lhs) | toInt(rhs));
    \\        }
    \\        pub fn intersect(lhs: FlagsType, rhs: FlagsType) FlagsType {
    \\            return fromInt(toInt(lhs) & toInt(rhs));
    \\        }
    \\        pub fn complement(self: FlagsType) FlagsType {
    \\            return fromInt(~toInt(self));
    \\        }
    \\        pub fn subtract(lhs: FlagsType, rhs: FlagsType) FlagsType {
    \\            return fromInt(toInt(lhs) & toInt(rhs.complement()));
    \\        }
    \\        pub fn contains(lhs: FlagsType, rhs: FlagsType) bool {
    \\            return toInt(intersect(lhs, rhs)) == toInt(rhs);
    \\        }
    \\    };
    \\}
    \\pub fn makeVersion(major: u16, minor: u16, patch: u32) u64 {
    \\    return (@as(u64, major) << 48) | (@as(u64, minor) << 32) | patch;
    \\}
    \\pub fn versionMajor(version: u64) u16 {
    \\    return @truncate(version >> 48);
    \\}
    \\pub fn versionMinor(version: u16) u16 {
    \\    return @truncate(version >> 32);
    \\}
    \\pub fn versionPatch(version: u64) u32 {
    \\    return @truncate(version);
    \\}
    \\
;

const builtin_types = std.StaticStringMap([]const u8).initComptime(.{
    .{ "void", @typeName(void) },
    .{ "char", @typeName(u8) },
    .{ "float", @typeName(f32) },
    .{ "double", @typeName(f64) },
    .{ "uint8_t", @typeName(u8) },
    .{ "uint16_t", @typeName(u16) },
    .{ "uint32_t", @typeName(u32) },
    .{ "uint64_t", @typeName(u64) },
    .{ "int16_t", @typeName(i16) },
    .{ "int32_t", @typeName(i32) },
    .{ "int64_t", @typeName(i64) },
    .{ "size_t", @typeName(usize) },
    .{ "wchar_t", @typeName(u16) },
    .{ "int", @typeName(c_int) },
});

const foreign_types = std.StaticStringMap([]const u8).initComptime(.{
    .{ "Display", "opaque {}" },
    .{ "VisualID", @typeName(c_uint) },
    .{ "Window", @typeName(c_ulong) },
    .{ "xcb_glx_fbconfig_t", "opaque {}" },
    .{ "xcb_glx_drawable_t", "opaque {}" },
    .{ "xcb_glx_context_t", "opaque {}" },
    .{ "xcb_connection_t", "opaque {}" },
    .{ "xcb_visualid_t", @typeName(u32) },
    .{ "xcb_window_t", @typeName(u32) },
    .{ "VkAllocationCallbacks", "@import(\"vulkan\").AllocationCallbacks" },
    .{ "VkDevice", "@import(\"vulkan\").Device" },
    .{ "VkDeviceCreateInfo", "@import(\"vulkan\").DeviceCreateInfo" },
    .{ "VkFormat", "@import(\"vulkan\").Format" },
    .{ "VkImage", "@import(\"vulkan\").Image" },
    .{ "VkInstance", "@import(\"vulkan\").Instance" },
    .{ "VkInstanceCreateInfo", "@import(\"vulkan\").InstanceCreateInfo" },
    .{ "VkPhysicalDevice", "@import(\"vulkan\").PhysicalDevice" },
    .{ "VkResult", "@import(\"vulkan\").Result" },
    .{ "PFN_vkGetInstanceProcAddr", "@import(\"vulkan\").PfnGetInstanceProcAddr" },
});

const initialized_types = std.StaticStringMap([]const u8).initComptime(.{
    .{ "XrVector2f", "0" },
    .{ "XrVector3f", "0" },
    .{ "XrVector4f", "0" },
    .{ "XrColor4f", "0" },
    .{ "XrQuaternionf", "0" },
    .{ "XrPosef", ".{}" },
    .{ "XrOffset2Df", "0" },
    .{ "XrExtent2Df", "0" },
    .{ "XrRect2Df", ".{}" },
    .{ "XrOffset2Di", "0" },
    .{ "XrExtent2Di", "0" },
    .{ "XrRect2Di", ".{}" },
});

fn eqlIgnoreCase(lhs: []const u8, rhs: []const u8) bool {
    if (lhs.len != rhs.len) {
        return false;
    }

    for (lhs, rhs) |l, r| {
        if (std.ascii.toLower(l) != std.ascii.toLower(r)) {
            return false;
        }
    }

    return true;
}

pub fn trimXrNamespace(id: []const u8) []const u8 {
    const prefixes = [_][]const u8{ "XR_", "xr", "Xr", "PFN_xr" };
    for (prefixes) |prefix| {
        if (mem.startsWith(u8, id, prefix)) {
            return id[prefix.len..];
        }
    }

    return id;
}

fn Renderer(comptime WriterType: type) type {
    return struct {
        const Self = @This();
        const WriteError = WriterType.Error;
        const RenderTypeInfoError = WriteError || error{OutOfMemory};

        const BitflagName = struct {
            /// Name without FlagBits, so XrSurfaceTransformFlagBitsKHR
            /// becomes XrSurfaceTransform
            base_name: []const u8,

            /// Optional tag of the flag
            tag: ?[]const u8,
        };

        const ParamType = enum {
            in_pointer,
            out_pointer,
            in_out_pointer,
            bitflags,
            mut_buffer_len,
            buffer_len,
            other,
        };

        const ReturnValue = struct {
            name: []const u8,
            return_value_type: reg.TypeInfo,
            origin: enum {
                parameter,
                inner_return_value,
            },
        };

        const CommandDispatchType = enum {
            base,
            instance,
        };

        writer: WriterType,
        allocator: Allocator,
        registry: *const reg.Registry,
        id_renderer: *IdRenderer,
        declarations_by_name: std.StringHashMap(*const reg.DeclarationType),
        structure_types: std.StringHashMap(void),

        fn init(writer: WriterType, allocator: Allocator, registry: *const reg.Registry, id_renderer: *IdRenderer) !Self {
            var declarations_by_name = std.StringHashMap(*const reg.DeclarationType).init(allocator);
            errdefer declarations_by_name.deinit();

            for (registry.decls) |*decl| {
                const result = try declarations_by_name.getOrPut(decl.name);
                if (result.found_existing) {
                    return error.InvalidRegistry;
                }

                result.value_ptr.* = &decl.decl_type;
            }

            const xr_structure_type_decl = declarations_by_name.get("XrStructureType") orelse return error.InvalidRegistry;
            const xr_structure_type = switch (xr_structure_type_decl.*) {
                .enumeration => |e| e,
                else => return error.InvalidRegistry,
            };
            var structure_types = std.StringHashMap(void).init(allocator);
            errdefer structure_types.deinit();

            for (xr_structure_type.fields) |field| {
                try structure_types.put(field.name, {});
            }

            return Self{
                .writer = writer,
                .allocator = allocator,
                .registry = registry,
                .id_renderer = id_renderer,
                .declarations_by_name = declarations_by_name,
                .structure_types = structure_types,
            };
        }

        fn deinit(self: *Self) void {
            self.declarations_by_name.deinit();
        }

        fn writeIdentifier(self: Self, id: []const u8) !void {
            try id_render.writeIdentifier(self.writer, id);
        }

        fn writeIdentifierWithCase(self: *Self, case: CaseStyle, id: []const u8) !void {
            try self.id_renderer.renderWithCase(self.writer, case, id);
        }

        fn writeIdentifierFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
            try self.id_renderer.renderFmt(self.writer, fmt, args);
        }

        fn extractEnumFieldName(self: Self, enum_name: []const u8, field_name: []const u8) ![]const u8 {
            const adjusted_enum_name = if (mem.eql(u8, enum_name, "XrStructureType"))
                "XrType"
            else
                self.id_renderer.stripAuthorTag(enum_name);
            var enum_it = id_render.SegmentIterator.init(adjusted_enum_name);
            var field_it = id_render.SegmentIterator.init(field_name);

            while (true) {
                const rest = field_it.rest();
                const enum_segment = enum_it.next() orelse return rest;
                const field_segment = field_it.next() orelse return error.InvalidRegistry;

                if (!eqlIgnoreCase(enum_segment, field_segment)) {
                    return rest;
                }
            }
        }

        fn extractBitflagName(self: Self, name: []const u8) ?BitflagName {
            const tag = self.id_renderer.getAuthorTag(name);
            const base_name = if (tag) |tag_name| name[0 .. name.len - tag_name.len] else name;

            if (!mem.endsWith(u8, base_name, "FlagBits")) {
                return null;
            }

            return BitflagName{
                .base_name = base_name[0 .. base_name.len - "FlagBits".len],
                .tag = tag,
            };
        }

        fn isFlags(self: Self, name: []const u8) bool {
            const tag = self.id_renderer.getAuthorTag(name);
            const base_name = if (tag) |tag_name| name[0 .. name.len - tag_name.len] else name;

            return mem.endsWith(u8, base_name, "Flags64");
        }

        fn resolveDeclaration(self: Self, name: []const u8) ?reg.DeclarationType {
            const decl = self.declarations_by_name.get(name) orelse return null;
            return self.resolveAlias(decl.*) catch return null;
        }

        fn resolveAlias(self: Self, start_decl: reg.DeclarationType) !reg.DeclarationType {
            var decl = start_decl;
            while (true) {
                const name = switch (decl) {
                    .alias => |alias| alias.name,
                    else => return decl,
                };

                const decl_ptr = self.declarations_by_name.get(name) orelse return error.InvalidRegistry;
                decl = decl_ptr.*;
            }
        }

        fn isInOutPointer(self: Self, ptr: reg.Pointer) !bool {
            if (ptr.child.* != .name) {
                return false;
            }

            const decl = self.resolveDeclaration(ptr.child.name) orelse return error.InvalidRegistry;
            if (decl != .container) {
                return false;
            }

            const container = decl.container;
            if (container.is_union) {
                return false;
            }

            for (container.fields) |field| {
                if (mem.eql(u8, field.name, "next")) {
                    return true;
                }
            }

            return false;
        }

        fn classifyParam(self: Self, param: reg.Command.Param) !ParamType {
            switch (param.param_type) {
                .pointer => |ptr| {
                    if (param.is_buffer_len) {
                        if (ptr.is_const or ptr.is_optional) {
                            return error.InvalidRegistry;
                        }

                        return .mut_buffer_len;
                    }

                    if (ptr.child.* == .name) {
                        const child_name = ptr.child.name;
                        if (mem.eql(u8, child_name, "void")) {
                            return .other;
                        } else if (builtin_types.get(child_name) == null and trimXrNamespace(child_name).ptr == child_name.ptr) {
                            return .other; // External type
                        }
                    }

                    if (ptr.size == .one and !ptr.is_optional) {
                        // Sometimes, a mutable pointer to a struct is taken, even though
                        // OpenXR expects this struct to be initialized. This is particularly the case
                        // for getting structs which include next chains.
                        if (ptr.is_const) {
                            return .in_pointer;
                        } else if (try self.isInOutPointer(ptr)) {
                            return .in_out_pointer;
                        } else {
                            return .out_pointer;
                        }
                    }
                },
                .name => |name| {
                    if (self.extractBitflagName(name) != null or self.isFlags(name)) {
                        return .bitflags;
                    }
                },
                else => {},
            }

            if (param.is_buffer_len) {
                return .buffer_len;
            }

            return .other;
        }

        fn classifyCommandDispatch(name: []const u8, command: reg.Command) CommandDispatchType {
            const override_functions = std.StaticStringMap(CommandDispatchType).initComptime(.{
                .{ "xrGetInstanceProcAddr", .base },
                .{ "xrCreateInstance", .base },
                .{ "xrEnumerateApiLayerProperties", .base },
                .{ "xrEnumerateInstanceExtensionProperties", .base },
            });

            if (override_functions.get(name)) |dispatch_type| {
                return dispatch_type;
            }

            return switch (command.params[0].param_type) {
                .name => .instance,
                else => return .base,
            };
        }

        fn render(self: *Self) !void {
            try self.writer.writeAll(preamble);

            for (self.registry.api_constants) |api_constant| {
                try self.renderApiConstant(api_constant);
            }

            for (self.registry.decls) |decl| {
                try self.renderDecl(decl);
            }

            try self.renderCommandPtrs();
            try self.renderExtensionInfo();
            try self.renderWrappers();
        }

        fn renderApiConstant(self: *Self, api_constant: reg.ApiConstant) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(api_constant.name);
            try self.writer.writeAll(" = ");

            switch (api_constant.value) {
                .expr => |expr| try self.renderApiConstantExpr(expr),
                .version => |version| {
                    try self.writer.writeAll("makeVersion(");
                    for (version, 0..) |part, i| {
                        if (i != 0) {
                            try self.writer.writeAll(", ");
                        }
                        try self.renderApiConstantExpr(part);
                    }
                    try self.writer.writeAll(")");
                },
            }

            try self.writer.writeAll(";\n");
        }

        fn renderApiConstantExpr(self: *Self, expr: []const u8) !void {
            const adjusted_expr = if (expr.len > 2 and expr[0] == '(' and expr[expr.len - 1] == ')')
                expr[1 .. expr.len - 1]
            else
                expr;

            var tokenizer = cparse.CTokenizer{ .source = adjusted_expr };
            var peeked: ?cparse.Token = null;
            while (true) {
                const tok = peeked orelse (try tokenizer.next()) orelse break;
                peeked = null;

                switch (tok.kind) {
                    .lparen, .rparen, .tilde, .minus => {
                        try self.writer.writeAll(tok.text);
                        continue;
                    },
                    .id => {
                        try self.renderName(tok.text);
                        continue;
                    },
                    .int => {},
                    else => return error.InvalidApiConstant,
                }

                const suffix = (try tokenizer.next()) orelse {
                    try self.writer.writeAll(tok.text);
                    break;
                };

                switch (suffix.kind) {
                    .id => {
                        if (mem.eql(u8, suffix.text, "ULL")) {
                            try self.writer.print("@as(u64, {s})", .{tok.text});
                        } else if (mem.eql(u8, suffix.text, "U")) {
                            try self.writer.print("@as(u32, {s})", .{tok.text});
                        } else {
                            return error.InvalidApiConstant;
                        }
                    },
                    .dot => {
                        const decimal = (try tokenizer.next()) orelse return error.InvalidConstantExpr;
                        try self.writer.print("@as(f32, {s}.{s})", .{ tok.text, decimal.text });

                        const f = (try tokenizer.next()) orelse return error.InvalidConstantExpr;
                        if (f.kind != .id or !mem.eql(u8, f.text, "f")) {
                            return error.InvalidApiConstant;
                        }
                    },
                    else => {
                        try self.writer.writeAll(tok.text);
                        peeked = suffix;
                    },
                }
            }
        }

        fn renderTypeInfo(self: *Self, type_info: reg.TypeInfo) RenderTypeInfoError!void {
            switch (type_info) {
                .name => |name| try self.renderName(name),
                .command_ptr => |command_ptr| try self.renderCommandPtr(command_ptr, true),
                .pointer => |pointer| try self.renderPointer(pointer),
                .array => |array| try self.renderArray(array),
            }
        }

        fn renderName(self: *Self, name: []const u8) !void {
            if (builtin_types.get(name)) |zig_name| {
                try self.writer.writeAll(zig_name);
                return;
            } else if (self.extractBitflagName(name)) |bitflag_name| {
                try self.writeIdentifierFmt("{s}Flags{s}", .{
                    trimXrNamespace(bitflag_name.base_name),
                    @as([]const u8, if (bitflag_name.tag) |tag| tag else ""),
                });
                return;
            } else if (mem.startsWith(u8, name, "xr")) {
                // Function type, always render with the exact same text for linking purposes.
                try self.writeIdentifier(name);
                return;
            } else if (mem.startsWith(u8, name, "Xr")) {
                // Type, strip namespace and write, as they are alreay in title case.
                try self.writeIdentifier(name[2..]);
                return;
            } else if (mem.startsWith(u8, name, "PFN_xr")) {
                // Function pointer type, strip off the PFN_xr part and replace it with Pfn. Note that
                // this function is only called to render the typedeffed function pointers like xrVoidFunction
                try self.writeIdentifierFmt("Pfn{s}", .{name[6..]});
                return;
            } else if (mem.startsWith(u8, name, "XR_")) {
                // Constants
                try self.writeIdentifier(name[3..]);
                return;
            }

            try self.writeIdentifier(name);
        }

        fn renderCommandPtr(self: *Self, command_ptr: reg.Command, optional: bool) !void {
            if (optional) {
                try self.writer.writeByte('?');
            }
            try self.writer.writeAll("*const fn(");
            for (command_ptr.params) |param| {
                try self.writeIdentifierWithCase(.snake, param.name);
                try self.writer.writeAll(": ");

                blk: {
                    if (param.param_type == .name) {
                        if (self.extractBitflagName(param.param_type.name)) |bitflag_name| {
                            try self.writeIdentifierFmt("{s}Flags{s}", .{
                                trimXrNamespace(bitflag_name.base_name),
                                @as([]const u8, if (bitflag_name.tag) |tag| tag else ""),
                            });
                            break :blk;
                        } else if (self.isFlags(param.param_type.name)) {
                            try self.renderTypeInfo(param.param_type);
                            break :blk;
                        }
                    }

                    try self.renderTypeInfo(param.param_type);
                }

                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll(") callconv(openxr_call_conv)");
            try self.renderTypeInfo(command_ptr.return_type.*);
        }

        fn renderPointer(self: *Self, pointer: reg.Pointer) !void {
            const child_is_void = pointer.child.* == .name and mem.eql(u8, pointer.child.name, "void");

            if (pointer.is_optional) {
                try self.writer.writeByte('?');
            }

            const size = if (child_is_void) .one else pointer.size;
            switch (size) {
                .one => try self.writer.writeByte('*'),
                .many, .other_field => try self.writer.writeAll("[*]"),
                .zero_terminated => try self.writer.writeAll("[*:0]"),
            }

            if (pointer.is_const) {
                try self.writer.writeAll("const ");
            }

            if (child_is_void) {
                try self.writer.writeAll("anyopaque");
            } else {
                try self.renderTypeInfo(pointer.child.*);
            }
        }

        fn renderArray(self: *Self, array: reg.Array) !void {
            try self.writer.writeByte('[');
            switch (array.size) {
                .int => |size| try self.writer.print("{}", .{size}),
                .alias => |alias| try self.renderName(alias),
            }
            try self.writer.writeByte(']');
            try self.renderTypeInfo(array.child.*);
        }

        fn renderDecl(self: *Self, decl: reg.Declaration) !void {
            switch (decl.decl_type) {
                .container => |container| try self.renderContainer(decl.name, container),
                .enumeration => |enumeration| try self.renderEnumeration(decl.name, enumeration),
                .handle => |handle| try self.renderHandle(decl.name, handle),
                .alias => |alias| try self.renderAlias(decl.name, alias),
                .foreign => |foreign| try self.renderForeign(decl.name, foreign),
                .typedef => |type_info| try self.renderTypedef(decl.name, type_info),
                .external => try self.renderExternal(decl.name),
                .command, .bitmask => {},
            }
        }

        fn renderContainer(self: *Self, name: []const u8, container: reg.Container) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = ");

            for (container.fields) |field| {
                if (field.bits != null) {
                    return error.UnhandledBitfieldStruct;
                }
            } else {
                try self.writer.writeAll("extern ");
            }

            if (container.is_union) {
                try self.writer.writeAll("union {");
            } else {
                try self.writer.writeAll("struct {");
            }

            for (container.fields) |field| {
                try self.writeIdentifierWithCase(.snake, field.name);
                try self.writer.writeAll(": ");
                if (field.bits) |bits| {
                    try self.writer.print(" u{},", .{bits});
                    if (field.field_type != .name or builtin_types.get(field.field_type.name) == null) {
                        try self.writer.writeAll("// ");
                        try self.renderTypeInfo(field.field_type);
                        try self.writer.writeByte('\n');
                    }
                } else {
                    try self.renderTypeInfo(field.field_type);
                    if (!container.is_union) {
                        try self.renderContainerDefaultField(container, name, field);
                    }
                    try self.writer.writeAll(", ");
                }
            }

            if (!container.is_union) {
                const have_next_or_type = for (container.fields) |field| {
                    if (mem.eql(u8, field.name, "next") or mem.eql(u8, field.name, "type")) {
                        break true;
                    }
                } else false;

                if (have_next_or_type) {
                    try self.writer.writeAll(
                        \\    pub fn empty() @This() {
                        \\        var value: @This() = undefined;
                    );

                    for (container.fields) |field| {
                        if (mem.eql(u8, field.name, "next") or mem.eql(u8, field.name, "type")) {
                            try self.writer.writeAll("value.");
                            try self.writeIdentifierWithCase(.snake, field.name);
                            try self.renderContainerDefaultField(container, name, field);
                            try self.writer.writeAll(";\n");
                        }
                    }

                    try self.writer.writeAll(
                        \\        return value;
                        \\    }
                    );
                }
            }

            try self.writer.writeAll("};\n");
        }

        fn renderContainerDefaultField(self: *Self, container: reg.Container, container_name: []const u8, field: reg.Container.Field) !void {
            if (mem.eql(u8, field.name, "next")) {
                try self.writer.writeAll(" = null");
            } else if (mem.eql(u8, field.name, "type")) {
                if (container.stype == null) {
                    return;
                }

                const stype = container.stype.?;
                if (!mem.startsWith(u8, stype, "XR_TYPE_")) {
                    return error.InvalidRegistry;
                }

                try self.writer.writeAll(" = .");
                try self.writeIdentifierWithCase(.snake, stype["XR_TYPE_".len..]);
            } else if (mem.eql(u8, field.name, "w") and mem.eql(u8, container_name, "XrQuaternionf")) {
                try self.writer.writeAll(" = 1");
            } else if (field.is_optional) {
                if (field.field_type == .name) {
                    const field_type_name = field.field_type.name;
                    if (self.resolveDeclaration(field_type_name)) |decl_type| {
                        if (decl_type == .handle) {
                            try self.writer.writeAll(" = .null_handle");
                        } else if (decl_type == .bitmask) {
                            try self.writer.writeAll(" = .{}");
                        } else if (decl_type == .typedef and decl_type.typedef == .command_ptr) {
                            try self.writer.writeAll(" = null");
                        } else if ((decl_type == .typedef and builtin_types.has(decl_type.typedef.name)) or
                            (decl_type == .foreign and builtin_types.has(field_type_name)))
                        {
                            try self.writer.writeAll(" = 0");
                        }
                    }
                } else if (field.field_type == .pointer) {
                    try self.writer.writeAll(" = null");
                }
            } else if (field.field_type == .pointer and field.field_type.pointer.is_optional) {
                // pointer nullability could be here or above
                try self.writer.writeAll(" = null");
            } else if (initialized_types.get(container_name)) |value| {
                try self.writer.writeAll(" = ");
                try self.writer.writeAll(value);
            }
        }

        fn renderEnumFieldName(self: *Self, name: []const u8, field_name: []const u8) !void {
            try self.writeIdentifierWithCase(.snake, try self.extractEnumFieldName(name, field_name));
        }

        fn renderEnumeration(self: *Self, name: []const u8, enumeration: reg.Enum) !void {
            if (enumeration.is_bitmask) {
                try self.renderBitmaskBits(name, enumeration);
                return;
            }

            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = enum(i32) {");

            for (enumeration.fields) |field| {
                if (field.value == .alias)
                    continue;

                try self.renderEnumFieldName(name, field.name);
                switch (field.value) {
                    .int => |int| try self.writer.print(" = {}, ", .{int}),
                    .bitpos => |pos| try self.writer.print(" = 1 << {}, ", .{pos}),
                    .bit_vector => |bv| try self.writer.print("= 0x{X}, ", .{bv}),
                    .alias => unreachable,
                }
            }

            try self.writer.writeAll("_,");

            for (enumeration.fields) |field| {
                if (field.value != .alias or field.value.alias.is_compat_alias)
                    continue;

                try self.writer.writeAll("pub const ");
                try self.renderEnumFieldName(name, field.name);
                try self.writer.writeAll(" = ");
                try self.renderName(name);
                try self.writer.writeByte('.');
                try self.renderEnumFieldName(name, field.value.alias.name);
                try self.writer.writeAll(";\n");
            }

            try self.writer.writeAll("};\n");
        }

        fn renderBitmaskBits(self: *Self, name: []const u8, bits: reg.Enum) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = packed struct {");

            if (bits.fields.len == 0) {
                try self.writer.writeAll("_reserved_bits: Flags64 = 0,");
            } else {
                var flags_by_bitpos = [_]?[]const u8{null} ** 64;
                for (bits.fields) |field| {
                    if (field.value == .bitpos) {
                        flags_by_bitpos[field.value.bitpos] = field.name;
                    }
                }

                for (flags_by_bitpos, 0..) |opt_flag_name, bitpos| {
                    if (opt_flag_name) |flag_name| {
                        try self.renderEnumFieldName(name, flag_name);
                    } else {
                        try self.writer.print("_reserved_bit_{}", .{bitpos});
                    }

                    try self.writer.writeAll(":bool = false, ");
                }
            }
            try self.writer.writeAll("pub usingnamespace FlagsMixin(");
            try self.renderName(name);
            try self.writer.writeAll(");\n};\n");
        }

        fn renderHandle(self: *Self, name: []const u8, handle: reg.Handle) !void {
            const backing_type: []const u8 = if (handle.is_dispatchable) "usize" else "u64";

            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.print(" = enum({s}) {{null_handle = 0, _}};\n", .{backing_type});
        }

        fn renderAlias(self: *Self, name: []const u8, alias: reg.Alias) !void {
            if (alias.target == .other_command) {
                return;
            } else if (self.extractBitflagName(name) != null) {
                // Don't make aliases of the bitflag names, as those are replaced by just the flags type
                return;
            }

            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = ");
            try self.renderName(alias.name);
            try self.writer.writeAll(";\n");
        }

        fn renderExternal(self: *Self, name: []const u8) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = opaque {};\n");
        }

        fn renderForeign(self: *Self, name: []const u8, foreign: reg.Foreign) !void {
            if (mem.eql(u8, foreign.depends, "openxr_platform_defines")) {
                return; // Skip built-in types, they are handled differently
            }

            try self.writer.writeAll("pub const ");
            try self.writeIdentifier(name);
            try self.writer.print(" = if (@hasDecl(root, \"{s}\")) root.", .{name});
            try self.writeIdentifier(name);
            try self.writer.writeAll(" else ");

            if (foreign_types.get(name)) |default| {
                try self.writer.writeAll(default);
                try self.writer.writeAll(";\n");
            } else {
                try self.writer.print("@compileError(\"Missing type definition of '{s}'\");\n", .{name});
            }
        }

        fn renderTypedef(self: *Self, name: []const u8, type_info: reg.TypeInfo) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = ");
            try self.renderTypeInfo(type_info);
            try self.writer.writeAll(";\n");
        }

        fn renderCommandPtrName(self: *Self, name: []const u8) !void {
            try self.writeIdentifierFmt("Pfn{s}", .{trimXrNamespace(name)});
        }

        fn renderCommandPtrs(self: *Self) !void {
            for (self.registry.decls) |decl| {
                switch (decl.decl_type) {
                    .command => {
                        try self.writer.writeAll("pub const ");
                        try self.renderCommandPtrName(decl.name);
                        try self.writer.writeAll(" = ");
                        try self.renderCommandPtr(decl.decl_type.command, false);
                        try self.writer.writeAll(";\n");
                    },
                    .alias => |alias| if (alias.target == .other_command) {
                        try self.writer.writeAll("pub const ");
                        try self.renderCommandPtrName(decl.name);
                        try self.writer.writeAll(" = ");
                        try self.renderCommandPtrName(alias.name);
                        try self.writer.writeAll(";\n");
                    },
                    else => {},
                }
            }
        }

        fn renderExtensionInfo(self: *Self) !void {
            try self.writer.writeAll(
                \\pub const extension_info = struct {
                \\    const Info = struct {
                \\        name: [:0]const u8,
                \\        version: u32,
                \\    };
            );
            for (self.registry.extensions) |ext| {
                try self.writer.writeAll("pub const ");
                try self.writeIdentifierWithCase(.snake, trimXrNamespace(ext.name));
                try self.writer.writeAll("= Info {\n");
                try self.writer.print(".name = \"{s}\", .version = {},", .{ ext.name, ext.version });
                try self.writer.writeAll("};\n");
            }
            try self.writer.writeAll("};\n");
        }

        fn renderWrappers(self: *Self) !void {
            try self.writer.writeAll(
                \\pub fn CommandFlagsMixin(comptime CommandFlags: type) type {
                \\    return struct {
                \\        pub fn merge(lhs: CommandFlags, rhs: CommandFlags) CommandFlags {
                \\            var result: CommandFlags = .{};
                \\            inline for (@typeInfo(CommandFlags).@"struct".fields) |field| {
                \\                @field(result, field.name) = @field(lhs, field.name) or @field(rhs, field.name);
                \\            }
                \\            return result;
                \\        }
                \\        pub fn intersect(lhs: CommandFlags, rhs: CommandFlags) CommandFlags {
                \\            var result: CommandFlags = .{};
                \\            inline for (@typeInfo(CommandFlags).@"struct".fields) |field| {
                \\                @field(result, field.name) = @field(lhs, field.name) and @field(rhs, field.name);
                \\            }
                \\            return result;
                \\        }
                \\        pub fn complement(self: CommandFlags) CommandFlags {
                \\            var result: CommandFlags = .{};
                \\            inline for (@typeInfo(CommandFlags).@"struct".fields) |field| {
                \\                @field(result, field.name) = !@field(self, field.name);
                \\            }
                \\            return result;
                \\        }
                \\        pub fn subtract(lhs: CommandFlags, rhs: CommandFlags) CommandFlags {
                \\            var result: CommandFlags = .{};
                \\            inline for (@typeInfo(CommandFlags).@"struct".fields) |field| {
                \\                @field(result, field.name) = @field(lhs, field.name) and !@field(rhs, field.name);
                \\            }
                \\            return result;
                \\        }
                \\        pub fn contains(lhs: CommandFlags, rhs: CommandFlags) bool {
                \\            inline for (@typeInfo(CommandFlags).@"struct".fields) |field| {
                \\                if (!@field(lhs, field.name) and @field(rhs, field.name)) {
                \\                    return false;
                \\                }
                \\            }
                \\            return true;
                \\        }
                \\    };
                \\}
                \\
            );
            try self.renderWrappersOfDispatchType(.base);
            try self.renderWrappersOfDispatchType(.instance);
        }

        fn renderWrappersOfDispatchType(self: *Self, dispatch_type: CommandDispatchType) !void {
            const name = switch (dispatch_type) {
                .base => "Base",
                .instance => "Instance",
            };

            try self.writer.print(
                \\pub const {0s}CommandFlags = packed struct {{
                \\
            , .{name});
            for (self.registry.decls) |decl| {
                // If the target type does not exist, it was likely an empty enum -
                // assume spec is correct and that this was not a function alias.
                const decl_type = self.resolveAlias(decl.decl_type) catch continue;
                const command = switch (decl_type) {
                    .command => |cmd| cmd,
                    else => continue,
                };

                if (classifyCommandDispatch(decl.name, command) == dispatch_type) {
                    try self.writer.writeAll("    ");
                    try self.writeIdentifierWithCase(.camel, trimXrNamespace(decl.name));
                    try self.writer.writeAll(": bool = false,\n");
                }
            }

            try self.writer.print(
                \\pub fn CmdType(comptime tag: std.meta.FieldEnum({0s}CommandFlags)) type {{
                \\    return switch (tag) {{
                \\
            , .{name});
            for (self.registry.decls) |decl| {
                // If the target type does not exist, it was likely an empty enum -
                // assume spec is correct and that this was not a function alias.
                const decl_type = self.resolveAlias(decl.decl_type) catch continue;
                const command = switch (decl_type) {
                    .command => |cmd| cmd,
                    else => continue,
                };

                if (classifyCommandDispatch(decl.name, command) == dispatch_type) {
                    try self.writer.writeAll((" " ** 8) ++ ".");
                    try self.writeIdentifierWithCase(.camel, trimXrNamespace(decl.name));
                    try self.writer.writeAll(" => ");
                    try self.renderCommandPtrName(decl.name);
                    try self.writer.writeAll(",\n");
                }
            }
            try self.writer.writeAll("    };\n}");

            try self.writer.print(
                \\pub fn cmdName(tag: std.meta.FieldEnum({0s}CommandFlags)) [:0]const u8 {{
                \\    return switch(tag) {{
                \\
            , .{name});
            for (self.registry.decls) |decl| {
                // If the target type does not exist, it was likely an empty enum -
                // assume spec is correct and that this was not a function alias.
                const decl_type = self.resolveAlias(decl.decl_type) catch continue;
                const command = switch (decl_type) {
                    .command => |cmd| cmd,
                    else => continue,
                };

                if (classifyCommandDispatch(decl.name, command) == dispatch_type) {
                    try self.writer.writeAll((" " ** 8) ++ ".");
                    try self.writeIdentifierWithCase(.camel, trimXrNamespace(decl.name));
                    try self.writer.print(
                        \\ => "{s}",
                        \\
                    , .{decl.name});
                }
            }
            try self.writer.writeAll("    };\n}");

            try self.writer.print(
                \\    pub usingnamespace CommandFlagsMixin({s}CommandFlags);
                \\}};
                \\
            , .{name});

            try self.writer.print(
                \\pub fn {0s}Wrapper(comptime cmds: {0s}CommandFlags) type {{
                \\    return struct {{
                \\        dispatch: Dispatch,
                \\
                \\        const Self = @This();
                \\        pub const commands = cmds;
                \\        pub const Dispatch = blk: {{
                \\            @setEvalBranchQuota(10_000);
                \\            const Type = std.builtin.Type;
                \\            const fields_len = fields_len: {{
                \\                var fields_len = 0;
                \\                for (@typeInfo({0s}CommandFlags).@"struct".fields) |field| {{
                \\                    fields_len += @intFromBool(@field(cmds, field.name));
                \\                }}
                \\                break :fields_len fields_len;
                \\            }};
                \\            var fields: [fields_len]Type.StructField = undefined;
                \\            var i: usize = 0;
                \\            for (@typeInfo({0s}CommandFlags).@"struct".fields) |field| {{
                \\                if (@field(cmds, field.name)) {{
                \\                    const field_tag = std.enums.nameCast(std.meta.FieldEnum({0s}CommandFlags), field.name);
                \\                    const PfnType = {0s}CommandFlags.CmdType(field_tag);
                \\                    fields[i] = .{{
                \\                        .name = {0s}CommandFlags.cmdName(field_tag),
                \\                        .type = PfnType,
                \\                        .default_value_ptr = null,
                \\                        .is_comptime = false,
                \\                        .alignment = @alignOf(PfnType),
                \\                    }};
                \\                    i += 1;
                \\                }}
                \\            }}
                \\            break :blk @Type(.{{
                \\                .@"struct" = .{{
                \\                    .layout = .auto,
                \\                    .fields = &fields,
                \\                    .decls = &[_]std.builtin.Type.Declaration{{}},
                \\                    .is_tuple = false,
                \\                }},
                \\            }});
                \\        }};
                \\
            , .{name});

            try self.renderWrapperLoader(dispatch_type);

            for (self.registry.decls) |decl| {
                // If the target type does not exist, it was likely an empty enum -
                // assume spec is correct and that this was not a function alias.
                const decl_type = self.resolveAlias(decl.decl_type) catch continue;
                const command = switch (decl_type) {
                    .command => |cmd| cmd,
                    else => continue,
                };

                if (classifyCommandDispatch(decl.name, command) != dispatch_type) {
                    continue;
                }
                // Note: If this decl is an alias, generate a full wrapper instead of simply an
                // alias like `const old = new;`. This ensures that OpenXR bindings generated
                // for newer versions of openxr can still invoke extension behavior on older
                // implementations.
                try self.renderWrapper(decl.name, command);
            }

            try self.writer.writeAll("};}\n");
        }

        fn renderWrapperLoader(self: *Self, dispatch_type: CommandDispatchType) !void {
            const params = switch (dispatch_type) {
                .base => "loader: anytype",
                .instance => "instance: Instance, loader: anytype",
            };

            const loader_first_arg = switch (dispatch_type) {
                .base => "Instance.null_handle",
                .instance => "instance",
            };

            @setEvalBranchQuota(2000);

            try self.writer.print(
                \\pub fn load({[params]s}) error{{CommandLoadFailure}}!Self {{
                \\    var self: Self = undefined;
                \\    inline for (std.meta.fields(Dispatch)) |field| {{
                \\        const name: [*:0]const u8 = @ptrCast(field.name ++ "\x00");
                \\        var cmd_ptr: PfnVoidFunction = undefined;
                \\        const result: Result = loader({[first_arg]s}, name, &cmd_ptr);
                \\        if (result != .success) return error.CommandLoadFailure;
                \\        @field(self.dispatch, field.name) = @ptrCast(cmd_ptr);
                \\    }}
                \\    return self;
                \\}}
                \\pub fn loadNoFail({[params]s}) Self {{
                \\    var self: Self = undefined;
                \\    inline for (std.meta.fields(Dispatch)) |field| {{
                \\        const name: [*:0]const u8 = @ptrCast(field.name ++ "\x00");
                \\        var cmd_ptr: PfnVoidFunction = undefined;
                \\        if (loader({[first_arg]s}, name, &cmd_ptr)) {{
                \\          @field(self.dispatch, field.name) = @ptrCast(cmd_ptr);
                \\        }}
                \\    }}
                \\    return self;
                \\}}
            , .{ .params = params, .first_arg = loader_first_arg });
        }

        fn derefName(name: []const u8) []const u8 {
            var it = id_render.SegmentIterator.init(name);
            return if (mem.eql(u8, it.next().?, "p"))
                name[1..]
            else
                name;
        }

        fn renderWrapperPrototype(self: *Self, name: []const u8, command: reg.Command, returns: []const ReturnValue) !void {
            try self.writer.writeAll("pub fn ");
            try self.writeIdentifierWithCase(.camel, trimXrNamespace(name));
            try self.writer.writeAll("(self: Self, ");

            for (command.params) |param| {
                // This parameter is returned instead.
                if ((try self.classifyParam(param)) == .out_pointer) {
                    continue;
                }

                try self.writeIdentifierWithCase(.snake, param.name);
                try self.writer.writeAll(": ");
                try self.renderTypeInfo(param.param_type);
                try self.writer.writeAll(", ");
            }

            try self.writer.writeAll(") ");

            const returns_xr_result = command.return_type.* == .name and mem.eql(u8, command.return_type.name, "XrResult");
            if (returns_xr_result) {
                try self.renderErrorSetName(name);
                try self.writer.writeByte('!');
            }

            if (returns.len == 1) {
                try self.renderTypeInfo(returns[0].return_value_type);
            } else if (returns.len > 1) {
                try self.renderReturnStructName(name);
            } else {
                try self.writer.writeAll("void");
            }
        }

        fn renderWrapperCall(self: *Self, name: []const u8, command: reg.Command, returns: []const ReturnValue) !void {
            try self.writer.writeAll("self.dispatch.");
            try self.writeIdentifier(name);
            try self.writer.writeAll("(");

            for (command.params) |param| {
                switch (try self.classifyParam(param)) {
                    .out_pointer => {
                        try self.writer.writeByte('&');
                        if (returns.len > 1) {
                            try self.writer.writeAll("return_values.");
                        }
                        try self.writeIdentifierWithCase(.snake, derefName(param.name));
                    },
                    .bitflags, .in_pointer, .in_out_pointer, .buffer_len, .mut_buffer_len, .other => {
                        try self.writeIdentifierWithCase(.snake, param.name);
                    },
                }

                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll(")");
        }

        fn extractReturns(self: *Self, command: reg.Command) ![]const ReturnValue {
            var returns = std.ArrayList(ReturnValue).init(self.allocator);

            if (command.return_type.* == .name) {
                const return_name = command.return_type.name;
                if (!mem.eql(u8, return_name, "void") and !mem.eql(u8, return_name, "XrResult")) {
                    try returns.append(.{
                        .name = "return_value",
                        .return_value_type = command.return_type.*,
                        .origin = .inner_return_value,
                    });
                }
            }

            if (command.success_codes.len > 1) {
                if (command.return_type.* != .name or !mem.eql(u8, command.return_type.name, "XrResult")) {
                    return error.InvalidRegistry;
                }

                try returns.append(.{
                    .name = "result",
                    .return_value_type = command.return_type.*,
                    .origin = .inner_return_value,
                });
            } else if (command.success_codes.len == 1 and !mem.eql(u8, command.success_codes[0], "XR_SUCCESS")) {
                return error.InvalidRegistry;
            }

            for (command.params) |param| {
                if ((try self.classifyParam(param)) == .out_pointer) {
                    try returns.append(.{
                        .name = derefName(param.name),
                        .return_value_type = param.param_type.pointer.child.*,
                        .origin = .parameter,
                    });
                }
            }

            return try returns.toOwnedSlice();
        }

        fn renderReturnStructName(self: *Self, command_name: []const u8) !void {
            try self.writeIdentifierFmt("{s}Result", .{trimXrNamespace(command_name)});
        }

        fn renderErrorSetName(self: *Self, name: []const u8) !void {
            try self.writeIdentifierWithCase(.title, trimXrNamespace(name));
            try self.writer.writeAll("Error");
        }

        fn renderReturnStruct(self: *Self, command_name: []const u8, returns: []const ReturnValue) !void {
            try self.writer.writeAll("pub const ");
            try self.renderReturnStructName(command_name);
            try self.writer.writeAll(" = struct {\n");
            for (returns) |ret| {
                try self.writeIdentifierWithCase(.snake, ret.name);
                try self.writer.writeAll(": ");
                try self.renderTypeInfo(ret.return_value_type);
                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll("};\n");
        }

        fn renderWrapper(self: *Self, name: []const u8, command: reg.Command) !void {
            const returns_xr_result = command.return_type.* == .name and mem.eql(u8, command.return_type.name, "XrResult");
            const returns_void = command.return_type.* == .name and mem.eql(u8, command.return_type.name, "void");

            const returns = try self.extractReturns(command);

            if (returns.len > 1) {
                try self.renderReturnStruct(name, returns);
            }

            if (returns_xr_result) {
                try self.writer.writeAll("pub const ");
                try self.renderErrorSetName(name);
                try self.writer.writeAll(" = ");
                try self.renderErrorSet(command.error_codes);
                try self.writer.writeAll(";\n");
            }

            try self.renderWrapperPrototype(name, command, returns);

            if (returns.len == 1 and returns[0].origin == .inner_return_value) {
                try self.writer.writeAll("{\n\n");

                if (returns_xr_result) {
                    try self.writer.writeAll("const result = ");
                    try self.renderWrapperCall(name, command, returns);
                    try self.writer.writeAll(";\n");

                    try self.renderErrorSwitch("result", command);
                    try self.writer.writeAll("return result;\n");
                } else {
                    try self.writer.writeAll("return ");
                    try self.renderWrapperCall(name, command, returns);
                    try self.writer.writeAll(";\n");
                }

                try self.writer.writeAll("\n}\n");
                return;
            }

            try self.writer.writeAll("{\n");
            if (returns.len == 1) {
                try self.writer.writeAll("var ");
                try self.writeIdentifierWithCase(.snake, returns[0].name);
                try self.writer.writeAll(": ");
                try self.renderTypeInfo(returns[0].return_value_type);
                try self.writer.writeAll(" = undefined;\n");
            } else if (returns.len > 1) {
                try self.writer.writeAll("var return_values: ");
                try self.renderReturnStructName(name);
                try self.writer.writeAll(" = undefined;\n");
            }

            if (returns_xr_result) {
                try self.writer.writeAll("const result = ");
                try self.renderWrapperCall(name, command, returns);
                try self.writer.writeAll(";\n");

                try self.renderErrorSwitch("result", command);
                if (command.success_codes.len > 1) {
                    try self.writer.writeAll("return_values.result = result;\n");
                }
            } else {
                if (!returns_void) {
                    try self.writer.writeAll("return_values.return_value = ");
                }
                try self.renderWrapperCall(name, command, returns);
                try self.writer.writeAll(";\n");
            }

            if (returns.len == 1) {
                try self.writer.writeAll("return ");
                try self.writeIdentifierWithCase(.snake, returns[0].name);
                try self.writer.writeAll(";\n");
            } else if (returns.len > 1) {
                try self.writer.writeAll("return return_values;\n");
            }

            try self.writer.writeAll("}\n");
        }

        fn renderErrorSwitch(self: *Self, result_var: []const u8, command: reg.Command) !void {
            try self.writer.writeAll("switch (");
            try self.writeIdentifier(result_var);
            try self.writer.writeAll(") {\n");

            for (command.success_codes) |success| {
                try self.writer.writeAll("Result.");
                try self.renderEnumFieldName("XrResult", success);
                try self.writer.writeAll(" => {},");
            }

            for (command.error_codes) |err| {
                try self.writer.writeAll("Result.");
                try self.renderEnumFieldName("XrResult", err);
                try self.writer.writeAll(" => return error.");
                try self.renderResultAsErrorName(err);
                try self.writer.writeAll(", ");
            }

            try self.writer.writeAll("else => return error.Unknown,}\n");
        }

        fn renderErrorSet(self: *Self, errors: []const []const u8) !void {
            try self.writer.writeAll("error{");
            for (errors) |name| {
                try self.renderResultAsErrorName(name);
                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll("Unknown, }");
        }

        fn renderResultAsErrorName(self: *Self, name: []const u8) !void {
            const error_prefix = "XR_ERROR_";
            if (mem.startsWith(u8, name, error_prefix)) {
                try self.writeIdentifierWithCase(.title, name[error_prefix.len..]);
            } else {
                // Apparently some commands (XrAcquireProfilingLockInfoKHR) return
                // success codes as error...
                try self.writeIdentifierWithCase(.title, trimXrNamespace(name));
            }
        }
    };
}

pub fn render(writer: anytype, allocator: Allocator, registry: *const reg.Registry, id_renderer: *IdRenderer) !void {
    var renderer = try Renderer(@TypeOf(writer)).init(writer, allocator, registry, id_renderer);
    defer renderer.deinit();
    try renderer.render();
}
