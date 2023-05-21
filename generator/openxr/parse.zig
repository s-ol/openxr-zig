const std = @import("std");
const registry = @import("registry.zig");
const xml = @import("../xml.zig");
const cparse = @import("c_parse.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const api_constants_name = "API Constants";

pub const ParseResult = struct {
    arena: ArenaAllocator,
    registry: registry.Registry,

    pub fn deinit(self: ParseResult) void {
        self.arena.deinit();
    }
};

pub fn parseXml(backing_allocator: Allocator, root: *xml.Element) !ParseResult {
    var arena = ArenaAllocator.init(backing_allocator);
    errdefer arena.deinit();

    const allocator = arena.allocator();

    var reg = registry.Registry{
        .decls = try parseDeclarations(allocator, root),
        .api_constants = try parseApiConstants(allocator, root),
        .tags = try parseTags(allocator, root),
        .features = try parseFeatures(allocator, root),
        .extensions = try parseExtensions(allocator, root),
    };

    return ParseResult{
        .arena = arena,
        .registry = reg,
    };
}

fn parseDeclarations(allocator: Allocator, root: *xml.Element) ![]registry.Declaration {
    var types_elem = root.findChildByTag("types") orelse return error.InvalidRegistry;
    var commands_elem = root.findChildByTag("commands") orelse return error.InvalidRegistry;

    const decl_upper_bound = types_elem.children.len + commands_elem.children.len;
    const decls = try allocator.alloc(registry.Declaration, decl_upper_bound);

    var count: usize = 0;
    count += try parseTypes(allocator, decls, types_elem);
    count += try parseEnums(allocator, decls[count..], root);
    count += try parseCommands(allocator, decls[count..], commands_elem);
    return decls[0..count];
}

fn parseTypes(allocator: Allocator, out: []registry.Declaration, types_elem: *xml.Element) !usize {
    var i: usize = 0;
    var it = types_elem.findChildrenByTag("type");
    while (it.next()) |ty| {
        out[i] = blk: {
            const category = ty.getAttribute("category") orelse {
                break :blk try parseForeigntype(ty);
            };

            if (mem.eql(u8, category, "bitmask")) {
                break :blk try parseBitmaskType(ty);
            } else if (mem.eql(u8, category, "handle")) {
                break :blk try parseHandleType(ty);
            } else if (mem.eql(u8, category, "basetype")) {
                break :blk try parseBaseType(allocator, ty);
            } else if (mem.eql(u8, category, "struct")) {
                break :blk try parseContainer(allocator, ty, false);
            } else if (mem.eql(u8, category, "union")) {
                break :blk try parseContainer(allocator, ty, true);
            } else if (mem.eql(u8, category, "funcpointer")) {
                break :blk try parseFuncPointer(allocator, ty);
            } else if (mem.eql(u8, category, "enum")) {
                break :blk (try parseEnumAlias(ty)) orelse continue;
            }

            continue;
        };

        i += 1;
    }

    return i;
}

fn parseForeigntype(ty: *xml.Element) !registry.Declaration {
    const name = ty.getAttribute("name") orelse return error.InvalidRegistry;
    const depends = ty.getAttribute("requires") orelse if (mem.eql(u8, name, "int"))
        "openxr_platform_defines" // for some reason, int doesn't depend on xr_platform (but the other c types do)
    else
        return error.InvalidRegistry;

    return registry.Declaration{
        .name = name,
        .decl_type = .{ .foreign = .{ .depends = depends } },
    };
}

fn parseBitmaskType(ty: *xml.Element) !registry.Declaration {
    if (ty.getAttribute("name")) |name| {
        const alias = ty.getAttribute("alias") orelse return error.InvalidRegistry;
        return registry.Declaration{
            .name = name,
            .decl_type = .{ .alias = .{ .name = alias, .target = .other_type } },
        };
    } else {
        return registry.Declaration{
            .name = ty.getCharData("name") orelse return error.InvalidRegistry,
            .decl_type = .{ .bitmask = .{ .bits_enum = ty.getAttribute("bitvalues") } },
        };
    }
}

fn parseHandleType(ty: *xml.Element) !registry.Declaration {
    // Parent is not handled in case of an alias
    if (ty.getAttribute("name")) |name| {
        const alias = ty.getAttribute("alias") orelse return error.InvalidRegistry;
        return registry.Declaration{
            .name = name,
            .decl_type = .{ .alias = .{ .name = alias, .target = .other_type } },
        };
    } else {
        const name = ty.getCharData("name") orelse return error.InvalidRegistry;
        const handle_type = ty.getCharData("type") orelse return error.InvalidRegistry;
        const dispatchable = mem.eql(u8, handle_type, "XR_DEFINE_HANDLE");
        if (!dispatchable and !mem.eql(u8, handle_type, "XR_DEFINE_NON_DISPATCHABLE_HANDLE")) {
            return error.InvalidRegistry;
        }

        return registry.Declaration{
            .name = name,
            .decl_type = .{
                .handle = .{
                    .parent = ty.getAttribute("parent"),
                    .is_dispatchable = dispatchable,
                },
            },
        };
    }
}

fn parseBaseType(allocator: Allocator, ty: *xml.Element) !registry.Declaration {
    const name = ty.getCharData("name") orelse return error.InvalidRegistry;
    if (ty.getCharData("type")) |_| {
        var tok = cparse.XmlCTokenizer.init(ty);
        return try cparse.parseTypedef(allocator, &tok, false);
    } else {
        // Either ANativeWindow, AHardwareBuffer or CAMetalLayer. The latter has a lot of
        // macros, which is why this part is not built into the xml/c parser.
        return registry.Declaration{
            .name = name,
            .decl_type = .{ .foreign = .{ .depends = &.{} } },
        };
    }
}

fn parseContainer(allocator: Allocator, ty: *xml.Element, is_union: bool) !registry.Declaration {
    const name = ty.getAttribute("name") orelse return error.InvalidRegistry;

    if (ty.getAttribute("alias")) |alias| {
        return registry.Declaration{
            .name = name,
            .decl_type = .{ .alias = .{ .name = alias, .target = .other_type } },
        };
    }

    var members = try allocator.alloc(registry.Container.Field, ty.children.len);

    var i: usize = 0;
    var it = ty.findChildrenByTag("member");
    var maybe_stype: ?[]const u8 = null;
    while (it.next()) |member| {
        var xctok = cparse.XmlCTokenizer.init(member);
        members[i] = try cparse.parseMember(allocator, &xctok, false);
        if (mem.eql(u8, members[i].name, "type")) {
            if (member.getAttribute("values")) |stype| {
                maybe_stype = stype;
            }
        }

        if (member.getAttribute("optional")) |optionals| {
            var optional_it = mem.split(u8, optionals, ",");
            if (optional_it.next()) |first_optional| {
                members[i].is_optional = mem.eql(u8, first_optional, "true");
            } else {
                // Optional is empty, probably incorrect.
                return error.InvalidRegistry;
            }
        }
        i += 1;
    }

    members = members[0..i];

    var maybe_extends: ?[][]const u8 = null;
    if (ty.getAttribute("structextends")) |extends| {
        const n_structs = std.mem.count(u8, extends, ",") + 1;
        maybe_extends = try allocator.alloc([]const u8, n_structs);
        var struct_extends = std.mem.split(u8, extends, ",");
        var j: usize = 0;
        while (struct_extends.next()) |struct_extend| {
            maybe_extends.?[j] = struct_extend;
            j += 1;
        }
    }

    it = ty.findChildrenByTag("member");
    for (members) |*member| {
        const member_elem = it.next().?;
        try parsePointerMeta(.{ .container = members }, &member.field_type, member_elem);

        // next isn't always properly marked as optional, so just manually override it,
        if (mem.eql(u8, member.name, "next")) {
            member.field_type.pointer.is_optional = true;
        }
    }

    return registry.Declaration{
        .name = name,
        .decl_type = .{
            .container = .{
                .stype = maybe_stype,
                .fields = members,
                .is_union = is_union,
                .extends = maybe_extends,
            },
        },
    };
}

fn parseFuncPointer(allocator: Allocator, ty: *xml.Element) !registry.Declaration {
    var xctok = cparse.XmlCTokenizer.init(ty);
    return try cparse.parseTypedef(allocator, &xctok, true);
}

// For some reason, the DeclarationType cannot be passed to lenToPointer, as
// that causes the Zig compiler to generate invalid code for the function. Using a
// dedicated enum fixes the issue...
const Fields = union(enum) {
    command: []registry.Command.Param,
    container: []registry.Container.Field,
};

// returns .{ size, nullable }
fn lenToPointer(fields: Fields, len: []const u8) std.meta.Tuple(&.{ registry.Pointer.PointerSize, bool }) {
    switch (fields) {
        .command => |params| {
            for (params) |*param| {
                if (mem.eql(u8, param.name, len)) {
                    param.is_buffer_len = true;
                    return .{ .{ .other_field = param.name }, false };
                }
            }
        },
        .container => |members| {
            for (members) |*member| {
                if (mem.eql(u8, member.name, len)) {
                    member.is_buffer_len = true;
                    return .{ .{ .other_field = member.name }, member.is_optional };
                }
            }
        },
    }

    if (mem.eql(u8, len, "null-terminated")) {
        return .{ .zero_terminated, false };
    } else {
        return .{ .many, false };
    }
}

fn parsePointerMeta(fields: Fields, type_info: *registry.TypeInfo, elem: *xml.Element) !void {
    if (elem.getAttribute("len")) |lens| {
        var it = mem.split(u8, lens, ",");
        var current_type_info = type_info;
        while (current_type_info.* == .pointer) {
            // TODO: Check altlen
            const size = if (it.next()) |len_str| blk: {
                const size_optional = lenToPointer(fields, len_str);
                current_type_info.pointer.is_optional = size_optional[1];
                break :blk size_optional[0];
            } else .many;
            current_type_info.pointer.size = size;
            current_type_info = current_type_info.pointer.child;
        }

        if (it.next()) |_| {
            // There are more elements in the `len` attribute than there are pointers
            // Something probably went wrong
            std.log.err("len: {s}", .{lens});
            return error.InvalidRegistry;
        }
    }

    if (elem.getAttribute("optional")) |optionals| {
        var it = mem.split(u8, optionals, ",");
        var current_type_info = type_info;
        while (current_type_info.* == .pointer) {
            if (it.next()) |current_optional| {
                current_type_info.pointer.is_optional = mem.eql(u8, current_optional, "true");
            } else {
                current_type_info.pointer.is_optional = true;
                // There is no information for this pointer, probably incorrect.
                // return error.InvalidRegistry;
            }

            current_type_info = current_type_info.pointer.child;
        }
    } else if (mem.eql(u8, elem.getCharData("name") orelse "", "next")) {
        type_info.pointer.is_optional = true;
    }
}

fn parseEnumAlias(elem: *xml.Element) !?registry.Declaration {
    if (elem.getAttribute("alias")) |alias| {
        const name = elem.getAttribute("name") orelse return error.InvalidRegistry;
        return registry.Declaration{
            .name = name,
            .decl_type = .{ .alias = .{ .name = alias, .target = .other_type } },
        };
    }

    return null;
}

fn parseEnums(allocator: Allocator, out: []registry.Declaration, root: *xml.Element) !usize {
    var i: usize = 0;
    var it = root.findChildrenByTag("enums");
    while (it.next()) |enums| {
        const name = enums.getAttribute("name") orelse return error.InvalidRegistry;
        if (mem.eql(u8, name, api_constants_name)) {
            continue;
        }

        out[i] = .{
            .name = name,
            .decl_type = .{ .enumeration = try parseEnumFields(allocator, enums) },
        };
        i += 1;
    }

    return i;
}

fn parseEnumFields(allocator: Allocator, elem: *xml.Element) !registry.Enum {
    // TODO: `type` was added recently, fall back to checking endswith FlagBits for older versions?
    const enum_type = elem.getAttribute("type") orelse return error.InvalidRegistry;
    const is_bitmask = mem.eql(u8, enum_type, "bitmask");
    if (!is_bitmask and !mem.eql(u8, enum_type, "enum")) {
        return error.InvalidRegistry;
    }

    const fields = try allocator.alloc(registry.Enum.Field, elem.children.len);

    var i: usize = 0;
    var it = elem.findChildrenByTag("enum");
    while (it.next()) |field| {
        fields[i] = try parseEnumField(field);
        i += 1;
    }

    return registry.Enum{
        .fields = fields[0..i],
        .is_bitmask = is_bitmask,
    };
}

fn parseEnumField(field: *xml.Element) !registry.Enum.Field {
    const is_compat_alias = if (field.getAttribute("comment")) |comment|
        mem.eql(u8, comment, "Backwards-compatible alias containing a typo") or
            mem.eql(u8, comment, "Deprecated name for backwards compatibility")
    else
        false;

    const name = field.getAttribute("name") orelse return error.InvalidRegistry;
    const value: registry.Enum.Value = blk: {
        // An enum variant's value could be defined by any of the following attributes:
        // - value: Straight up value of the enum variant, in either base 10 or 16 (prefixed with 0x).
        // - bitpos: Used for bitmasks, and can also be set in extensions.
        // - alias: The field is an alias of another variant within the same enum.
        // - offset: Used with features and extensions, where a non-bitpos value is added to an enum.
        //     The value is given by `1e9 + (extr_nr - 1) * 1e3 + offset`, where `ext_nr` is either
        //     given by the `extnumber` field (in the case of a feature), or given in the parent <extension>
        //     tag. In the latter case its passed via the `ext_nr` parameter.
        if (field.getAttribute("value")) |value| {
            if (mem.startsWith(u8, value, "0x")) {
                break :blk .{ .bit_vector = try std.fmt.parseInt(i32, value[2..], 16) };
            } else {
                break :blk .{ .int = try std.fmt.parseInt(i32, value, 10) };
            }
        } else if (field.getAttribute("bitpos")) |bitpos| {
            break :blk .{ .bitpos = try std.fmt.parseInt(u5, bitpos, 10) };
        } else if (field.getAttribute("alias")) |alias| {
            break :blk .{ .alias = .{ .name = alias, .is_compat_alias = is_compat_alias } };
        } else {
            return error.InvalidRegistry;
        }
    };

    return registry.Enum.Field{
        .name = name,
        .value = value,
    };
}

fn parseCommands(allocator: Allocator, out: []registry.Declaration, commands_elem: *xml.Element) !usize {
    var i: usize = 0;
    var it = commands_elem.findChildrenByTag("command");
    while (it.next()) |elem| {
        out[i] = try parseCommand(allocator, elem);
        i += 1;
    }

    return i;
}

fn splitCommaAlloc(allocator: Allocator, text: []const u8) ![][]const u8 {
    var n_codes: usize = 1;
    for (text) |c| {
        if (c == ',') n_codes += 1;
    }

    const codes = try allocator.alloc([]const u8, n_codes);
    var it = mem.split(u8, text, ",");
    for (codes) |*code| {
        code.* = it.next().?;
    }

    return codes;
}

fn parseCommand(allocator: Allocator, elem: *xml.Element) !registry.Declaration {
    if (elem.getAttribute("alias")) |alias| {
        const name = elem.getAttribute("name") orelse return error.InvalidRegistry;
        return registry.Declaration{
            .name = name,
            .decl_type = .{ .alias = .{ .name = alias, .target = .other_command } },
        };
    }

    const proto = elem.findChildByTag("proto") orelse return error.InvalidRegistry;
    var proto_xctok = cparse.XmlCTokenizer.init(proto);
    const command_decl = try cparse.parseParamOrProto(allocator, &proto_xctok, false);

    var params = try allocator.alloc(registry.Command.Param, elem.children.len);

    var i: usize = 0;
    var it = elem.findChildrenByTag("param");
    while (it.next()) |param| {
        var xctok = cparse.XmlCTokenizer.init(param);
        const decl = try cparse.parseParamOrProto(allocator, &xctok, false);
        params[i] = .{
            .name = decl.name,
            .param_type = decl.decl_type.typedef,
            .is_buffer_len = false,
        };
        i += 1;
    }

    const return_type = try allocator.create(registry.TypeInfo);
    return_type.* = command_decl.decl_type.typedef;

    var success_codes: [][]const u8 = if (elem.getAttribute("successcodes")) |codes|
        try splitCommaAlloc(allocator, codes)
    else
        &[_][]const u8{};

    var error_codes: [][]const u8 = if (elem.getAttribute("errorcodes")) |codes|
        try splitCommaAlloc(allocator, codes)
    else
        &[_][]const u8{};

    for (success_codes, 0..) |code, session_i| {
        if (mem.eql(u8, code, "XR_SESSION_LOSS_PENDING")) {
            var move_i = session_i + 1;
            while (move_i < success_codes.len) : (move_i += 1) {
                success_codes[move_i - 1] = success_codes[move_i];
            }
            success_codes = success_codes[0..success_codes.len];
            success_codes.len = success_codes.len - 1;

            const old_error_codes = error_codes;
            error_codes = try allocator.alloc([]const u8, error_codes.len + 1);
            mem.copy([]const u8, error_codes, old_error_codes);
            error_codes[error_codes.len - 1] = code;
            allocator.free(old_error_codes);
            break;
        }
    }

    params = params[0..i];

    it = elem.findChildrenByTag("param");
    for (params) |*param| {
        const param_elem = it.next().?;
        try parsePointerMeta(.{ .command = params }, &param.param_type, param_elem);
    }

    return registry.Declaration{
        .name = command_decl.name,
        .decl_type = .{
            .command = .{
                .params = params,
                .return_type = return_type,
                .success_codes = success_codes,
                .error_codes = error_codes,
            },
        },
    };
}

fn parseApiConstants(allocator: Allocator, root: *xml.Element) ![]registry.ApiConstant {
    const enums = blk: {
        var it = root.findChildrenByTag("enums");
        while (it.next()) |child| {
            const name = child.getAttribute("name") orelse continue;
            if (mem.eql(u8, name, api_constants_name)) {
                break :blk child;
            }
        }

        return error.InvalidRegistry;
    };

    const types = root.findChildByTag("types") orelse return error.InvalidRegistry;
    const n_defines = blk: {
        var n_defines: usize = 0;
        var it = types.findChildrenByTag("type");
        while (it.next()) |ty| {
            if (ty.getAttribute("category")) |category| {
                if (mem.eql(u8, category, "define")) {
                    n_defines += 1;
                }
            }
        }
        break :blk n_defines;
    };

    const extensions = root.findChildByTag("extensions") orelse return error.InvalidRegistry;
    const n_extension_defines = blk: {
        var n_extension_defines: usize = 0;
        var it = extensions.findChildrenByTag("extension");
        while (it.next()) |ext| {
            const require = ext.findChildByTag("require") orelse return error.InvalidRegistry;
            var defines = require.findChildrenByTag("enum");
            while (defines.next()) |e| {
                if (e.getAttribute("offset") != null and e.getAttribute("extends") != null) continue;

                const name = e.getAttribute("name") orelse continue;
                if (mem.endsWith(u8, name, "SPEC_VERSION")) continue;
                if (mem.endsWith(u8, name, "EXTENSION_NAME")) continue;

                n_extension_defines += 1;
            }
        }
        break :blk n_extension_defines;
    };

    const constants = try allocator.alloc(registry.ApiConstant, enums.children.len + n_defines + n_extension_defines);

    var i: usize = 0;
    var it = enums.findChildrenByTag("enum");
    while (it.next()) |constant| {
        const expr = if (constant.getAttribute("value")) |expr|
            expr
        else if (constant.getAttribute("alias")) |alias|
            alias
        else
            return error.InvalidRegistry;

        constants[i] = .{
            .name = constant.getAttribute("name") orelse return error.InvalidRegistry,
            .value = .{ .expr = expr },
        };

        i += 1;
    }

    i += try parseDefines(types, constants[i..]);
    i += try parseExtensionDefines(extensions, constants[i..]);
    return constants[0..i];
}

fn parseDefines(types: *xml.Element, out: []registry.ApiConstant) !usize {
    var i: usize = 0;
    var it = types.findChildrenByTag("type");
    while (it.next()) |ty| {
        const category = ty.getAttribute("category") orelse continue;
        if (!mem.eql(u8, category, "define")) {
            continue;
        }

        const name = ty.getCharData("name") orelse continue;
        if (mem.eql(u8, name, "XR_CURRENT_API_VERSION")) {
            var xctok = cparse.XmlCTokenizer.init(ty);
            out[i] = .{
                .name = name,
                .value = .{ .version = cparse.parseVersion(&xctok) catch continue },
            };
        } else {
            const expr = mem.trim(u8, ty.children[2].char_data, " ");

            // TODO this doesn't work with all #defines yet (need to handle hex, U/L suffix, etc.)
            _ = std.fmt.parseInt(i32, expr, 10) catch continue;

            out[i] = .{
                .name = name,
                .value = .{ .expr = expr },
            };
        }
        i += 1;
    }

    return i;
}

fn parseExtensionDefines(extensions: *xml.Element, out: []registry.ApiConstant) !usize {
    var i: usize = 0;
    var it = extensions.findChildrenByTag("extension");

    while (it.next()) |ext| {
        const require = ext.findChildByTag("require") orelse return error.InvalidRegistry;
        var defines = require.findChildrenByTag("enum");
        while (defines.next()) |e| {
            if (e.getAttribute("offset") != null and e.getAttribute("extends") != null) continue;

            const name = e.getAttribute("name") orelse continue;
            if (mem.endsWith(u8, name, "SPEC_VERSION")) continue;
            if (mem.endsWith(u8, name, "EXTENSION_NAME")) continue;

            out[i] = .{
                .name = name,
                .value = .{ .expr = e.getAttribute("value") orelse continue },
            };

            i += 1;
        }
    }

    return i;
}

fn parseTags(allocator: Allocator, root: *xml.Element) ![]registry.Tag {
    var tags_elem = root.findChildByTag("tags") orelse return error.InvalidRegistry;
    const tags = try allocator.alloc(registry.Tag, tags_elem.children.len);

    var i: usize = 0;
    var it = tags_elem.findChildrenByTag("tag");
    while (it.next()) |tag| {
        tags[i] = .{
            .name = tag.getAttribute("name") orelse return error.InvalidRegistry,
            .author = tag.getAttribute("author") orelse return error.InvalidRegistry,
        };

        i += 1;
    }

    return tags[0..i];
}

fn parseFeatures(allocator: Allocator, root: *xml.Element) ![]registry.Feature {
    var it = root.findChildrenByTag("feature");
    var count: usize = 0;
    while (it.next()) |_| count += 1;

    const features = try allocator.alloc(registry.Feature, count);
    var i: usize = 0;
    it = root.findChildrenByTag("feature");
    while (it.next()) |feature| {
        features[i] = try parseFeature(allocator, feature);
        i += 1;
    }

    return features;
}

fn parseFeature(allocator: Allocator, feature: *xml.Element) !registry.Feature {
    const name = feature.getAttribute("name") orelse return error.InvalidRegistry;
    const feature_level = blk: {
        const number = feature.getAttribute("number") orelse return error.InvalidRegistry;
        break :blk try splitFeatureLevel(number, ".");
    };

    var requires = try allocator.alloc(registry.Require, feature.children.len);
    var i: usize = 0;
    var it = feature.findChildrenByTag("require");
    while (it.next()) |require| {
        requires[i] = try parseRequire(allocator, require, null);
        i += 1;
    }

    return registry.Feature{
        .name = name,
        .level = feature_level,
        .requires = requires[0..i],
    };
}

fn parseEnumExtension(elem: *xml.Element, parent_extnumber: ?u31) !?registry.Require.EnumExtension {
    // check for either _SPEC_VERSION or _EXTENSION_NAME
    const extends = elem.getAttribute("extends") orelse return null;

    if (elem.getAttribute("offset")) |offset_str| {
        const offset = try std.fmt.parseInt(u31, offset_str, 10);
        const name = elem.getAttribute("name") orelse return error.InvalidRegistry;
        const extnumber = if (elem.getAttribute("extnumber")) |num|
            try std.fmt.parseInt(u31, num, 10)
        else
            null;

        const actual_extnumber = extnumber orelse parent_extnumber orelse return error.InvalidRegistry;
        const value = blk: {
            const abs_value = enumExtOffsetToValue(actual_extnumber, offset);
            if (elem.getAttribute("dir")) |dir| {
                if (mem.eql(u8, dir, "-")) {
                    break :blk -@as(i32, abs_value);
                } else {
                    return error.InvalidRegistry;
                }
            }

            break :blk @as(i32, abs_value);
        };

        return registry.Require.EnumExtension{
            .extends = extends,
            .extnumber = actual_extnumber,
            .field = .{ .name = name, .value = .{ .int = value } },
        };
    }

    return registry.Require.EnumExtension{
        .extends = extends,
        .extnumber = parent_extnumber,
        .field = try parseEnumField(elem),
    };
}

fn enumExtOffsetToValue(extnumber: u31, offset: u31) u31 {
    const extension_value_base = 1000000000;
    const extension_block = 1000;
    return extension_value_base + (extnumber - 1) * extension_block + offset;
}

fn parseRequire(allocator: Allocator, require: *xml.Element, extnumber: ?u31) !registry.Require {
    var n_extends: usize = 0;
    var n_types: usize = 0;
    var n_commands: usize = 0;

    var it = require.elements();
    while (it.next()) |elem| {
        if (mem.eql(u8, elem.tag, "enum")) {
            n_extends += 1;
        } else if (mem.eql(u8, elem.tag, "type")) {
            n_types += 1;
        } else if (mem.eql(u8, elem.tag, "command")) {
            n_commands += 1;
        }
    }

    const extends = try allocator.alloc(registry.Require.EnumExtension, n_extends);
    const types = try allocator.alloc([]const u8, n_types);
    const commands = try allocator.alloc([]const u8, n_commands);

    var i_extends: usize = 0;
    var i_types: usize = 0;
    var i_commands: usize = 0;

    it = require.elements();
    while (it.next()) |elem| {
        if (mem.eql(u8, elem.tag, "enum")) {
            if (try parseEnumExtension(elem, extnumber)) |ext| {
                extends[i_extends] = ext;
                i_extends += 1;
            }
        } else if (mem.eql(u8, elem.tag, "type")) {
            types[i_types] = elem.getAttribute("name") orelse return error.InvalidRegistry;
            i_types += 1;
        } else if (mem.eql(u8, elem.tag, "command")) {
            commands[i_commands] = elem.getAttribute("name") orelse return error.InvalidRegistry;
            i_commands += 1;
        }
    }

    const required_feature_level = blk: {
        const feature_level = require.getAttribute("feature") orelse break :blk null;
        if (!mem.startsWith(u8, feature_level, "XR_VERSION_")) {
            return error.InvalidRegistry;
        }

        break :blk try splitFeatureLevel(feature_level["XR_VERSION_".len..], "_");
    };

    return registry.Require{
        .extends = extends[0..i_extends],
        .types = types[0..i_types],
        .commands = commands[0..i_commands],
        .required_feature_level = required_feature_level,
        .required_extension = require.getAttribute("extension"),
    };
}

fn parseExtensions(allocator: Allocator, root: *xml.Element) ![]registry.Extension {
    const extensions_elem = root.findChildByTag("extensions") orelse return error.InvalidRegistry;

    const extensions = try allocator.alloc(registry.Extension, extensions_elem.children.len);
    var i: usize = 0;
    var it = extensions_elem.findChildrenByTag("extension");
    while (it.next()) |extension| {
        // Some extensions (in particular 94) are disabled, so just skip them
        if (extension.getAttribute("supported")) |supported| {
            if (mem.eql(u8, supported, "disabled")) {
                continue;
            }
        }

        extensions[i] = try parseExtension(allocator, extension);
        i += 1;
    }

    return extensions[0..i];
}

fn findExtVersion(extension: *xml.Element) !u32 {
    var req_it = extension.findChildrenByTag("require");
    while (req_it.next()) |req| {
        var enum_it = req.findChildrenByTag("enum");
        while (enum_it.next()) |e| {
            const name = e.getAttribute("name") orelse continue;
            const value = e.getAttribute("value") orelse continue;
            if (mem.endsWith(u8, name, "_SPEC_VERSION")) {
                return try std.fmt.parseInt(u32, value, 10);
            }
        }
    }

    return error.InvalidRegistry;
}

fn parseExtension(allocator: Allocator, extension: *xml.Element) !registry.Extension {
    const name = extension.getAttribute("name") orelse return error.InvalidRegistry;
    const platform = extension.getAttribute("platform");
    const version = try findExtVersion(extension);

    // For some reason there are two ways for an extension to state its required
    // feature level: both seperately in each <require> tag, or using
    // the requiresCore attribute.
    const requires_core = if (extension.getAttribute("requiresCore")) |feature_level|
        try splitFeatureLevel(feature_level, ".")
    else
        null;

    const promoted_to: registry.Extension.Promotion = blk: {
        const promotedto = extension.getAttribute("promotedto") orelse break :blk .none;
        if (mem.startsWith(u8, promotedto, "XR_VERSION_")) {
            const feature_level = try splitFeatureLevel(promotedto["XR_VERSION_".len..], "_");

            break :blk .{ .feature = feature_level };
        }

        break :blk .{ .extension = promotedto };
    };

    const number = blk: {
        const number_str = extension.getAttribute("number") orelse return error.InvalidRegistry;
        break :blk try std.fmt.parseInt(u31, number_str, 10);
    };

    const ext_type: ?registry.Extension.ExtensionType = blk: {
        const ext_type_str = extension.getAttribute("type") orelse break :blk null;
        if (mem.eql(u8, ext_type_str, "instance")) {
            break :blk .instance;
        } else if (mem.eql(u8, ext_type_str, "device")) {
            break :blk .device;
        } else {
            return error.InvalidRegistry;
        }
    };

    const depends = blk: {
        const requires_str = extension.getAttribute("requires") orelse break :blk &[_][]const u8{};
        break :blk try splitCommaAlloc(allocator, requires_str);
    };

    var requires = try allocator.alloc(registry.Require, extension.children.len);
    var i: usize = 0;
    var it = extension.findChildrenByTag("require");
    while (it.next()) |require| {
        requires[i] = try parseRequire(allocator, require, number);
        i += 1;
    }

    return registry.Extension{
        .name = name,
        .number = number,
        .version = version,
        .extension_type = ext_type,
        .depends = depends,
        .promoted_to = promoted_to,
        .platform = platform,
        .required_feature_level = requires_core,
        .requires = requires[0..i],
    };
}

fn splitFeatureLevel(ver: []const u8, split: []const u8) !registry.FeatureLevel {
    var it = mem.split(u8, ver, split);

    const major = it.next() orelse return error.InvalidFeatureLevel;
    const minor = it.next() orelse return error.InvalidFeatureLevel;
    if (it.next() != null) {
        return error.InvalidFeatureLevel;
    }

    return registry.FeatureLevel{
        .major = try std.fmt.parseInt(u32, major, 10),
        .minor = try std.fmt.parseInt(u32, minor, 10),
    };
}
