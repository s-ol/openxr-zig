const std = @import("std");
const reg = @import("registry.zig");
const xml = @import("../xml.zig");
const renderRegistry = @import("render.zig").render;
const parseXml = @import("parse.zig").parseXml;
const IdRenderer = @import("../id_render.zig").IdRenderer;
const mem = std.mem;
const Allocator = mem.Allocator;
const FeatureLevel = reg.FeatureLevel;

const EnumFieldMerger = struct {
    const EnumExtensionMap = std.StringArrayHashMapUnmanaged(std.ArrayListUnmanaged(reg.Enum.Field));
    const FieldSet = std.StringArrayHashMapUnmanaged(void);

    arena: Allocator,
    registry: *reg.Registry,
    enum_extensions: EnumExtensionMap,
    field_set: FieldSet,

    fn init(arena: Allocator, registry: *reg.Registry) EnumFieldMerger {
        return .{
            .arena = arena,
            .registry = registry,
            .enum_extensions = .{},
            .field_set = .{},
        };
    }

    fn putEnumExtension(self: *EnumFieldMerger, enum_name: []const u8, field: reg.Enum.Field) !void {
        const res = try self.enum_extensions.getOrPut(self.arena, enum_name);
        if (!res.found_existing) {
            res.value_ptr.* = std.ArrayListUnmanaged(reg.Enum.Field){};
        }

        try res.value_ptr.append(self.arena, field);
    }

    fn addRequires(self: *EnumFieldMerger, reqs: []const reg.Require) !void {
        for (reqs) |req| {
            for (req.extends) |enum_ext| {
                try self.putEnumExtension(enum_ext.extends, enum_ext.field);
            }
        }
    }

    fn mergeEnumFields(self: *EnumFieldMerger, name: []const u8, base_enum: *reg.Enum) !void {
        // If there are no extensions for this enum, assume its valid.
        const extensions = self.enum_extensions.get(name) orelse return;

        self.field_set.clearRetainingCapacity();

        const n_fields_upper_bound = base_enum.fields.len + extensions.items.len;
        const new_fields = try self.arena.alloc(reg.Enum.Field, n_fields_upper_bound);
        var i: usize = 0;

        for (base_enum.fields) |field| {
            const res = try self.field_set.getOrPut(self.arena, field.name);
            if (!res.found_existing) {
                new_fields[i] = field;
                i += 1;
            }
        }

        // Assume that if a field name clobbers, the value is the same
        for (extensions.items) |field| {
            const res = try self.field_set.getOrPut(self.arena, field.name);
            if (!res.found_existing) {
                new_fields[i] = field;
                i += 1;
            }
        }

        // Existing base_enum.fields was allocated by `self.arena`, so
        // it gets cleaned up whenever that is deinited.
        base_enum.fields = new_fields[0..i];
    }

    fn merge(self: *EnumFieldMerger) !void {
        for (self.registry.features) |feature| {
            try self.addRequires(feature.requires);
        }

        for (self.registry.extensions) |ext| {
            try self.addRequires(ext.requires);
        }

        // Merge all the enum fields.
        // Assume that all keys of enum_extensions appear in `self.registry.decls`
        for (self.registry.decls) |*decl| {
            if (decl.decl_type == .enumeration) {
                try self.mergeEnumFields(decl.name, &decl.decl_type.enumeration);
            }
        }
    }
};

pub const Generator = struct {
    arena: std.heap.ArenaAllocator,
    registry: reg.Registry,
    id_renderer: IdRenderer,

    fn init(allocator: Allocator, spec: *xml.Element) !Generator {
        const result = try parseXml(allocator, spec);

        const tags = try allocator.alloc([]const u8, result.registry.tags.len);
        for (tags, result.registry.tags) |*tag, registry_tag| tag.* = registry_tag.name;

        return Generator{
            .arena = result.arena,
            .registry = result.registry,
            .id_renderer = IdRenderer.init(allocator, tags),
        };
    }

    fn deinit(self: Generator) void {
        self.arena.deinit();
    }

    fn removePromotedExtensions(self: *Generator) void {
        var write_index: usize = 0;
        for (self.registry.extensions) |ext| {
            if (ext.promoted_to == .none) {
                self.registry.extensions[write_index] = ext;
                write_index += 1;
            }
        }
        self.registry.extensions.len = write_index;
    }

    fn stripFlagBits(self: Generator, name: []const u8) []const u8 {
        const tagless = self.id_renderer.stripAuthorTag(name);
        return tagless[0 .. tagless.len - "FlagBits".len];
    }

    fn stripFlags(self: Generator, name: []const u8) []const u8 {
        const tagless = self.id_renderer.stripAuthorTag(name);
        return tagless[0 .. tagless.len - "Flags64".len];
    }

    // Solve `registry.declarations` according to `registry.extensions` and `registry.features`.
    fn mergeEnumFields(self: *Generator) !void {
        var merger = EnumFieldMerger.init(self.arena.allocator(), &self.registry);
        try merger.merge();
    }

    fn render(self: *Generator, writer: anytype) !void {
        try renderRegistry(writer, self.arena.allocator(), &self.registry, &self.id_renderer);
    }
};

/// Main function for generating the OpenXR bindings. xr.xml is to be provided via `spec_xml`,
/// and the resulting binding is written to `writer`. `allocator` will be used to allocate temporary
/// internal datastructures - mostly via an ArenaAllocator, but sometimes a hashmap uses this allocator
/// directly.
pub fn generate(allocator: Allocator, spec_xml: []const u8, writer: anytype) !void {
    const spec = xml.parse(allocator, spec_xml) catch |err| switch (err) {
        error.InvalidDocument,
        error.UnexpectedEof,
        error.UnexpectedCharacter,
        error.IllegalCharacter,
        error.InvalidEntity,
        error.InvalidName,
        error.InvalidStandaloneValue,
        error.NonMatchingClosingTag,
        error.UnclosedComment,
        error.UnclosedValue,
        => return error.InvalidXml,
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer spec.deinit();

    var gen = Generator.init(allocator, spec.root) catch |err| switch (err) {
        error.InvalidXml,
        error.InvalidCharacter,
        error.Overflow,
        error.InvalidFeatureLevel,
        error.InvalidSyntax,
        error.InvalidTag,
        error.MissingTypeIdentifier,
        error.UnexpectedCharacter,
        error.UnexpectedEof,
        error.UnexpectedToken,
        error.InvalidRegistry,
        => return error.InvalidRegistry,
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer gen.deinit();

    gen.removePromotedExtensions();
    try gen.mergeEnumFields();
    gen.render(writer) catch |err| switch (err) {
        error.InvalidApiConstant,
        error.InvalidConstantExpr,
        error.InvalidRegistry,
        error.UnexpectedCharacter,
        => return error.InvalidRegistry,
        else => |others| return others,
    };
}
