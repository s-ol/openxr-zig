pub const Registry = struct {
    decls: []Declaration,
    api_constants: []ApiConstant,
    tags: []Tag,
    features: []Feature,
    extensions: []Extension,
};

pub const Declaration = struct {
    name: []const u8,
    decl_type: DeclarationType,
};

pub const DeclarationType = union(enum) {
    container: Container,
    enumeration: Enum,
    bitmask: Bitmask,
    handle: Handle,
    command: Command,
    alias: Alias,
    foreign: Foreign,
    typedef: TypeInfo,
    external,
};

pub const Alias = struct {
    pub const Target = enum {
        other_command,
        other_type,
    };

    name: []const u8,
    target: Target,
};

pub const ApiConstant = struct {
    pub const Value = union(enum) {
        expr: []const u8,
        version: [3][]const u8,
    };

    name: []const u8,
    value: Value,
};

pub const Tag = struct {
    name: []const u8,
    author: []const u8,
};

pub const TypeInfo = union(enum) {
    name: []const u8,
    command_ptr: Command,
    pointer: Pointer,
    array: Array,
};

pub const Container = struct {
    pub const Field = struct {
        name: []const u8,
        field_type: TypeInfo,
        bits: ?usize,
        is_buffer_len: bool,
        is_optional: bool,
    };

    stype: ?[]const u8,
    extends: ?[]const []const u8,
    fields: []Field,
    is_union: bool,
};

pub const Enum = struct {
    pub const Value = union(enum) {
        bitpos: u5, // 1 << bitpos
        bit_vector: i32, // Combined flags & some vendor IDs
        int: i32,
        alias: struct {
            name: []const u8,
            is_compat_alias: bool,
        },
    };

    pub const Field = struct {
        name: []const u8,
        value: Value,
    };

    fields: []Field,
    is_bitmask: bool,
};

pub const Bitmask = struct {
    bits_enum: ?[]const u8,
};

pub const Handle = struct {
    parent: ?[]const u8, // XrInstance has no parent
    is_dispatchable: bool,
};

pub const Command = struct {
    pub const Param = struct {
        name: []const u8,
        param_type: TypeInfo,
        is_buffer_len: bool,
    };

    params: []Param,
    return_type: *TypeInfo,
    success_codes: []const []const u8,
    error_codes: []const []const u8,
};

pub const Pointer = struct {
    pub const PointerSize = union(enum) {
        one,
        many, // The length is given by some complex expression, possibly involving another field
        other_field: []const u8, // The length is given by some other field or parameter
        zero_terminated,
    };

    is_const: bool,
    is_optional: bool,
    size: PointerSize,
    child: *TypeInfo,
};

pub const Array = struct {
    pub const ArraySize = union(enum) {
        int: usize,
        alias: []const u8, // Field size is given by an api constant
    };

    size: ArraySize,
    child: *TypeInfo,
};

pub const Foreign = struct {
    depends: []const u8, // Either a header or openxr_platform_defines
};

pub const Feature = struct {
    name: []const u8,
    level: FeatureLevel, // from 'number'
    requires: []Require,
};

pub const Extension = struct {
    pub const ExtensionType = enum {
        instance,
        device,
    };

    pub const Promotion = union(enum) {
        none,
        feature: FeatureLevel,
        extension: []const u8,
    };

    name: []const u8,
    number: u31,
    version: u32,
    extension_type: ?ExtensionType,
    depends: []const []const u8, // Other extensions
    promoted_to: Promotion,
    platform: ?[]const u8,
    required_feature_level: ?FeatureLevel,
    requires: []Require,
};

pub const Require = struct {
    pub const EnumExtension = struct {
        extends: []const u8,
        extnumber: ?u31,
        field: Enum.Field,
    };

    extends: []EnumExtension,
    types: []const []const u8,
    commands: []const []const u8,
    required_feature_level: ?FeatureLevel,
    required_extension: ?[]const u8,
};

pub const FeatureLevel = struct {
    major: u32,
    minor: u32,
};
