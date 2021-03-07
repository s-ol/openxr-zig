pub const generateXr = @import("openxr/generator.zig").generate;
pub const XrGenerateStep = @import("openxr/build_integration.zig").GenerateStep;
pub const generateSpirv = @import("spirv/generator.zig").generate;
pub const ShaderCompileStep = @import("build_integration.zig").ShaderCompileStep;

test "main" {
    _ = @import("xml.zig");
    _ = @import("openxr/c_parse.zig");
}
