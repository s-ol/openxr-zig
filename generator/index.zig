pub const generateXr = @import("openxr/generator.zig").generate;
pub const XrGenerateStep = @import("openxr/build_integration.zig").GenerateStep;

test "main" {
    _ = @import("xml.zig");
    _ = @import("openxr/c_parse.zig");
}
