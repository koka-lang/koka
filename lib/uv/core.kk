module uv/core
pub import uv/status-code

extern import
  c { conan="libuv[>=1.47.0]"; vcpkg="libuv"; library="uv" }

extern import
  c file "inline/core"

// The base uv handle type
pub value struct uv-handle { internal: any }
