/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module uv/core
pub import uv/status-code

extern import
  c { conan="libuv[>=1.47.0]"; vcpkg="libuv"; library="uv" }

extern import
  c file "inline/core"

// The base uv handle type
pub value struct uv-handle { internal: any }
