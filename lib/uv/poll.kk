/*---------------------------------------------------------------------------
  Copyright 2024 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module uv/poll
import uv/utils
pub import uv/file

extern import
  c file "inline/poll.c"

value struct uv-fs-poll {internal: any }

// Initialize the handle.
pub extern uv-fs-poll-init(): io-noexn error<uv-fs-poll>
  c "kk_uv_fs_poll_init"

// Check the file at path for changes every interval milliseconds.
// For maximum portability, use multi-second intervals.
// Sub-second intervals will not detect all changes on many file systems.
pub extern uv-fs-poll-start(poll: uv-fs-poll, path: string, interval: int, cb: error<(fstat, fstat)> -> io-noexn ()): io-noexn error<()>
  c "kk_uv_fs_poll_start"

// Stop the handle, the callback will no longer be called.
pub extern uv-fs-poll-stop(poll: uv-fs-poll): io-noexn error<()>
  c "kk_uv_fs_poll_stop"

// Get the path being monitored by the handle.
pub extern uv-fs-poll-getpath(poll: uv-fs-poll): io-noexn error<string>
  c "kk_uv_fs_poll_getpath"
