/*---------------------------------------------------------------------------
  Copyright 2024 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module uv/tty
pub import std/core/bytes
pub import uv/event-loop
pub import uv/stream
pub import uv/utils

extern import
  c file "inline/tty.c"

// A uv tty handle
pub value struct uv-tty { internal : any };

// Get the underlying stream handle from a TTY handle
pub fun tty/stream(tty: uv-tty): uv-stream
  Uv-stream(tty.internal)

pub type tty-mode
  // Initial/normal terminal mode
  UV_TTY_MODE_NORMAL
  // Raw input mode (On Windows, ENABLE_WINDOW_INPUT is also enabled)
  UV_TTY_MODE_RAW
  // Binary-safe I/O mode for IPC (Unix-only)
  UV_TTY_MODE_IO

pub type tty-vtermstate
  // The console supports handling of virtual terminal sequences
  // (Windows10 new console, ConEmu)
  UV_TTY_SUPPORTED
  // The console cannot process virtual terminal sequences. (Legacy console)
  UV_TTY_UNSUPPORTED

// Initialize a new TTY stream with the given file descriptor. Usually the file descriptor will be:
// - 0 = stdin
// - 1 = stdout
// - 2 = stderr
// On Unix this function will determine the path of the fd of the terminal using ttyname_r(3), open it, and use it if the passed file descriptor refers to a TTY.
// This lets libuv put the tty in non-blocking mode without affecting other processes that share the tty.
// This function is not thread safe on systems that don’t support ioctl TIOCGPTN or TIOCPTYGNAME, for instance OpenBSD and Solaris.
pub extern uv-tty-init(fd: int32): io-noexn error<uv-tty>
  c "kk_uv_tty_init"

// Set the TTY using the specified terminal mode.
pub extern uv-tty-set-mode(^tty: uv-tty, mode: tty-mode): io-noexn int32
  c inline "uv_tty_set_mode(kk_owned_handle_to_uv_handle(uv_tty_t, #1), kk_tty_mode_to_uv_tty_mode(#2, kk_context()))"

// To be called when the program exits. Resets TTY settings to default values for the next process to take over.
// This function is async signal-safe on Unix platforms but can fail with error code UV_EBUSY if you call it when execution is inside uv_tty_set_mode().
pub extern uv-tty-reset-mode(): io-noexn int32
  c inline "uv_tty_reset_mode()"

// Gets the current Window size. On success it returns 0.
pub extern uv-tty-get-winsize(^tty: uv-tty): io-noexn error<(int32, int32)>
  c "kk_uv_tty_get_winsize"

// Controls whether console virtual terminal sequences are processed by libuv or console. Useful in particular for enabling ConEmu support of ANSI X3.64 and Xterm 256 colors. Otherwise Windows10 consoles are usually detected automatically.
// This function is only meaningful on Windows systems. On Unix it is silently ignored.
pub extern uv-tty-set-vtermstate(^state: tty-vtermstate): io-noexn ()
  c inline "uv_tty_set_vterm_state(kk_tty_vtermstate_to_uv_tty_vtermstate(#1, kk_context()))"

// Get the current state of whether console virtual terminal sequences are handled by libuv or the console.
// This function is not implemented on Unix, where it returns UV_ENOTSUP.
pub extern uv-tty-get-vtermstate(): io-noexn error<tty-vtermstate>
  c "kk_uv_tty_get_vtermstate"
