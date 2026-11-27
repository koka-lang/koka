/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* File-system primitives for the build: modification times, an atomic write,
   and a bounded read.

   They are syscalls rather than shell commands because a process spawn
   MEASURED ~8ms against ~2.4us for the syscall, and the build asks for an
   mtime per module AND per dependency and writes three files (.kki, .c, .h)
   per compiled module.

   Paths are UTF-8. Windows takes them as UTF-16 through the `W` APIs, so a
   non-ASCII path works regardless of the active code page. */
#if defined(_WIN32)
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <wchar.h>
#else
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Times are nanoseconds since the Unix epoch, not whole seconds: a dependency
   rewritten in the same second as its dependent's interface must still
   compare as newer. */
#if defined(_WIN32)
/* A FILETIME counts 100ns intervals since 1601-01-01, 11644473600s before the epoch. */
static int64_t kk_compiler_filetime_ns(FILETIME ft) {
  int64_t t = (int64_t)(((uint64_t)ft.dwHighDateTime << 32) | ft.dwLowDateTime);
  return (t - INT64_C(116444736000000000)) * 100;
}
#else
static int64_t kk_compiler_stat_mtime_ns(const struct stat* st) {
#if defined(__APPLE__)
  return (int64_t)st->st_mtimespec.tv_sec * 1000000000 + st->st_mtimespec.tv_nsec;
#elif defined(__linux__)
  return (int64_t)st->st_mtim.tv_sec * 1000000000 + st->st_mtim.tv_nsec;
#else
  return (int64_t)st->st_mtime * 1000000000;
#endif
}
#endif

/* 0 for a missing or unreadable file, which callers treat as infinitely
   stale. An existing file is therefore never 0, even where the file system
   records no times at all (the playground's in-memory WASI file system
   reports 0 for every file). */
kk_integer_t kk_compiler_file_mtime(kk_string_t p, kk_context_t* ctx) {
  int64_t r = 0;
  bool found = false;
#if defined(_WIN32)
  kk_with_string_as_qutf16w_borrow(p, wpath, ctx) {
    WIN32_FILE_ATTRIBUTE_DATA fa;
    if (GetFileAttributesExW(wpath, GetFileExInfoStandard, &fa)) {
      r = kk_compiler_filetime_ns(fa.ftLastWriteTime);
      found = true;
    }
  }
#else
  struct stat st;
  const char* cpath = kk_string_cbuf_borrow(p, NULL, ctx);
  if (cpath != NULL && stat(cpath, &st) == 0) {
    r = kk_compiler_stat_mtime_ns(&st);
    found = true;
  }
#endif
  if (found && r <= 0) r = 1;
  kk_string_drop(p, ctx);
  return kk_integer_from_int64(r, ctx);
}

/* Wall-clock time on the scale of `kk_compiler_file_mtime`. */
kk_integer_t kk_compiler_epoch_nanos(kk_context_t* ctx) {
#if defined(_WIN32)
  FILETIME ft;
  GetSystemTimePreciseAsFileTime(&ft);
  return kk_integer_from_int64(kk_compiler_filetime_ns(ft), ctx);
#else
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return kk_integer_from_int64((int64_t)ts.tv_sec * 1000000000 + ts.tv_nsec, ctx);
#endif
}

/* Write `content` to a unique temp file in the DESTINATION's own directory,
   then rename it over the destination. A rename within one file system is
   atomic, so a concurrent reader sees either the old file or the complete new
   one, never a half-written interface or C file. Several koka processes share
   one build cache (8 sweep workers do exactly this), which is why the write
   cannot truncate and write in place. The temp file must be in the
   destination's directory, or the rename could cross file systems and lose
   atomicity.

   Returns false on any failure; the Koka side then writes the file directly. */
#if defined(_WIN32)
static bool kk_compiler_write_atomic_w(const wchar_t* wpath, const char* cbuf, kk_ssize_t clen) {
  /* the directory part of the path: everything before the last separator */
  size_t dlen = wcslen(wpath);
  while (dlen > 0 && wpath[dlen-1] != L'/' && wpath[dlen-1] != L'\\') dlen--;
  wchar_t* dir = (wchar_t*)malloc((dlen + 2) * sizeof(wchar_t));
  if (dir == NULL) return false;
  if (dlen == 0) { dir[0] = L'.'; dir[1] = 0; }
  else { memcpy(dir, wpath, dlen * sizeof(wchar_t)); dir[dlen] = 0; }
  /* GetTempFileNameW creates the file, so the name is unique among concurrent writers */
  wchar_t tmp[MAX_PATH];
  bool ok = (GetTempFileNameW(dir, L"kkw", 0, tmp) != 0);
  free(dir);
  if (!ok) return false;
  FILE* f = NULL;
  if (_wfopen_s(&f, tmp, L"wb") != 0 || f == NULL) { DeleteFileW(tmp); return false; }
  if (clen > 0 && fwrite(cbuf, 1, (size_t)clen, f) != (size_t)clen) ok = false;
  if (fclose(f) != 0) ok = false;
  if (ok && !MoveFileExW(tmp, wpath, MOVEFILE_REPLACE_EXISTING)) ok = false;
  if (!ok) DeleteFileW(tmp);
  return ok;
}
#endif

bool kk_compiler_write_atomic(kk_string_t path, kk_string_t content, kk_context_t* ctx) {
  kk_ssize_t clen = 0;
  const char* cbuf = kk_string_cbuf_borrow(content, &clen, ctx);
  bool ok = false;
#if defined(_WIN32)
  if (cbuf != NULL) {
    kk_with_string_as_qutf16w_borrow(path, wpath, ctx) {
      ok = kk_compiler_write_atomic_w(wpath, cbuf, clen);
    }
  }
#elif !defined(__wasi__)   /* wasi-libc has no mkstemp; a WASI build has one writer anyway */
  const char* cpath = kk_string_cbuf_borrow(path, NULL, ctx);
  if (cpath != NULL && cbuf != NULL) {
    const char* slash = strrchr(cpath, '/');
    size_t dlen = (slash == NULL) ? 1 : (size_t)(slash - cpath);
    char* tmp = (char*)malloc(dlen + 16);
    if (tmp != NULL) {
      if (slash == NULL) tmp[0] = '.'; else memcpy(tmp, cpath, dlen);
      memcpy(tmp + dlen, "/.kkw-XXXXXX", 13);  /* includes the NUL */
      int fd = mkstemp(tmp);
      if (fd >= 0) {
        ok = true;
        /* mkstemp creates 0600; generated artifacts are ordinary build output */
        (void)fchmod(fd, 0644);
        kk_ssize_t off = 0;
        while (off < clen) {
          ssize_t w = write(fd, cbuf + off, (size_t)(clen - off));
          if (w <= 0) { ok = false; break; }
          off += (kk_ssize_t)w;
        }
        if (close(fd) != 0) ok = false;
        if (ok && rename(tmp, cpath) != 0) ok = false;
        if (!ok) (void)unlink(tmp);
      }
      free(tmp);
    }
  }
#endif
  kk_string_drop(path, ctx);
  kk_string_drop(content, ctx);
  return ok;
}

/* Read at most `max_bytes` from the head of a file.

   An interface's import block is the first thing in the file, but interfaces
   are far larger than their sources -- compiler/compile/build is 83KB of
   source against a 5.77MB .kki -- so reading the whole file to get 25 lines
   is 69x more I/O than parsing the source would be. */
kk_string_t kk_compiler_read_head(kk_string_t p, kk_integer_t max_bytes, kk_context_t* ctx) {
  kk_ssize_t cap = (kk_ssize_t)kk_integer_clamp_ssize_t(max_bytes, ctx);
  kk_string_t out = kk_string_empty();
  FILE* f = NULL;
  if (cap > 0) {
#if defined(_WIN32)
    kk_with_string_as_qutf16w_borrow(p, wpath, ctx) {
      if (_wfopen_s(&f, wpath, L"rb") != 0) f = NULL;
    }
#else
    const char* cpath = kk_string_cbuf_borrow(p, NULL, ctx);
    if (cpath != NULL) f = fopen(cpath, "rb");
#endif
  }
  if (f != NULL) {
    char* buf = (char*)malloc((size_t)cap + 1);
    if (buf != NULL) {
      size_t n = fread(buf, 1, (size_t)cap, f);
      buf[n] = 0;
      /* a truncated multi-byte sequence at the cut is possible; the caller
         only scans ASCII `import` lines well before the cut, and
         `alloc_from_qutf8` sanitises anything invalid */
      out = kk_string_alloc_from_qutf8(buf, ctx);
      free(buf);
    }
    fclose(f);
  }
  kk_string_drop(p, ctx);
  return out;
}
