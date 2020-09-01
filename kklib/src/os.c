/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS   // getenv
#endif
#include "kklib.h"

/*--------------------------------------------------------------------------------------------------
  Path max
--------------------------------------------------------------------------------------------------*/
kk_decl_export size_t kk_os_path_max(void);

#if defined(_WIN32)
kk_decl_export size_t kk_os_path_max(void) {
  return 32*1024; // _MAX_PATH;
}

#elif defined(__MACH__)
#include <sys/syslimits.h>
kk_decl_export size_t kk_os_path_max(void) {
  return PATH_MAX;
}

#elif defined(unix) || defined(__unix__) || defined(__unix)
#include <unistd.h>  // pathconf
kk_decl_export size_t kk_os_path_max(void) {
  #ifdef PATH_MAX
  return PATH_MAX;
  #else
  static size_t path_max = 0;
  if (path_max <= 0) {
    long m = pathconf("/", _PC_PATH_MAX);
    if (m <= 0) path_max = 4096;      // guess
    else if (m < 256) path_max = 256; // at least 256
    else path_max = m;
  }
  return path_max;
  #endif
}
#else
kk_decl_export size_t kk_os_path_max(void) {
#ifdef PATH_MAX
  return PATH_MAX;
#else
  return 4096;
#endif
}
#endif

/*--------------------------------------------------------------------------------------------------
  Realpath
--------------------------------------------------------------------------------------------------*/

static kk_string_t kk_os_realpathx(const char* fname, kk_context_t* ctx);

#if defined(_WIN32)
#include <Windows.h>
static kk_string_t kk_os_realpathx(const char* fname, kk_context_t* ctx) {  
  char buf[264];
  DWORD res = GetFullPathNameA(fname, 264, buf, NULL);
  if (res >= 264) {
    // path too long
    // TODO: use GetFullPathNameW to allow longer file names?
    return kk_string_alloc_dup(fname, ctx);
  }
  else {
    return kk_string_alloc_dup(buf, ctx);
  }
}

#elif defined(__linux__) || defined(__CYGWIN__) || defined(__sun) || defined(unix) || defined(__unix__) || defined(__unix)
#include <limits.h>  
static kk_string_t kk_os_realpathx(const char* fname, kk_context_t* ctx) {
  char* rpath   = realpath(fname, NULL);
  kk_string_t s = kk_string_alloc_dup(rpath, ctx);
  free(rpath);
  return s;
}

#else
#pragma message("realpath ignored on this platform")
static kk_string_t kk_os_realpathx(const char* fname, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return kk_string_alloc_dup(fname,ctx);
}
#endif

kk_decl_export kk_string_t kk_os_realpath(kk_string_t fname, kk_context_t* ctx) {
  kk_string_t p = kk_os_realpathx(kk_string_cbuf_borrow(fname), ctx);
  kk_string_drop(fname, ctx);
  return p;
}

/*--------------------------------------------------------------------------------------------------
  Application path
--------------------------------------------------------------------------------------------------*/
#ifdef _WIN32
#define KK_PATH_SEP  ';'
#define KK_DIR_SEP   '\\'
#include <io.h>      
#else
#define KK_PATH_SEP  ':'
#define KK_DIR_SEP   '/'
#include <unistd.h>
#endif

static bool kk_os_file_exists(const char* fname) {
#if defined(_WIN32)
  return (_access(fname, 4 /* R_OK */) == 0);
#else
  return (access(fname, F_OK) == 0);
#endif
}

static kk_string_t kk_os_searchpathx(const char* paths, const char* fname, kk_context_t* ctx) {
  if (paths==NULL || fname==NULL || fname[0]==0) return kk_string_empty();
  const char* p = paths;
  size_t pathslen = strlen(paths);
  size_t fnamelen = strlen(fname);
  char* buf = (char*)kk_malloc(pathslen + fnamelen + 2, ctx);
  if (buf==NULL) return kk_string_empty();

  kk_string_t s = kk_string_empty();
  const char* pend = p + pathslen;
  while (p < pend) {
    const char* r = strchr(p, KK_PATH_SEP);
    if (r==NULL) r = pend;
    size_t plen = (size_t)(r - p);
    memcpy(buf, p, plen);
    memcpy(buf + plen, "/", 1);
    memcpy(buf + plen + 1, fname, fnamelen);
    buf[plen+1+fnamelen] = 0;
    p = (r == pend ? r : r + 1);
    if (kk_os_file_exists(buf)) {
      s = kk_os_realpathx(buf, ctx);
      break;
    }
  }
  kk_free(buf);
  return s;
}

// generic application path by using argv[0] and looking at the current working directory and the PATH environment.
static kk_string_t kk_os_app_path_generic(kk_context_t* ctx) {
  const char* p = ctx->argv[0];
  if (p==0 || strlen(p)==0) return kk_string_empty();
  
  if (p[0]=='/' 
#ifdef _WIN32
      || (p[1]==':' && ((p[0] >= 'a' && p[0] <= 'z') || ((p[0] >= 'A' && p[0] <= 'Z'))) && (p[2]=='\\' || p[2]=='/'))
#endif
     ) {
    // absolute path
    return kk_os_realpathx(p, ctx);
  }
  else if (strchr(p,'/') != NULL 
#ifdef _WIN32
    || strchr(p,'\\') != NULL
#endif
    ) {
    // relative path, combine with "./"
    kk_string_t s = kk_string_alloc_len(strlen(p) + 2, "./", ctx);
    strcat((char*)kk_string_cbuf_borrow(s)+2, p);
    return kk_os_realpath(s, ctx);
  }
  else {
    // basename, try to prefix with all entries in PATH
    kk_string_t s = kk_os_searchpathx(getenv("PATH"), p, ctx);
    if (kk_string_is_empty_borrow(s)) s = kk_os_realpathx(p,ctx);
    return s;
  }
}

#if defined(_WIN32)
#include <Windows.h>
kk_decl_export kk_string_t kk_os_app_path(kk_context_t* ctx) { 
  char buf[264];
  DWORD len = GetModuleFileNameA(NULL, buf, 264);
  buf[min(len,263)] = 0;  
  if (len < 264) { 
    // success
    return kk_string_alloc_dup(buf, ctx);
  }
  else {
    // not enough space in the buffer, try again with larger buffer
    size_t slen = kk_os_path_max();
    kk_string_t s = kk_string_alloc_len(slen, NULL, ctx);
    len = GetModuleFileNameA(NULL, (char*)kk_string_cbuf_borrow(s), (DWORD)slen+1);
    if (len > slen) {
      // failed again, use fall back
      kk_string_drop(s, ctx);
      return kk_os_app_path_generic(ctx);
    }
    return kk_string_adjust_length(s, len, ctx);
  }  
}
#elif defined(__MACH__)
#include <errno.h>
#include <libproc.h>
#include <unistd.h>
kk_string_t kk_os_app_path(kk_context_t* ctx) {
  pid_t pid = getpid();
  kk_string_t s = kk_string_alloc_len(PROC_PIDPATHINFO_MAXSIZE, NULL, ctx);
  int ret = proc_pidpath(pid, (char*)kk_string_cbuf_borrow(s), PROC_PIDPATHINFO_MAXSIZE /* must be this value or the call fails */);
  if (ret > 0) {
    // failed, use fall back
    kk_string_drop(s, ctx);
    return kk_os_app_path_generic(ctx);
  }
  else {
    return kk_string_adjust_length(s, strlen(kk_string_cbuf_borrow(s)), ctx);
  }
}

#elif defined(__linux__) || defined(__CYGWIN__) || defined(__sun) || defined(__DragonFly__) || defined(__NetBSD__)
#if defined(__sun)
#define KK_PROC_SELF "/proc/self/path/a.out"
#elif defined(__NetBSD__)
#define KK_PROC_SELF "/proc/curproc/exe"
#elif defined(__DragonFly__)
#define KK_PROC_SELF "/proc/curproc/file"
#else
#define KK_PROC_SELF "/proc/self/exe"
#endif

kk_string_t kk_os_app_path(kk_context_t* ctx) {
  kk_string_t s = kk_os_realpathx(KK_PROC_SELF,ctx);
  if (strcmp(kk_string_cbuf_borrow(s), KK_PROC_SELF)==0) {
    // failed? try generic search
    return kk_os_app_path_generic(ctx);
  }
  else {
    return s;
  }
}

#else
#pragma message("using generic application path detection")
kk_string_t kk_os_app_path(kk_context_t* ctx) {
  return kk_os_app_path_generic(ctx);
}
#endif

/*--------------------------------------------------------------------------------------------------
  Misc.
--------------------------------------------------------------------------------------------------*/

kk_decl_export kk_string_t kk_os_path_sep(kk_context_t* ctx) {
  char pathsep[2] = { KK_PATH_SEP, 0 };
  return kk_string_alloc_dup(pathsep, ctx);
}

kk_decl_export kk_string_t kk_os_dir_sep(kk_context_t* ctx) {
  char dirsep[2] = { KK_DIR_SEP, 0 };
  return kk_string_alloc_dup(dirsep, ctx);
}

kk_decl_export kk_string_t kk_os_home_dir(kk_context_t* ctx) {
  const char* h = getenv("HOME");
  if (h!=NULL) return kk_string_alloc_dup(h, ctx);
#ifdef _WIN32
  const char* hd = getenv("HOMEDRIVE");
  const char* hp = getenv("HOMEPATH");
  if (hd!=NULL && hp!=NULL) {
    kk_string_t s = kk_string_alloc_len(strlen(hd) + strlen(hp), hd, ctx);
    strcat((char*)kk_string_cbuf_borrow(s), hp);
    return s;
  }
#endif      
  return kk_string_alloc_dup(".", ctx);
}

kk_decl_export kk_string_t kk_os_temp_dir(kk_context_t* ctx) {
  const char* tmp = getenv("TEMP");
  if (tmp!=NULL) return kk_string_alloc_dup(tmp, ctx);
  tmp = getenv("TEMPDIR");
  if (tmp!=NULL) return kk_string_alloc_dup(tmp, ctx);
#ifdef _WIN32
  const char* ad = getenv("LOCALAPPDATA");
  if (ad!=NULL) {
    kk_string_t s = kk_string_alloc_len(strlen(ad) + 5, ad, ctx);
    strcat((char*)kk_string_cbuf_borrow(s), "\\Temp");
    return s;
  }
#endif      
#ifdef _WIN32
  return kk_string_alloc_dup("c:\\tmp", ctx);
#else
  return kk_string_alloc_dup("/tmp", ctx);
#endif
}