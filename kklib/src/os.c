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


static bool kk_os_is_directory_cstr(const char* path);

/*--------------------------------------------------------------------------------------------------
  Text files
--------------------------------------------------------------------------------------------------*/

kk_decl_export int kk_os_read_text_file(kk_string_t path, kk_string_t* result, kk_context_t* ctx)
{
  kk_string_t s = kk_string_empty();
  *result = s;
  FILE* f = fopen(kk_string_cbuf_borrow(path), "rb");
  kk_string_drop(path, ctx);

  // find length
  if (f==NULL) goto fail;
  if (fseek(f, 0, SEEK_END) != 0) goto fail;
  long fsize = ftell(f);
  if (fsize<0) goto fail;
  if (fseek(f, 0, SEEK_SET) != 0) goto fail;  // rewind

  // pre-allocate and read at most length
  s = kk_string_alloc_buf(fsize, ctx);
  size_t nread = fread((char*)kk_string_cbuf_borrow(s), 1, fsize, f);
  if (ferror(f)) goto fail;
  if (nread < (size_t)fsize) { kk_string_adjust_length(s, nread, ctx); }
  fclose(f); f = NULL;

  // TODO: validate UTF8 to UTF8
  *result = s;
  return 0;

fail:
  kk_string_drop(s, ctx);
  if (f != NULL) fclose(f);
  return (errno != 0 ? errno : -1);
}

kk_decl_export int kk_os_write_text_file(kk_string_t path, kk_string_t content, kk_context_t* ctx)
{
  FILE* f = fopen(kk_string_cbuf_borrow(path), "wb");
  kk_string_drop(path, ctx);
  if (f==NULL) goto fail;
  size_t len = kk_string_len_borrow(content);
  if (len > 0) {
    size_t nwritten = fwrite(kk_string_cbuf_borrow(content), 1, len, f);
    if (nwritten < len) goto fail;
  }
  fclose(f); f = NULL;
  kk_string_drop(content,ctx);
  return 0;

fail:
  kk_string_drop(content, ctx);
  if (f != NULL) fclose(f);
  return (errno != 0 ? errno : -1);
}



/*--------------------------------------------------------------------------------------------------
  Directories
--------------------------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------------------------
  mkdir
--------------------------------------------------------------------------------------------------*/

#if defined(_MSC_VER) || defined(__MINGW32__)
#include <direct.h>
#define os_mkdir(p,m)  _mkdir(p)
#else
#include <sys/types.h>
#include <sys/stat.h>
#define os_mkdir(p,m)  mkdir(p,m)
#endif


kk_decl_export int kk_os_ensure_dir(kk_string_t path, int mode, kk_context_t* ctx) 
{
  int err = 0;
  if (mode < 0) {
#if defined(S_IRWXU)
    mode = S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH;
#else
    mode = 0755;
#endif
  }
  path = kk_string_copy(path, ctx); // copy so we can mutate
  char* cpath = (char*)kk_string_cbuf_borrow(path);
  char* p = cpath;
  do {
    char c = *p;
    if (c == 0 || c == '/' || c == '\\') {
      *p = 0;
      if (cpath[0]!=0) {
	if (!kk_os_is_directory_cstr(cpath)) {
          int res = os_mkdir(cpath, mode);
          if (res != 0 && errno != EEXIST) {
            err = errno;
	  }
	}
      }
      *p = c;
    }
  } while (err == 0 && *p++ != 0);

  kk_string_drop(path, ctx);
  return err;
}

/*--------------------------------------------------------------------------------------------------
  Copy File
--------------------------------------------------------------------------------------------------*/

#if defined(WIN32) || defined(__MINGW32__)
#include <Windows.h>
static int os_copy_file(const char* from, const char* to, bool preserve_mtime) {
  KK_UNUSED(preserve_mtime);
  if (!CopyFileA(from, to, FALSE)) {
    DWORD err = GetLastError();
    if (err == ERROR_FILE_NOT_FOUND) return ENOENT;
    else if (err == ERROR_ACCESS_DENIED) return EPERM;
    else if (err == ERROR_PATH_NOT_FOUND) return ENOTDIR;
    else return EINVAL;
  }
  else {
    return 0;
  }
}
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>

#if defined(__APPLE__) || defined(__FreeBSD__)
#include <copyfile.h>
#else
#include <sys/sendfile.h>
#endif

static int os_copy_file(const char* from, const char* to, bool preserve_mtime) {
  int inp, out;
  struct stat finfo = { 0 };
  if ((inp = open(from, O_RDONLY)) == -1) {
    return errno;
  }
  if (fstat(inp, &finfo) < 0) { 
    close(inp);
    return errno;
  }      
  if ((out = creat(to, finfo.st_mode)) == -1) {  // keep the mode
    close(inp);
    return errno;
  }

  int err = 0;
#if defined(__APPLE__) || defined(__FreeBSD__)
  if (fcopyfile(inp, out, 0, COPYFILE_ALL) != 0) {
    err = errno;
  }
#else
  // Linux
  off_t copied = 0;
  if (sendfile(out, inp, &copied, finfo.st_size) == -1) {
    err = errno;
  }
#endif

  close(inp);
  close(out);

  // maintain access/mod time
  if (preserve_mtime) {
    struct timeval times[2];
    times[0].tv_sec  = finfo.st_atim.tv_sec;
    times[0].tv_usec = finfo.st_atim.tv_nsec / 1000;
    times[1].tv_sec  = finfo.st_mtim.tv_sec;
    times[1].tv_usec = finfo.st_mtim.tv_nsec / 1000;
    utimes(to, times);
  }
  return err;
}
#endif

kk_decl_export int  kk_os_copy_file(kk_string_t from, kk_string_t to, bool preserve_mtime, kk_context_t* ctx) {
  int err = os_copy_file(kk_string_cbuf_borrow(from), kk_string_cbuf_borrow(to), preserve_mtime );
  kk_string_drop(from,ctx);
  kk_string_drop(to,ctx);
  return err;
}


/*--------------------------------------------------------------------------------------------------
  Stat directory
--------------------------------------------------------------------------------------------------*/
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#define stat    _stat
#define S_IFDIR _S_IFDIR
#define S_IFREG _S_IFREG
#endif

static bool kk_os_is_directory_cstr(const char* path) {
  struct stat finfo = { 0 };
  if (stat(path, &finfo) != 0) return false;
  return ((finfo.st_mode & S_IFDIR) != 0);
}

kk_decl_export bool kk_os_is_directory(kk_string_t path, kk_context_t* ctx) {
  bool is_dir = kk_os_is_directory_cstr(kk_string_cbuf_borrow(path));
  kk_string_drop(path,ctx);
  return is_dir;
}

static bool kk_os_is_file_cstr(const char* path) {
  struct stat finfo = { 0 };
  if (stat(path, &finfo) != 0) return false;
  return ((finfo.st_mode & S_IFREG) != 0);
}

kk_decl_export bool kk_os_is_file(kk_string_t path, kk_context_t* ctx) {
  bool is_file = kk_os_is_file_cstr(kk_string_cbuf_borrow(path));
  kk_string_drop(path, ctx);
  return is_file;
}

/*--------------------------------------------------------------------------------------------------
  List directory
--------------------------------------------------------------------------------------------------*/

#if defined(WIN32)
#include <io.h>
#define dir_cursor intptr_t
#define dir_entry  struct _finddata_t
static bool os_findfirst(const char* path, dir_cursor* d, dir_entry* entry, int* err, kk_context_t* ctx) {
  kk_string_t s = kk_string_alloc_buf(strlen(path) + 2, ctx);
  char* buf = (char*)kk_string_buf_borrow(s);
  strcpy(buf, path);
  strcat(buf, "/*");
  *d = _findfirst(buf, entry);
  kk_string_drop(s,ctx);
  bool ok = (*d != -1);
  *err = (ok ? 0 : errno);
  return ok;
}
static bool os_findnext(dir_cursor d, dir_entry* entry, int* err) {
  bool ok = (_findnext(d, entry) == 0);
  *err = (ok || errno == ENOENT ? 0 : errno);
  return ok;
}
static void os_findclose(dir_cursor d) {
  _findclose(d);
}
static const char* os_direntry_name(dir_entry* entry) {
  return entry->name;
}

#else
#include <sys/types.h>
#include <dirent.h>
#define dir_cursor DIR*
#define dir_entry  struct dirent*
static bool os_findnext(dir_cursor d, dir_entry* entry, int* err) {
  *entry = readdir(d);
  *err = (*entry != NULL || errno == ENOENT ? 0 : errno);
  return (*entry != NULL);
}
static bool os_findfirst(const char* path, dir_cursor* d, dir_entry* entry, int* err, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  *d = opendir(path);
  if (*d == NULL) {
    *err = errno;
    return false;
  }
  else {
    return os_findnext(*d, entry, err);
  }
}
static void os_findclose(dir_cursor d) {
  closedir(d);
}
static const char* os_direntry_name(dir_entry* entry) {
  return (*entry)->d_name;
}
#endif

kk_decl_export int kk_os_list_directory(kk_string_t dir, kk_vector_t* contents, kk_context_t* ctx) {
  dir_cursor d;
  dir_entry entry;
  int err;
  bool ok = os_findfirst(kk_string_cbuf_borrow(dir), &d, &entry, &err, ctx);
  kk_string_drop(dir,ctx);
  if (!ok) {
    *contents = kk_vector_empty();
    return err;
  }

  size_t count = 0;
  size_t len = 100;
  kk_vector_t vec = kk_vector_alloc(len, kk_integer_box(kk_integer_from_small(0)), ctx);
  
  do {
    const char* cname = os_direntry_name(&entry);
    if (cname != NULL && strcmp(cname, ".") != 0 && strcmp(cname, "..") != 0) {
      // push name
      if (count == len) {
        // realloc vector
        const size_t newlen = (len > 1000 ? len + 1000 : 2*len);
        vec = kk_vector_realloc(vec, newlen, kk_integer_box(kk_integer_from_small(0)), ctx);
        len = newlen;
      }
      (kk_vector_buf(vec, NULL))[count] = kk_string_box(kk_string_alloc_dup(cname, ctx));
      count++;
    }
  } while (os_findnext(d, &entry, &err));
  os_findclose(d);
  
  *contents = kk_vector_realloc(vec, count, kk_box_null, ctx);
  return err;
}


/*--------------------------------------------------------------------------------------------------
  Run system command
--------------------------------------------------------------------------------------------------*/

#if defined(WIN32) || defined(__MINGW32__) 
#define popen  _popen
#define pclose _pclose
#define POPEN_READ  "rt"
#else
#define POPEN_READ  "r"
#endif

kk_decl_export int kk_os_run_command(kk_string_t cmd, kk_string_t* output, kk_context_t* ctx) {
  FILE* f = popen(kk_string_cbuf_borrow(cmd), POPEN_READ);
  kk_string_drop(cmd, ctx);
  if (f==NULL) return errno;
  kk_string_t out = kk_string_empty();
  char buf[1025];
  while (fgets(buf, 1024, f) != NULL) {
    buf[1024] = 0; // paranoia
    out = kk_string_cat_fromc(out, buf, ctx);
  }
  if (feof(f)) errno = 0;
  pclose(f);
  *output = out;
  return errno;
}

kk_decl_export int kk_os_run_system(kk_string_t cmd, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  int exitcode = system(kk_string_cbuf_borrow(cmd));
  kk_string_drop(cmd, ctx);
  return exitcode;
}



/*--------------------------------------------------------------------------------------------------
  Args
--------------------------------------------------------------------------------------------------*/

kk_decl_export kk_vector_t kk_os_get_argv(kk_context_t* ctx) {
  if (ctx->argc==0 || ctx->argv==NULL) return kk_vector_empty();
  kk_vector_t args = kk_vector_alloc(ctx->argc, kk_box_null, ctx);
  kk_box_t* buf = kk_vector_buf(args, NULL);
  for (size_t i = 0; i < ctx->argc; i++) {
    buf[i] = kk_string_box(kk_string_alloc_dup(ctx->argv[i], ctx));
  }
  return args;
}


#if defined _WIN32
#include <Windows.h>
kk_decl_export kk_vector_t kk_os_get_env(kk_context_t* ctx) {
  const LPCH env = GetEnvironmentStringsA();
  if (env==NULL) return kk_vector_empty();
  // first count the number of environment variables  (ends with two zeros)
  size_t count = 0;
  for (size_t i = 0; !(env[i]==0 && env[i+1]==0); i++) {
    if (env[i]==0) count++;
  }
  kk_vector_t v = kk_vector_alloc(count*2, kk_box_null, ctx);
  kk_box_t* buf = kk_vector_buf(v, NULL);
  const char* p = env;
  // copy the strings into the vector
  for(size_t i = 0; i < count; i++) {
    const char* pname = p;
    while (*p != '=' && *p != 0) { p++; }
    buf[2*i] = kk_string_box( kk_string_alloc_len((p - pname), pname, ctx) );
    p++; // skip '='
    const char* pvalue = p;
    while (*p != 0) { p++; }
    buf[2*i + 1] = kk_string_box(kk_string_alloc_len((p - pvalue), pvalue, ctx));
    p++;
  }
  FreeEnvironmentStringsA(env);
  return v;
}
#else
#if defined(__APPLE__) && defined(__has_include) && __has_include(<crt_externs.h>)
#include <crt_externs.h>
static char** kk_get_environ(void) {
  return (*_NSGetEnviron());
}
#else
// On Posix systems use `environ`
extern char** environ;
static char** kk_get_environ(void) {
  return environ;
}
#endif
kk_decl_export kk_vector_t kk_os_get_env(kk_context_t* ctx) {
  const char** env = (const char**)kk_get_environ();
  if (env==NULL) return kk_vector_empty();
  // first count the number of environment variables
  size_t count;
  for (count = 0; env[count]!=NULL; count++) { /* nothing */ }
  kk_vector_t v = kk_vector_alloc(count*2, kk_box_null, ctx);
  kk_box_t* buf = kk_vector_buf(v, NULL);
  // copy the strings into the vector
  for (size_t i = 0; i < count; i++) {
    const char* p = env[i];
    const char* pname = p;
    while (*p != '=' && *p != 0) { p++; }
    buf[2*i] = kk_string_box(kk_string_alloc_len((p - pname), pname, ctx));
    p++; // skip '='
    const char* pvalue = p;
    while (*p != 0) { p++; }
    buf[2*i + 1] = kk_string_box(kk_string_alloc_len((p - pvalue), pvalue, ctx));
  }
  return v;
}
#endif


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

static kk_string_t kk_os_realpath_cstr(const char* fname, kk_context_t* ctx);

#if defined(_WIN32)
#include <Windows.h>
static kk_string_t kk_os_realpath_cstr(const char* fname, kk_context_t* ctx) {
  char buf[264];
  DWORD res = GetFullPathNameA(fname, 264, buf, NULL);
  if (res >= 264 || res == 0) {  
    // path too long or failure
    // TODO: use GetFullPathNameW to allow longer file names?
    return kk_string_alloc_dup(fname, ctx);
  }
  else {
    return kk_string_alloc_dup(buf, ctx);
  }
}

#elif defined(__linux__) || defined(__CYGWIN__) || defined(__sun) || defined(unix) || defined(__unix__) || defined(__unix) || defined(__MACH__)
#include <limits.h>
static kk_string_t kk_os_realpath_cstr(const char* fname, kk_context_t* ctx) {
  char* rpath   = realpath(fname, NULL);
  kk_string_t s = kk_string_alloc_dup( (rpath!=NULL ? rpath : fname), ctx);
  free(rpath);
  return s;
}

#else
#pragma message("realpath ignored on this platform")
static kk_string_t kk_os_realpath_cstr(const char* fname, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return kk_string_alloc_dup(fname,ctx);
}
#endif

kk_decl_export kk_string_t kk_os_realpath(kk_string_t fname, kk_context_t* ctx) {
  kk_string_t p = kk_os_realpath_cstr(kk_string_cbuf_borrow(fname), ctx);
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
    if (kk_os_is_file_cstr(buf)) {
      s = kk_os_realpath_cstr(buf, ctx);
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
    return kk_os_realpath_cstr(p, ctx);
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
    if (kk_string_is_empty_borrow(s)) s = kk_os_realpath_cstr(p,ctx);
    return s;
  }
}

#if defined(_WIN32)
#include <Windows.h>
kk_decl_export kk_string_t kk_os_app_path(kk_context_t* ctx) {
  char buf[264];
  DWORD len = GetModuleFileNameA(NULL, buf, 264);
  buf[min(len,263)] = 0;
  if (len == 0) { 
    // fail, fall back
    return kk_os_app_path_generic(ctx);
  }
  else if (len < 264) {
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
    else {
      return kk_string_adjust_length(s, len, ctx);
    }
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
  kk_string_t s = kk_os_realpath_cstr(KK_PROC_SELF,ctx);
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
