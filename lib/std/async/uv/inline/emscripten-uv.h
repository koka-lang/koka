/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#ifdef __EMSCRIPTEN__

#include <emscripten.h>

#define UV_ERRNO_MAP(XX) \
  XX(E2BIG, "argument list too long")                                         \
  XX(EACCES, "permission denied")                                             \
  XX(EADDRINUSE, "address already in use")                                    \
  XX(EADDRNOTAVAIL, "address not available")                                  \
  XX(EAFNOSUPPORT, "address family not supported")                            \
  XX(EAGAIN, "resource temporarily unavailable")                              \
  XX(EAI_ADDRFAMILY, "address family not supported")                          \
  XX(EAI_AGAIN, "temporary failure")                                          \
  XX(EAI_BADFLAGS, "bad ai_flags value")                                      \
  XX(EAI_BADHINTS, "invalid value for hints")                                 \
  XX(EAI_CANCELED, "request canceled")                                        \
  XX(EAI_FAIL, "permanent failure")                                           \
  XX(EAI_FAMILY, "ai_family not supported")                                   \
  XX(EAI_MEMORY, "out of memory")                                             \
  XX(EAI_NODATA, "no address")                                                \
  XX(EAI_NONAME, "unknown node or service")                                   \
  XX(EAI_OVERFLOW, "argument buffer overflow")                                \
  XX(EAI_PROTOCOL, "resolved protocol is unknown")                            \
  XX(EAI_SERVICE, "service not available for socket type")                    \
  XX(EAI_SOCKTYPE, "socket type not supported")                               \
  XX(EALREADY, "connection already in progress")                              \
  XX(EBADF, "bad file descriptor")                                            \
  XX(EBUSY, "resource busy or locked")                                        \
  XX(ECANCELED, "operation canceled")                                         \
  XX(ECHARSET, "invalid Unicode character")                                   \
  XX(ECONNABORTED, "software caused connection abort")                        \
  XX(ECONNREFUSED, "connection refused")                                      \
  XX(ECONNRESET, "connection reset by peer")                                  \
  XX(EDESTADDRREQ, "destination address required")                            \
  XX(EEXIST, "file already exists")                                           \
  XX(EFAULT, "bad address in system call argument")                           \
  XX(EFBIG, "file too large")                                                 \
  XX(EHOSTUNREACH, "host is unreachable")                                     \
  XX(EINTR, "interrupted system call")                                        \
  XX(EINVAL, "invalid argument")                                              \
  XX(EIO, "i/o error")\
  XX(EISCONN, "socket is already connected")                                  \
  XX(EISDIR, "illegal operation on a directory")                              \
  XX(ELOOP, "too many symbolic links encountered")                            \
  XX(EMFILE, "too many open files")                                           \
  XX(EMSGSIZE, "message too long")                                            \
  XX(ENAMETOOLONG, "name too long")                                           \
  XX(ENETDOWN, "network is down")                                             \
  XX(ENETUNREACH, "network is unreachable")                                   \
  XX(ENFILE, "file table overflow")                                           \
  XX(ENOBUFS, "no buffer space available")                                    \
  XX(ENODEV, "no such device")                                                \
  XX(ENOENT, "no such file or directory")                                     \
  XX(ENOMEM, "not enough memory")                                             \
  XX(ENONET, "machine is not on the network")                                 \
  XX(ENOPROTOOPT, "protocol not available")                                   \
  XX(ENOSPC, "no space left on device")                                       \
  XX(ENOSYS, "function not implemented")                                      \
  XX(ENOTCONN, "socket is not connected")                                     \
  XX(ENOTDIR, "not a directory")                                              \
  XX(ENOTEMPTY, "directory not empty")                                        \
  XX(ENOTSOCK, "socket operation on non-socket")                              \
  XX(ENOTSUP, "operation not supported on socket")                            \
  XX(EOVERFLOW, "value too large for defined data type")                      \
  XX(EPERM, "operation not permitted")                                        \
  XX(EPIPE, "broken pipe")                                                    \
  XX(EPROTO, "protocol error")                                                \
  XX(EPROTONOSUPPORT, "protocol not supported")                               \
  XX(EPROTOTYPE, "protocol wrong type for socket")                            \
  XX(ERANGE, "result too large")                                              \
  XX(EROFS, "read-only file system")                                          \
  XX(ESHUTDOWN, "cannot send after transport endpoint shutdown")              \
  XX(ESPIPE, "invalid seek")                                                  \
  XX(ESRCH, "no such process")                                                \
  XX(ETIMEDOUT, "connection timed out")                                       \
  XX(ETXTBSY, "text file is busy")                                            \
  XX(EXDEV, "cross-device link not permitted")                                \
  XX(UNKNOWN, "unknown error")                                                \
  XX(EOF, "end of file")                                                      \
  XX(ENXIO, "no such device or address")                                      \
  XX(EMLINK, "too many links")                                                \
  XX(EHOSTDOWN, "host is down")                                               \
  XX(EREMOTEIO, "remote I/O error")                                           \
  XX(ENOTTY, "inappropriate ioctl for device")                                \
  XX(EFTYPE, "inappropriate file type or format")                             \
  XX(EILSEQ, "illegal byte sequence")                                         \
  XX(ESOCKTNOSUPPORT, "socket type not supported")                            \
  XX(ENODATA, "no data available")                                            \
  XX(EUNATCH, "protocol driver not attached")                                 \
  XX(ENOEXEC, "exec format error")                                            \

#define UV_HANDLE_TYPE_MAP(XX)                                                \
  XX(ASYNC, async)                                                            \
  XX(CHECK, check)                                                            \
  XX(FS_EVENT, fs_event)                                                      \
  XX(FS_POLL, fs_poll)                                                        \
  XX(HANDLE, handle)                                                          \
  XX(IDLE, idle)                                                              \
  XX(NAMED_PIPE, pipe)                                                        \
  XX(POLL, poll)                                                              \
  XX(PREPARE, prepare)                                                        \
  XX(PROCESS, process)                                                        \
  XX(STREAM, stream)                                                          \
  XX(TCP, tcp)                                                                \
  XX(TIMER, timer)                                                            \
  XX(TTY, tty)                                                                \
  XX(UDP, udp)                                                                \
  XX(SIGNAL, signal)                                                          \

#define UV_REQ_TYPE_MAP(XX)                                                   \
  XX(REQ, req)                                                                \
  XX(CONNECT, connect)                                                        \
  XX(WRITE, write)                                                            \
  XX(SHUTDOWN, shutdown)                                                      \
  XX(UDP_SEND, udp_send)                                                      \
  XX(FS, fs)                                                                  \
  XX(WORK, work)                                                              \
  XX(GETADDRINFO, getaddrinfo)                                                \
  XX(GETNAMEINFO, getnameinfo)                                                \
  XX(RANDOM, random)                                                          \

#include <errno.h>
#if EDOM > 0
# define UV__ERR(x) (-(x))
#else
# define UV__ERR(x) (x)
#endif

typedef enum {
  UV_ERRNO_MIN = -1000,
  #define XX(code,_msg) UV_##code, /* = UV__ERR(code), */
  UV_ERRNO_MAP(XX)
  #undef XX
  UV_ERRNO_MAX
} uv_errno_t;


typedef enum {
  UV_UNKNOWN_HANDLE = 0,
  #define XX(uc, lc) UV_##uc,
  UV_HANDLE_TYPE_MAP(XX)
  #undef XX
  UV_FILE,
  UV_HANDLE_TYPE_MAX
} uv_handle_type;


typedef enum {
  UV_UNKNOWN_REQ = 0,
  #define XX(uc, lc) UV_##uc,
  UV_REQ_TYPE_MAP(XX)
  #undef XX  
  UV_REQ_TYPE_PRIVATE,
  UV_REQ_TYPE_MAX
} uv_req_type;


typedef enum {
  UV_LOOP_BLOCK_SIGNAL = 0,
  UV_METRICS_IDLE_TIME,
  UV_LOOP_USE_IO_URING_SQPOLL
} uv_loop_option;


typedef enum {
  UV_RUN_DEFAULT = 0,
  UV_RUN_ONCE,
  UV_RUN_NOWAIT
} uv_run_mode;


typedef struct uv_loop_s   uv_loop_t;
typedef struct uv_handle_s uv_handle_t;
typedef struct uv_timer_s  uv_timer_t;
typedef struct uv_timer_s  uv_check_t;   // check is emulated as a timer with timeout 0
typedef struct uv_loop_s uv_loop_t;
typedef struct uv_handle_s uv_handle_t;
typedef struct uv_dir_s uv_dir_t;
typedef struct uv_stream_s uv_stream_t;
typedef struct uv_tcp_s uv_tcp_t;
typedef struct uv_udp_s uv_udp_t;
typedef struct uv_pipe_s uv_pipe_t;
typedef struct uv_tty_s uv_tty_t;
typedef struct uv_poll_s uv_poll_t;
typedef struct uv_prepare_s uv_prepare_t;
typedef struct uv_idle_s uv_idle_t;
typedef struct uv_async_s uv_async_t;
typedef struct uv_process_s uv_process_t;
typedef struct uv_fs_event_s uv_fs_event_t;
typedef struct uv_fs_poll_s uv_fs_poll_t;
typedef struct uv_signal_s uv_signal_t;

typedef struct uv_req_s uv_req_t;
typedef struct uv_getaddrinfo_s uv_getaddrinfo_t;
typedef struct uv_getnameinfo_s uv_getnameinfo_t;
typedef struct uv_shutdown_s uv_shutdown_t;
typedef struct uv_write_s uv_write_t;
typedef struct uv_connect_s uv_connect_t;
typedef struct uv_udp_send_s uv_udp_send_t;
typedef struct uv_fs_s uv_fs_t;
typedef struct uv_work_s uv_work_t;
typedef struct uv_random_s uv_random_t;

typedef struct uv_env_item_s uv_env_item_t;
typedef struct uv_cpu_info_s uv_cpu_info_t;
typedef struct uv_interface_address_s uv_interface_address_t;
typedef struct uv_dirent_s uv_dirent_t;
typedef struct uv_passwd_s uv_passwd_t;
typedef struct uv_group_s uv_group_t;
typedef struct uv_utsname_s uv_utsname_t;
typedef struct uv_statfs_s uv_statfs_t;
typedef struct uv_metrics_s uv_metrics_t;

typedef void (*uv_check_cb)(uv_check_t* handle);
typedef void (*uv_close_cb)(uv_handle_t* handle);
typedef void (*uv_walk_cb)(uv_handle_t* handle, void* arg);
typedef void (*uv_timer_cb)(uv_timer_t* handle);
typedef void (*uv_fs_cb)(uv_fs_t* req);

// -----------------------------------
// ev loop
// -----------------------------------

typedef void* (*uv_malloc_func)(size_t size);
typedef void* (*uv_realloc_func)(void* ptr, size_t size);
typedef void* (*uv_calloc_func)(size_t count, size_t size);
typedef void (*uv_free_func)(void* ptr);

int  uv_replace_allocator(uv_malloc_func malloc_fun, uv_realloc_func realloc_fun, uv_calloc_func calloc_fun, uv_free_func free_fun);
const char* uv_strerror(int uverr);

int  uv_loop_init(uv_loop_t* loop);
int  uv_loop_close(uv_loop_t* loop);

int  uv_run(uv_loop_t*, uv_run_mode mode);
void uv_stop(uv_loop_t*);
void uv_walk(uv_loop_t* loop, uv_walk_cb walk_cb, void* arg);

void uv_ref(uv_handle_t*);
void uv_unref(uv_handle_t*);
void uv_close(uv_handle_t* handle, uv_close_cb close_cb);

struct uv_loop_s {
  int64_t refcount;
};


// -----------------------------------
// handles
// -----------------------------------

#define UV_HANDLE_FIELDS \
  void* data;            \
  uv_loop_t* loop;       \
  uv_handle_type type;   \
  /* private */          \
  uv_close_cb close_cb;  \
  bool active;           \
  bool hasref;         

struct uv_handle_s {
  UV_HANDLE_FIELDS
};

static inline bool uv_is_closing(const uv_handle_t* h) { return (h==NULL || h->close_cb != NULL); }
static inline bool uv_is_active(const uv_handle_t* h)  { return (h!=NULL && h->active);  }
static inline uv_handle_type uv_handle_get_type(const uv_handle_t* h) { return h->type; }
static inline uv_loop_t* uv_handle_get_loop(const uv_handle_t* h) { return h->loop; }

// -----------------------------------
// requests
// -----------------------------------

#define UV_REQ_FIELDS \
  void* data;         \
  /* read-only */     \
  uv_req_type type;
  
struct uv_req_s {
  UV_REQ_FIELDS
};

void uv_cancel(uv_req_t* req);

// -----------------------------------
// timer
// -----------------------------------

struct uv_timer_s {
  UV_HANDLE_FIELDS
  uv_timer_cb timer_cb;
  uint64_t    timeout;
  uint64_t    repeat;
  int         jstimer;
};

int uv_timer_init(uv_loop_t*, uv_timer_t* handle);
int uv_timer_start(uv_timer_t* handle, uv_timer_cb cb, uint64_t timeout, uint64_t repeat);
int uv_timer_stop(uv_timer_t* handle);

// emulate uv_check_t as a uv_timer_t with timeout 0
static inline int uv_check_init(uv_loop_t* loop, uv_check_t* h) { return uv_timer_init(loop,h); }
static inline int uv_check_start(uv_check_t* h, uv_check_cb cb) { return uv_timer_start(h,cb,0,0); }
static inline int uv_check_stop(uv_check_t* h) { return uv_timer_stop(h); }

// --------------------------
// fs
// --------------------------

typedef struct uv_buf_s {
  char* base;
  size_t len;
} uv_buf_t;

uv_buf_t uv_buf_init( char* p, size_t len );

typedef ssize_t uv_file;

typedef enum {
  UV_CLOCK_MONOTONIC,
  UV_CLOCK_REALTIME
} uv_clock_id;

typedef struct {
  long tv_sec;
  long tv_nsec;
} uv_timespec_t;

typedef struct {
  int64_t tv_sec;
  int32_t tv_nsec;
} uv_timespec64_t;

typedef struct {
  long tv_sec;
  long tv_usec;
} uv_timeval_t;

typedef struct {
  int64_t tv_sec;
  int32_t tv_usec;
} uv_timeval64_t;

typedef struct {
  uint64_t st_dev;
  uint64_t st_mode;
  uint64_t st_nlink;
  uint64_t st_uid;
  uint64_t st_gid;
  uint64_t st_rdev;
  uint64_t st_ino;
  uint64_t st_size;
  uint64_t st_blksize;
  uint64_t st_blocks;
  uint64_t st_flags;
  uint64_t st_gen;
  uv_timespec_t st_atim;
  uv_timespec_t st_mtim;
  uv_timespec_t st_ctim;
  uv_timespec_t st_birthtim;
} uv_stat_t;

typedef enum {
  UV_DIRENT_UNKNOWN,
  UV_DIRENT_FILE,
  UV_DIRENT_DIR,
  UV_DIRENT_LINK,
  UV_DIRENT_FIFO,
  UV_DIRENT_SOCKET,
  UV_DIRENT_CHAR,
  UV_DIRENT_BLOCK
} uv_dirent_type_t;

struct uv_dirent_s {
  const char* name;
  uv_dirent_type_t type;
};

typedef enum {
  UV_FS_UNKNOWN = -1,
  UV_FS_CUSTOM,
  UV_FS_OPEN,
  UV_FS_CLOSE,
  UV_FS_READ,
  UV_FS_WRITE,
  UV_FS_SENDFILE,
  UV_FS_STAT,
  UV_FS_LSTAT,
  UV_FS_FSTAT,
  UV_FS_FTRUNCATE,
  UV_FS_UTIME,
  UV_FS_FUTIME,
  UV_FS_ACCESS,
  UV_FS_CHMOD,
  UV_FS_FCHMOD,
  UV_FS_FSYNC,
  UV_FS_FDATASYNC,
  UV_FS_UNLINK,
  UV_FS_RMDIR,
  UV_FS_MKDIR,
  UV_FS_MKDTEMP,
  UV_FS_RENAME,
  UV_FS_SCANDIR,
  UV_FS_LINK,
  UV_FS_SYMLINK,
  UV_FS_READLINK,
  UV_FS_CHOWN,
  UV_FS_FCHOWN,
  UV_FS_REALPATH,
  UV_FS_COPYFILE,
  UV_FS_LCHOWN,
  UV_FS_OPENDIR,
  UV_FS_READDIR,
  UV_FS_CLOSEDIR,
  UV_FS_STATFS,
  UV_FS_MKSTEMP,
  UV_FS_LUTIME
} uv_fs_type;

struct uv_fs_s {
  UV_REQ_FIELDS
  uv_fs_type  fs_type;
  uv_loop_t*  loop;
  uv_fs_cb    cb;
  ssize_t     result;
  void*       ptr;
  const char* path;
  uv_stat_t   statbuf;   
};

struct uv_dir_s {
  uv_dirent_t* dirents;
  size_t       nentries;  
};

static inline uv_fs_type uv_fs_get_type(const uv_fs_t* req) { return req->fs_type; }
static inline ssize_t uv_fs_get_result(const uv_fs_t* req)  { return req->result;  };
static inline void* uv_fs_get_ptr(const uv_fs_t* req)       { return req->ptr; };
static inline const char* uv_fs_get_path(const uv_fs_t* req){ return req->path; };
static inline uv_stat_t* uv_fs_get_statbuf(uv_fs_t* req)    { return &req->statbuf; };

void uv_fs_req_cleanup(uv_fs_t* req);
int  uv_fs_close(uv_loop_t* loop, uv_fs_t* req, uv_file file, uv_fs_cb cb);
int  uv_fs_open(uv_loop_t* loop, uv_fs_t* req, const char* path, int flags, int mode, uv_fs_cb cb);
int  uv_fs_read(uv_loop_t* loop, uv_fs_t* req, uv_file file, const uv_buf_t bufs[], unsigned int nbufs, int64_t offset, uv_fs_cb cb);

#endif
