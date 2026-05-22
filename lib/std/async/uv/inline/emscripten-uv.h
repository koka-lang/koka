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
  XX(EIO, "i/o error")                                                        \
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
  UV_ERRNO_MIN = 1000,
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

typedef void (*uv_check_cb)(uv_check_t* handle);
typedef void (*uv_close_cb)(uv_handle_t* handle);
typedef void (*uv_walk_cb)(uv_handle_t* handle, void* arg);
typedef void (*uv_timer_cb)(uv_timer_t* handle);


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

void uv_ref(uv_handle_t*);
void uv_unref(uv_handle_t*);
void uv_close(uv_handle_t* handle, uv_close_cb close_cb);

struct uv_loop_s {
  int64_t refcount;
};

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

#endif
