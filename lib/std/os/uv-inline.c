#include "std_os_uv.h"

// UV allocator helpers, getting thread local context

static inline void* kk_malloc_ctx(size_t size) {
  return kk_malloc(size, kk_get_context());
}

static inline void* kk_realloc_ctx(void* p, size_t size) {
  return kk_realloc(p, size, kk_get_context());
}

static inline void* kk_calloc_ctx(size_t count, size_t size) {
  void* p = kk_malloc(count*size, kk_get_context());
  kk_memset(p, 0, count*size);
  return p;
}

static inline void kk_free_ctx(void* p) {
  kk_free(p, kk_get_context());
}

static void kk_uv_loop_init(kk_context_t* _ctx) {
  uv_loop_t* loop = kk_malloc(sizeof(uv_loop_t), kk_get_context());
  uv_replace_allocator(kk_malloc_ctx, kk_realloc_ctx, kk_calloc_ctx, kk_free_ctx);
  uv_loop_init(loop);
  kk_context_t* ctx = loop->data = kk_get_context();
  ctx->loop = loop;
}

void kk_uv_loop_run(kk_context_t* _ctx){
  // Run the event loop after the initial startup of the program
  int ret = uv_run(uvloop(), UV_RUN_DEFAULT);
  if (ret != 0){
    kk_info_message("Event loop closed with status %s", uv_err_name(ret));
  }
}

static void kk_uv_loop_close(kk_context_t* _ctx) {
  uv_loop_close(uvloop());
  kk_free(uvloop(), _ctx);
}

static void kk_uv_handle_close_callback(uv_handle_t* handle){
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)handle->data;
  kk_function_t callback = wrapper->callback;
  kk_function_call(void, (kk_function_t, kk_context_t*), callback, (callback, _ctx), _ctx);  
  kk_free(handle, _ctx);
  kk_free(wrapper, _ctx);
}

static void kk_uv_close(kk_std_os_uv__uvHandle handle, kk_function_t fun, kk_context_t* _ctx) {
  kk_uv_callback_t* cb = kk_new_uv_callback(fun, (uv_handle_t*)handle.internal, _ctx);
  return uv_close((uv_handle_t*)handle.internal, kk_uv_handle_close_callback);
}

static kk_std_os_uv__uvStatusCode kk_uv_status_to_status_code(int32_t status,
                                                       kk_context_t *_ctx) {
  switch (status) {
  case 0:
    return kk_std_os_uv_UV__OK;
  case UV_E2BIG:
    return kk_std_os_uv_UV__E2BIG;
  case UV_EACCES:
    return kk_std_os_uv_UV__EACCES;
  case UV_EADDRINUSE:
    return kk_std_os_uv_UV__EADDRINUSE;
  case UV_EADDRNOTAVAIL:
    return kk_std_os_uv_UV__EADDRNOTAVAIL;
  case UV_EAFNOSUPPORT:
    return kk_std_os_uv_UV__EAFNOSUPPORT;
  case UV_EAGAIN:
    return kk_std_os_uv_UV__EAGAIN;
  case UV_EAI_ADDRFAMILY:
    return kk_std_os_uv_UV__EAI__ADDRFAMILY;
  case UV_EAI_AGAIN:
    return kk_std_os_uv_UV__EAI__AGAIN;
  case UV_EAI_BADFLAGS:
    return kk_std_os_uv_UV__EAI__BADFLAGS;
  case UV_EAI_BADHINTS:
    return kk_std_os_uv_UV__EAI__BADHINTS;
  case UV_EAI_CANCELED:
    return kk_std_os_uv_UV__EAI__CANCELED;
  case UV_EAI_FAIL:
    return kk_std_os_uv_UV__EAI__FAIL;
  case UV_EAI_FAMILY:
    return kk_std_os_uv_UV__EAI__FAMILY;
  case UV_EAI_MEMORY:
    return kk_std_os_uv_UV__EAI__MEMORY;
  case UV_EAI_NODATA:
    return kk_std_os_uv_UV__EAI__NODATA;
  case UV_EAI_NONAME:
    return kk_std_os_uv_UV__EAI__NONAME;
  case UV_EAI_OVERFLOW:
    return kk_std_os_uv_UV__EAI__OVERFLOW;
  case UV_EAI_PROTOCOL:
    return kk_std_os_uv_UV__EAI__PROTOCOL;
  case UV_EAI_SERVICE:
    return kk_std_os_uv_UV__EAI__SERVICE;
  case UV_EAI_SOCKTYPE:
    return kk_std_os_uv_UV__EAI__SOCKTYPE;
  case UV_EALREADY:
    return kk_std_os_uv_UV__EALREADY;
  case UV_EBADF:
    return kk_std_os_uv_UV__EBADF;
  case UV_EBUSY:
    return kk_std_os_uv_UV__EBUSY;
  case UV_ECANCELED:
    return kk_std_os_uv_UV__ECANCELED;
  case UV_ECHARSET:
    return kk_std_os_uv_UV__ECHARSET;
  case UV_ECONNABORTED:
    return kk_std_os_uv_UV__ECONNABORTED;
  case UV_ECONNREFUSED:
    return kk_std_os_uv_UV__ECONNREFUSED;
  case UV_ECONNRESET:
    return kk_std_os_uv_UV__ECONNRESET;
  case UV_EDESTADDRREQ:
    return kk_std_os_uv_UV__EDESTADDRREQ;
  case UV_EEXIST:
    return kk_std_os_uv_UV__EEXIST;
  case UV_EFAULT:
    return kk_std_os_uv_UV__EFAULT;
  case UV_EFBIG:
    return kk_std_os_uv_UV__EFBIG;
  case UV_EHOSTUNREACH:
    return kk_std_os_uv_UV__EHOSTUNREACH;
  case UV_EINTR:
    return kk_std_os_uv_UV__EINTR;
  case UV_EINVAL:
    return kk_std_os_uv_UV__EINVAL;
  case UV_EIO:
    return kk_std_os_uv_UV__EIO;
  case UV_EISCONN:
    return kk_std_os_uv_UV__EISCONN;
  case UV_EISDIR:
    return kk_std_os_uv_UV__EISDIR;
  case UV_ELOOP:
    return kk_std_os_uv_UV__ELOOP;
  case UV_EMFILE:
    return kk_std_os_uv_UV__EMFILE;
  case UV_EMSGSIZE:
    return kk_std_os_uv_UV__EMSGSIZE;
  case UV_ENAMETOOLONG:
    return kk_std_os_uv_UV__ENAMETOOLONG;
  case UV_ENETDOWN:
    return kk_std_os_uv_UV__ENETDOWN;
  case UV_ENETUNREACH:
    return kk_std_os_uv_UV__ENETUNREACH;
  case UV_ENFILE:
    return kk_std_os_uv_UV__ENFILE;
  case UV_ENOBUFS:
    return kk_std_os_uv_UV__ENOBUFS;
  case UV_ENODEV:
    return kk_std_os_uv_UV__ENODEV;
  case UV_ENOENT:
    return kk_std_os_uv_UV__ENOENT;
  case UV_ENOMEM:
    return kk_std_os_uv_UV__ENOMEM;
  case UV_ENONET:
    return kk_std_os_uv_UV__ENONET;
  case UV_ENOPROTOOPT:
    return kk_std_os_uv_UV__ENOPROTOOPT;
  case UV_ENOSPC:
    return kk_std_os_uv_UV__ENOSPC;
  case UV_ENOSYS:
    return kk_std_os_uv_UV__ENOSYS;
  case UV_ENOTCONN:
    return kk_std_os_uv_UV__ENOTCONN;
  case UV_ENOTDIR:
    return kk_std_os_uv_UV__ENOTDIR;
  case UV_ENOTEMPTY:
    return kk_std_os_uv_UV__ENOTEMPTY;
  case UV_ENOTSOCK:
    return kk_std_os_uv_UV__ENOTSOCK;
  case UV_ENOTSUP:
    return kk_std_os_uv_UV__ENOTSUP;
  case UV_EOVERFLOW:
    return kk_std_os_uv_UV__EOVERFLOW;
  case UV_EPERM:
    return kk_std_os_uv_UV__EPERM;
  case UV_EPIPE:
    return kk_std_os_uv_UV__EPIPE;
  case UV_EPROTO:
    return kk_std_os_uv_UV__EPROTO;
  case UV_EPROTONOSUPPORT:
    return kk_std_os_uv_UV__EPROTONOSUPPORT;
  case UV_EPROTOTYPE:
    return kk_std_os_uv_UV__EPROTOTYPE;
  case UV_ERANGE:
    return kk_std_os_uv_UV__ERANGE;
  case UV_EROFS:
    return kk_std_os_uv_UV__EROFS;
  case UV_ESHUTDOWN:
    return kk_std_os_uv_UV__ESHUTDOWN;
  case UV_ESPIPE:
    return kk_std_os_uv_UV__ESPIPE;
  case UV_ESRCH:
    return kk_std_os_uv_UV__ESRCH;
  case UV_ETIMEDOUT:
    return kk_std_os_uv_UV__ETIMEDOUT;
  case UV_ETXTBSY:
    return kk_std_os_uv_UV__ETXTBSY;
  case UV_EXDEV:
    return kk_std_os_uv_UV__EXDEV;
  case UV_UNKNOWN:
    return kk_std_os_uv_UV__UNKNOWN;
  case UV_EOF:
    return kk_std_os_uv_UV__EOF;
  case UV_ENXIO:
    return kk_std_os_uv_UV__ENXIO;
  case UV_EMLINK:
    return kk_std_os_uv_UV__EMLINK;
  case UV_ENOTTY:
    return kk_std_os_uv_UV__ENOTTY;
  case UV_EFTYPE:
    return kk_std_os_uv_UV__EFTYPE;
  case UV_EILSEQ:
    return kk_std_os_uv_UV__EILSEQ;
  case UV_ESOCKTNOSUPPORT:
    return kk_std_os_uv_UV__ESOCKTNOSUPPORT;
  default:
    return kk_std_os_uv_UV__UNKNOWN;
    // case UV_EUNACH:
    // return kk_std_os_uv_UV__EUNATCH;
  }
}

kk_std_core__error kk_async_error_from_errno( int err, kk_context_t* _ctx ) {  
  kk_std_os_uv__uvStatusCode code = kk_uv_status_to_status_code(err, _ctx);
  kk_string_t msg = kk_std_os_uv_message(code, _ctx);
  return kk_std_core__new_Error( kk_std_core__new_Exception( msg, kk_std_os_uv__new_AsyncExn(kk_reuse_null, 0, code, _ctx), _ctx), _ctx );  
}

uv_buf_t* kk_bytes_list_to_uv_buffs(kk_std_core__list buffs, int* size, kk_context_t* _ctx){
  kk_std_core__list_dup(buffs, _ctx);
  kk_integer_t klist_len = kk_std_core_length_1(buffs, _ctx);
  int list_len = kk_integer_clamp32(klist_len, _ctx);
  uv_buf_t* uv_buffs = kk_malloc(sizeof(uv_buf_t) * list_len, _ctx);
  kk_std_core__list list = buffs;
  for (int i = 0; i < list_len; i++){
    struct kk_std_core_Cons* cons = kk_std_core__as_Cons(list, _ctx);
    kk_bytes_t bytes = kk_bytes_unbox(cons->head);
    kk_ssize_t len;
    const char* chars = kk_bytes_cbuf_borrow(bytes, &len, _ctx);
    uv_buffs[i].base = kk_malloc(len, _ctx);
    kk_memcpy(uv_buffs[i].base, chars, len);
    uv_buffs[i].len = len;
    list = cons->tail;
  }
  *size = list_len;
  return uv_buffs;
}

uv_buf_t* kk_bytes_to_uv_buffs(kk_bytes_t bytes, kk_context_t* _ctx){
  uv_buf_t* uv_buffs = kk_malloc(sizeof(uv_buf_t) * 1, _ctx);
  kk_ssize_t len;
  const char* chars = kk_bytes_cbuf_borrow(bytes, &len, _ctx);
  uv_buffs[0].base = kk_malloc(sizeof(char)*len, _ctx);
  kk_memcpy(uv_buffs[0].base, chars, len);
  uv_buffs[0].len = len;
  kk_bytes_drop(bytes, _ctx);
  return uv_buffs;
}
