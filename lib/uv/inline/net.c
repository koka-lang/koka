// #include "std_os_net.h"
#include "uv_event_dash_loop.h"

static struct sockaddr* to_sockaddr(kk_uv_net__sock_addr addr, kk_context_t* _ctx){
  int p;
  if (kk_std_core_types__is_Just(addr.port, _ctx)){
    p = (int)kk_int32_unbox(addr.port._cons.Just.value, KK_BORROWED, _ctx);
  } else {
    p = 80;
  }
  if (kk_uv_net__is_AF__INET(addr.family, _ctx)){
    struct sockaddr_in* addrIn = kk_malloc(sizeof(struct sockaddr_in), _ctx);
    kk_ssize_t len;
    const char* str = kk_string_cbuf_borrow(addr.data, &len, _ctx);
    uv_ip4_addr(str, p, addrIn);
    return (struct sockaddr*)addrIn;
  } else if (kk_uv_net__is_AF__INET6(addr.family, _ctx)){
    struct sockaddr_in6* addrIn6 = kk_malloc(sizeof(struct sockaddr_in6), _ctx);
    kk_ssize_t len;
    const char* str = kk_string_cbuf_borrow(addr.data, &len, _ctx);
    uv_ip6_addr(str, p, addrIn6);
    return (struct sockaddr*)addrIn6;
  } else {
    // family = AF_UNSPEC;
    // Assume INET
    struct sockaddr_in* addrIn = kk_malloc(sizeof(struct sockaddr_in), _ctx);
    kk_ssize_t len;
    const char* str = kk_string_cbuf_borrow(addr.data, &len, _ctx);
    uv_ip4_addr(str, p, addrIn);
    return (struct sockaddr*)addrIn;
    // TODO: return error type error?
  }
}

static kk_uv_net__sock_addr to_kk_sockaddr(struct sockaddr* addr, kk_context_t* _ctx){
  enum kk_uv_net__net_family_e family = kk_uv_net_AF__ANY;
  
  kk_std_core_types__maybe portMaybe;
  if (addr->sa_family == AF_INET){
    family = kk_uv_net_AF__INET;
    portMaybe = kk_std_core_types__new_Just(kk_int32_box(((struct sockaddr_in*)addr)->sin_port, _ctx), _ctx);
  } else if (addr->sa_family == AF_INET6){
    family = kk_uv_net_AF__INET6;
    portMaybe = kk_std_core_types__new_Just(kk_int32_box(((struct sockaddr_in6*)addr)->sin6_port, _ctx), _ctx);
  } else {
    portMaybe = kk_std_core_types__new_Nothing(_ctx);
  }
  char ip[50]= "";
  inet_ntop(addr->sa_family, &addr->sa_data[2], ip, sizeof(ip));

  kk_string_t ipStr = kk_string_alloc_from_qutf8(ip, _ctx);
  return kk_uv_net__new_Sock_addr(family, ipStr, portMaybe, _ctx);
}

static void kk_addrinfo_cb(uv_getaddrinfo_t* req, int status, struct addrinfo* res){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = kk_function_from_ptr(req->data, _ctx);

  if (status < UV_OK) {
    kk_info_message("Addr info callback returned error code %d %s\n", status, uv_strerror(status));
    kk_function_call(void, (kk_function_t, kk_std_core_types__list, kk_context_t*), callback, (callback, kk_std_core_types__new_Nil(_ctx), _ctx), _ctx);
    uv_freeaddrinfo(res);
    return;
  }

  kk_std_core_types__list list = kk_std_core_types__new_Nil(_ctx);
  for (struct addrinfo* p = res; p != NULL; p = p->ai_next) {
    enum kk_uv_net__net_family_e family = kk_uv_net_AF__ANY;
    if (p->ai_family == AF_INET){
      family = kk_uv_net_AF__INET;
    } else if (p->ai_family == AF_INET6){
      family = kk_uv_net_AF__INET6;
    }
    enum kk_uv_net__sock_type_e sockType = kk_uv_net_SOCK__ANY;
    if (p->ai_socktype == SOCK_DGRAM){
      sockType = kk_uv_net_SOCK__DGRAM;
    } else if (p->ai_socktype == SOCK_STREAM){
      sockType = kk_uv_net_SOCK__STREAM;
    }
    kk_string_t canonName = kk_string_empty();
    
    if (p->ai_canonname) {
      canonName = kk_string_alloc_from_qutf8(p->ai_canonname, _ctx);
    }

    kk_uv_net__sock_addr addr = to_kk_sockaddr(p->ai_addr, _ctx);
    kk_uv_net__addr_info addrInfo = kk_uv_net__new_Addr_info(kk_reuse_null, 0, p->ai_flags, family, sockType, p->ai_protocol, addr, canonName, _ctx);
    kk_std_core_types__list head = kk_std_core_types__new_Cons(kk_reuse_null, 0, kk_uv_net__addr_info_box(addrInfo, _ctx), list, _ctx);
    list = head;
  }
  uv_freeaddrinfo(res);
  kk_function_call(void, (kk_function_t, kk_std_core_types__list, kk_context_t*), callback, (callback, list, _ctx), _ctx);
}


static kk_std_core_exn__error kk_get_addrinfo(kk_string_t node, kk_string_t service, kk_std_core_types__maybe hints, kk_function_t callback, kk_context_t* _ctx) {
  kk_new_req_cb(uv_getaddrinfo_t, req, callback)
  const char* nodeChars = kk_string_cbuf_borrow(node, NULL, _ctx);
  const char* serviceChars = kk_string_cbuf_borrow(service, NULL, _ctx);
  int result = uv_getaddrinfo(uvloop(), req, &kk_addrinfo_cb, nodeChars, serviceChars, NULL);
  kk_uv_check_err_drops(result, {kk_function_drop(callback, _ctx); kk_free(req, _ctx);})
}

static kk_std_core_exn__error kk_uv_tcp_init(kk_context_t* _ctx) {
  uv_tcp_t* tcp = kk_malloc(sizeof(uv_tcp_t), _ctx);
  int status = uv_tcp_init(uvloop(), tcp);
  kk_uv_check_return_err_drops(status, uv_handle_to_owned_kk_handle_box(tcp, kk_handle_free, net, tcp), {kk_free(tcp, _ctx);})
}

static kk_std_core_exn__error kk_uv_tcp_init_ex(int32_t flags, kk_context_t* _ctx) {
  uv_tcp_t* tcp = kk_malloc(sizeof(uv_tcp_t), _ctx);
  int status = uv_tcp_init_ex(uvloop(), tcp, (unsigned int)flags);
  kk_uv_check_return_err_drops(status, uv_handle_to_owned_kk_handle_box(tcp, kk_handle_free, net, tcp), {kk_free(tcp, _ctx);})
}

static kk_uv_utils__uv_status_code kk_uv_tcp_open(kk_uv_net__uv_tcp handle, kk_uv_net__uv_os_sock sock, kk_context_t* _ctx) {
  return kk_uv_utils_int_fs_status_code(uv_tcp_open(kk_owned_handle_to_uv_handle(uv_tcp_t, handle), *((uv_os_sock_t*)sock.internal)), _ctx);
}

static kk_uv_utils__uv_status_code kk_uv_tcp_nodelay(kk_uv_net__uv_tcp handle, bool enable, kk_context_t* _ctx) {
  return kk_uv_utils_int_fs_status_code(uv_tcp_nodelay(kk_owned_handle_to_uv_handle(uv_tcp_t, handle), enable), _ctx);
}

static kk_uv_utils__uv_status_code kk_uv_tcp_keepalive(kk_uv_net__uv_tcp handle, bool enable, uint32_t delay, kk_context_t* _ctx) {
  return kk_uv_utils_int_fs_status_code(uv_tcp_keepalive(kk_owned_handle_to_uv_handle(uv_tcp_t, handle), enable, delay), _ctx);
}

static kk_uv_utils__uv_status_code kk_uv_tcp_simultaneous_accepts(kk_uv_net__uv_tcp handle, bool enable, kk_context_t* _ctx) {
  return kk_uv_utils_int_fs_status_code(uv_tcp_simultaneous_accepts(kk_owned_handle_to_uv_handle(uv_tcp_t, handle), enable), _ctx);
}

static kk_uv_utils__uv_status_code kk_uv_tcp_bind(kk_uv_net__uv_tcp handle, kk_uv_net__sock_addr addr, uint32_t flags, kk_context_t* _ctx) {
  struct sockaddr* sockAddr = to_sockaddr(addr, _ctx);
  return kk_uv_utils_int_fs_status_code(uv_tcp_bind(kk_owned_handle_to_uv_handle(uv_tcp_t, handle), sockAddr, flags), _ctx);
}

static kk_std_core_exn__error kk_uv_tcp_getsockname(kk_uv_net__uv_tcp handle, kk_context_t* _ctx) {
  struct sockaddr_storage sockAddr;
  int len;
  int status = uv_tcp_getsockname(kk_owned_handle_to_uv_handle(uv_tcp_t, handle), (struct sockaddr*)&sockAddr, &len);
  kk_uv_check_return(status, kk_uv_net__sock_addr_box(to_kk_sockaddr((struct sockaddr*)&sockAddr, _ctx), _ctx))
}

static kk_std_core_exn__error kk_uv_tcp_getpeername(kk_uv_net__uv_tcp handle, kk_context_t* _ctx) {
  struct sockaddr_storage sockAddr;
  int len;
  int status = uv_tcp_getpeername(kk_owned_handle_to_uv_handle(uv_tcp_t, handle), (struct sockaddr*)&sockAddr, &len);
  kk_uv_check_return(status, kk_uv_net__sock_addr_box(to_kk_sockaddr((struct sockaddr*)&sockAddr, _ctx), _ctx))
}


static void kk_uv_tcp_connect_callback(uv_connect_t* req, int status) {
  kk_uv_get_callback(req, callback)
  kk_function_call(void, (kk_function_t, kk_uv_utils__uv_status_code, kk_context_t*), callback, (callback, kk_uv_utils_int_fs_status_code(status, _ctx), _ctx), _ctx);  
  kk_free(req, _ctx);
}

static kk_uv_utils__uv_status_code kk_uv_tcp_connect(kk_uv_net__uv_tcp handle, kk_uv_net__sock_addr addr, kk_function_t callback, kk_context_t* _ctx) {
  struct sockaddr* sockAddr = to_sockaddr(addr, _ctx);
  kk_new_req_cb(uv_connect_t, req, callback)
  kk_uv_check_status_drops(uv_tcp_connect(req, kk_owned_handle_to_uv_handle(uv_tcp_t, handle), sockAddr, kk_uv_tcp_connect_callback), {kk_function_drop(callback, _ctx);})
}