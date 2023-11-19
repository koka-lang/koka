// #include "std_os_net.h"
#include "std_os_uv.h"

static struct sockaddr* to_sockaddr(kk_std_os_net__sockAddr addr, kk_context_t* _ctx){
  int p;
  if (kk_std_core_types__is_Just(addr.port, _ctx)){
    p = (int)kk_int32_unbox(addr.port._cons.Just.value, KK_BORROWED, _ctx);
  } else {
    p = 80;
  }
  if (kk_std_os_net__is_AF__INET(addr.family, _ctx)){
    struct sockaddr_in* addrIn = kk_malloc(sizeof(struct sockaddr_in), _ctx);
    kk_ssize_t len;
    const char* str = kk_string_cbuf_borrow(addr.data, &len, _ctx);
    uv_ip4_addr(str, p, addrIn);
    return (struct sockaddr*)addrIn;
  } else if (kk_std_os_net__is_AF__INET6(addr.family, _ctx)){
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

static kk_std_os_net__sockAddr to_kk_sockaddr(struct sockaddr* addr, kk_context_t* _ctx){
  enum kk_std_os_net__netFamily_e family = kk_std_os_net_AF__ANY;
  
  kk_std_core_types__maybe portMaybe;
  if (addr->sa_family == AF_INET){
    family = kk_std_os_net_AF__INET;
    portMaybe = kk_std_core_types__new_Just(kk_int32_box(((struct sockaddr_in*)addr)->sin_port, _ctx), _ctx);
  } else if (addr->sa_family == AF_INET6){
    family = kk_std_os_net_AF__INET6;
    portMaybe = kk_std_core_types__new_Just(kk_int32_box(((struct sockaddr_in6*)addr)->sin6_port, _ctx), _ctx);
  } else {
    portMaybe = kk_std_core_types__new_Nothing(_ctx);
  }
  char ip[50]= "";
  inet_ntop(addr->sa_family, &addr->sa_data[2], ip, sizeof(ip));

  kk_string_t ipStr = kk_string_alloc_from_qutf8(ip, _ctx);
  return kk_std_os_net__new_SockAddr(family, ipStr, portMaybe, _ctx);
}

static void kk_addrinfo_cb(uv_getaddrinfo_t* req, int status, struct addrinfo* res){
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* r = (kk_uv_callback_t*)req->data;
  kk_function_t f = r->callback;

  if (status < 0) {
    kk_info_message("Addr info callback returned error code %d %s\n", status, uv_strerror(status));
    kk_function_call(void, (kk_function_t, kk_std_core__list, kk_context_t*), f, (f, kk_std_core__new_Nil(_ctx), _ctx), _ctx);
    uv_freeaddrinfo(res);
    return;
  }

  kk_std_core__list list = kk_std_core__new_Nil(_ctx);
  for (struct addrinfo* p = res; p != NULL; p = p->ai_next) {
    enum kk_std_os_net__netFamily_e family = kk_std_os_net_AF__ANY;
    if (p->ai_family == AF_INET){
      family = kk_std_os_net_AF__INET;
    } else if (p->ai_family == AF_INET6){
      family = kk_std_os_net_AF__INET6;
    }
    enum kk_std_os_net__sockType_e sockType = kk_std_os_net_SOCK__ANY;
    if (p->ai_socktype == SOCK_DGRAM){
      sockType = kk_std_os_net_SOCK__DGRAM;
    } else if (p->ai_socktype == SOCK_STREAM){
      sockType = kk_std_os_net_SOCK__STREAM;
    }
    kk_string_t canonName = kk_string_empty();
    
    if (p->ai_canonname) {
      canonName = kk_string_alloc_from_qutf8(p->ai_canonname, _ctx);
    }

    kk_std_os_net__sockAddr addr = to_kk_sockaddr(p->ai_addr, _ctx);
    kk_std_os_net__addrInfo addrInfo = kk_std_os_net__new_AddrInfo(kk_reuse_null, 0, p->ai_flags, family, sockType, p->ai_protocol, addr, canonName, _ctx);
    kk_std_core__list head = kk_std_core__new_Cons(kk_reuse_null, 0, kk_std_os_net__addrInfo_box(addrInfo, _ctx), list, _ctx);
    list = head;
  }
  uv_freeaddrinfo(res);
  kk_function_call(void, (kk_function_t, kk_std_core__list, kk_context_t*), f, (f, list, _ctx), _ctx);
}


static void kk_get_addrinfo(kk_string_t node, kk_string_t service, kk_std_core_types__maybe hints, kk_function_t callback, kk_context_t* _ctx) {
  uv_getaddrinfo_t* req = kk_malloc(sizeof(uv_getaddrinfo_t), _ctx);
  kk_uv_callback_t* r = kk_malloc(sizeof(kk_uv_callback_t), _ctx);
  r->callback = callback;
  req->data = r;
  const char* nodeChars = kk_string_cbuf_borrow(node, NULL, _ctx);
  const char* serviceChars = kk_string_cbuf_borrow(service, NULL, _ctx);
  int result = uv_getaddrinfo(uvloop(), req, &kk_addrinfo_cb, nodeChars, serviceChars, NULL);
  if (result < 0) {
    kk_info_message("Addr info returned error code %d %s\n", result, uv_strerror(result));
  }
}

static kk_std_core__error kk_uv_tcp_init(kk_context_t* _ctx) {
  uv_tcp_t* tcp = kk_malloc(sizeof(uv_tcp_t), _ctx);
  int status = uv_tcp_init(uvloop(), tcp);

  if (status == 0){
    kk_std_os_net__uvTcp handle = kk_std_os_net__new_UvTcp((kk_std_core_types__intptr__t)tcp, _ctx);
    return kk_std_core__new_Ok(kk_std_os_net__uvTcp_box(handle, _ctx), _ctx);
  } else {
    kk_string_t msg = kk_string_alloc_from_qutf8(uv_strerror(status), _ctx);
    return kk_std_core__new_Error( kk_std_core__new_Exception(msg, kk_std_core__new_ExnSystem(kk_reuse_null, 0, kk_integer_from_int(status, _ctx), _ctx), _ctx), _ctx );
  }
}

static kk_std_core__error kk_uv_tcp_init_ex(int32_t flags, kk_context_t* _ctx) {
  uv_tcp_t* tcp = kk_malloc(sizeof(uv_tcp_t), _ctx);
  int status = uv_tcp_init_ex(uvloop(), tcp, (unsigned int)flags);
  if (status == 0){
    kk_std_os_net__uvTcp handle = kk_std_os_net__new_UvTcp((kk_std_core_types__intptr__t)tcp, _ctx);
    return kk_std_core__new_Ok(kk_std_os_net__uvTcp_box(handle, _ctx), _ctx);
  } else {
    kk_string_t msg = kk_string_alloc_from_qutf8(uv_strerror(status), _ctx);
    return kk_std_core__new_Error( kk_std_core__new_Exception(msg, kk_std_core__new_ExnSystem(kk_reuse_null, 0, kk_integer_from_int(status, _ctx), _ctx), _ctx), _ctx );
  }
}

static kk_std_os_uv__uvStatusCode kk_uv_tcp_open(kk_std_os_net__uvTcp handle, kk_std_os_net__uvOsSock sock, kk_context_t* _ctx) {
  return kk_std_os_uv_toStatusCode(uv_tcp_open((uv_tcp_t*)handle.internal, *((uv_os_sock_t*)sock.internal)), _ctx);
}

static kk_std_os_uv__uvStatusCode kk_uv_tcp_nodelay(kk_std_os_net__uvTcp handle, bool enable, kk_context_t* _ctx) {
  return kk_std_os_uv_toStatusCode(uv_tcp_nodelay((uv_tcp_t*)handle.internal, enable), _ctx);
}

static kk_std_os_uv__uvStatusCode kk_uv_tcp_keepalive(kk_std_os_net__uvTcp handle, bool enable, uint32_t delay, kk_context_t* _ctx) {
  return kk_std_os_uv_toStatusCode(uv_tcp_keepalive((uv_tcp_t*)handle.internal, enable, delay), _ctx);
}

static kk_std_os_uv__uvStatusCode kk_uv_tcp_simultaneous_accepts(kk_std_os_net__uvTcp handle, bool enable, kk_context_t* _ctx) {
  return kk_std_os_uv_toStatusCode(uv_tcp_simultaneous_accepts((uv_tcp_t*)handle.internal, enable), _ctx);
}

static kk_std_os_uv__uvStatusCode kk_uv_tcp_bind(kk_std_os_net__uvTcp handle, kk_std_os_net__sockAddr addr, uint32_t flags, kk_context_t* _ctx) {
  struct sockaddr* sockAddr = to_sockaddr(addr, _ctx);
  return kk_std_os_uv_toStatusCode(uv_tcp_bind((uv_tcp_t*)handle.internal, sockAddr, flags), _ctx);
}

static kk_std_core__error kk_uv_tcp_getsockname(kk_std_os_net__uvTcp handle, kk_context_t* _ctx) {
  struct sockaddr_storage sockAddr;
  int len;
  int status = uv_tcp_getsockname((uv_tcp_t*)handle.internal, (struct sockaddr*)&sockAddr, &len);
  if (status == 0) {
    kk_std_os_net__sockAddr addr = to_kk_sockaddr((struct sockaddr*)&sockAddr, _ctx);
    return kk_std_core__new_Ok(kk_std_os_net__sockAddr_box(addr, _ctx), _ctx);
  } else {
    kk_string_t msg = kk_string_alloc_from_qutf8(uv_strerror(status), _ctx);
    return kk_std_core__new_Error( kk_std_core__new_Exception(msg, kk_std_core__new_ExnSystem(kk_reuse_null, 0, kk_integer_from_int(status, _ctx), _ctx), _ctx), _ctx );
  }
}

static kk_std_core__error kk_uv_tcp_getpeername(kk_std_os_net__uvTcp handle, kk_context_t* _ctx) {
  struct sockaddr_storage sockAddr;
  int len;
  int status = uv_tcp_getpeername((uv_tcp_t*)handle.internal, (struct sockaddr*)&sockAddr, &len);
  if (status == 0) {
    kk_std_os_net__sockAddr addr = to_kk_sockaddr((struct sockaddr*)&sockAddr, _ctx);
    return kk_std_core__new_Ok(kk_std_os_net__sockAddr_box(addr, _ctx), _ctx);
  } else {
    kk_string_t msg = kk_string_alloc_from_qutf8(uv_strerror(status), _ctx);
    return kk_std_core__new_Error( kk_std_core__new_Exception(msg, kk_std_core__new_ExnSystem(kk_reuse_null, 0, kk_integer_from_int(status, _ctx), _ctx), _ctx), _ctx );
  }
}

static void kk_uv_tcp_connect_callback(uv_connect_t* req, int status) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  kk_function_call(void, (kk_function_t, kk_std_os_uv__uvStatusCode, kk_context_t*), callback, (callback, kk_std_os_uv_toStatusCode(status, _ctx), _ctx), _ctx);  
  kk_free(req, _ctx);
  kk_free(wrapper, _ctx);
}

static kk_std_os_uv__uvStatusCode kk_uv_tcp_connect(kk_std_os_net__uvTcp handle, kk_std_os_net__sockAddr addr, kk_function_t callback, kk_context_t* _ctx) {
  struct sockaddr* sockAddr = to_sockaddr(addr, _ctx);
  uv_connect_t* req = kk_malloc(sizeof(uv_connect_t), _ctx);
  kk_uv_callback_t* wrapper = kk_malloc(sizeof(kk_uv_callback_t), _ctx);
  req->data = wrapper;
  wrapper->callback = callback;
  return kk_std_os_uv_toStatusCode(uv_tcp_connect(req, (uv_tcp_t*)handle.internal, sockAddr, kk_uv_tcp_connect_callback), _ctx);
}