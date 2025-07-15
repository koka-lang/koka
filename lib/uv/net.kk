/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// A Koka wrapper around low level libuv networking functions
module uv/net

pub import std/num/int32
pub import std/core/bytes
pub import uv/event-loop
pub import uv/stream
import uv/utils

extern import
  c file "inline/net.c"

// Socket types
pub type sock-type
  SOCK_ANY
  SOCK_STREAM
  SOCK_DGRAM

// Internet address families
pub type net-family
  AF_INET
  AF_INET6
  AF_ANY

pub fun net-family/show(family: net-family): string
  match family
    AF_INET -> "AF_INET"
    AF_INET6 -> "AF_INET6"
    AF_ANY -> "AF_ANY"

// Internet address info
pub struct addr-info
  flags: int32
  pub family: net-family
  pub socktype: sock-type
  protocol: int32
  pub addr: sock-addr
  pub canonName: string;

// Socket addresses
pub value struct sock-addr
  pub family: net-family
  pub data: string
  pub port: maybe<int32> = Nothing;

// A uv tcp handle
pub value struct uv-tcp { internal: any }
// A uv OS socket handle
pub value struct uv-os-sock { internal: intptr_t }

pub inline fun get-addr-info(host : string, callback: (list<addr-info>) -> io-noexn (), hints : maybe<addr-info> = Nothing): io-noexn ()
  uv-get-addr-info(host, "", hints, callback)

extern uv-get-addr-info(node : string, service : string, hints : maybe<addr-info>, callback: (list<addr-info>) -> io-noexn ()): io-noexn ()
  c "kk_get_addrinfo"

// Initialize the handle. No socket is created as of yet.
pub extern tcp-init(): io-noexn error<uv-tcp>
  c "kk_uv_tcp_init"

// Bind the handle to an address and port. addr should point to an initialized struct sockaddr_in or struct sockaddr_in6.
// When the port is already taken, you can expect to see an UV_EADDRINUSE error from `listen()`` or `connect()`.
// That is, a successful call to this function does not guarantee that the call to `listen()` or `connect()` will succeed as well.
//
// flags can contain `UV_TCP_IPV6ONLY`, in which case dual-stack support is disabled and only IPv6 is used.
pub extern tcp/bind(^tcp: uv-tcp, addr: sock-addr, flags: int32): io-noexn uv-status-code
  c "kk_uv_tcp_bind"

// Not all tcp handles are streams (e.g. servers)
// Ensure there are safe wrappers
pub inline fun tcp/stream(^tcp: uv-tcp): io-noexn uv-stream
  Uv-stream(tcp.internal)

// Any uv can be a handle, so it's always safe to cast
pub inline fun tcp/uv-handle(^tcp: uv-tcp): io-noexn uv-handle
  Uv-handle(tcp.internal)

// Streams are not necessarily always tcp -- they could be files, pipes, udp, etc.
// Ensure there are safe wrappers
pub inline fun stream/tcp(^tcp: uv-stream): io-noexn uv-tcp
  Uv-tcp(tcp.internal)

// Establish an IPv4 or IPv6 TCP connection. Provide an initialized TCP handle.
// The callback is made when the connection has been established or when a connection error happened.
pub extern tcp/connect(^tcp: uv-tcp, addr: sock-addr, callback: (uv-status-code) -> io-noexn ()): io-noexn uv-status-code
  c "kk_uv_tcp_connect"
