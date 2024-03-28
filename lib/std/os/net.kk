/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module std/os/net

pub import std/os/stream
pub import std/num/int32
pub import std/data/bytes
pub import std/os/event-loop

extern import
  c file "net-inline.c"

pub type sock-type
  SOCK_ANY
  SOCK_STREAM
  SOCK_DGRAM

pub type net-family
  AF_INET
  AF_INET6
  AF_ANY

pub fun net-family/show(family: net-family): string
  match family
    AF_INET -> "AF_INET"
    AF_INET6 -> "AF_INET6"
    AF_ANY -> "AF_ANY"

pub struct addr-info
  flags: int32
  pub family: net-family
  pub socktype: sock-type
  protocol: int32
  pub addr: sock-addr
  pub canonName: string;

pub value struct sock-addr
  pub family: net-family
  pub data: string
  pub port: maybe<int32> = Nothing;

pub value struct uv-tcp { internal: intptr_t }
pub value struct uv-os-sock { internal: intptr_t }

pub inline fun get-addr-info(host : string, callback: (list<addr-info>) -> io-event (), hints : maybe<addr-info> = Nothing): io-event ()
  uv-get-addr-info(host, "", hints, callback)

extern uv-get-addr-info(node : string, service : string, hints : maybe<addr-info>, callback: (list<addr-info>) -> io-event ()): io-event ()
  c "kk_get_addrinfo"

pub extern tcp-init(): io-noexn error<uv-tcp>
  c "kk_uv_tcp_init"

pub extern tcp/bind(tcp: uv-tcp, addr: sock-addr, flags: int32): io-noexn uv-status-code
  c "kk_uv_tcp_bind"

// Not all tcp handles are streams (e.g. servers)
// Ensure there are safe wrappers
pub inline extern tcp/stream(tcp: uv-tcp): io-noexn uv-stream
  c inline "kk_std_os_stream__new_Uv_stream(#1.internal, kk_context())"

// Any uv can be a handle, so it's always safe to cast
pub inline extern tcp/uv-handle(tcp: uv-tcp): io-noexn uv-handle
  c inline "kk_std_os_event_dash_loop__new_Uv_handle(#1.internal, kk_context())"

// Streams are not necessarily always tcp -- they could be files, pipes, udp, etc.
// Ensure there are safe wrappers
pub inline extern stream/tcp(tcp: uv-stream): io-noexn uv-tcp
  c inline "kk_std_os_net__new_Uv_tcp(#1.internal, kk_context())"

pub extern tcp/connect(tcp: uv-tcp, addr: sock-addr, callback: (uv-status-code) -> io-event ()): io-event uv-status-code
  c "kk_uv_tcp_connect"
