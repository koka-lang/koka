/*---------------------------------------------------------------------------
  Copyright 2012-2023, Microsoft Research, Daan Leijen. Brigham Young University, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module std/os/net

pub import std/os/stream
pub import std/num/int32
pub import std/os/uv

extern import
  c file "net-inline.c"

pub type sockType
  SOCK_ANY
  SOCK_STREAM
  SOCK_DGRAM

pub type netFamily
  AF_INET
  AF_INET6
  AF_ANY

pub fun show(family: netFamily): string
  match family
    AF_INET -> "AF_INET"
    AF_INET6 -> "AF_INET6"
    AF_ANY -> "AF_ANY"

pub struct addrInfo
  flags: int32
  pub family: netFamily
  pub socktype: sockType
  protocol: int32
  pub addr: sockAddr
  pub canonName: string;

pub value struct sockAddr
  pub family: netFamily
  pub data: string
  pub port: maybe<int32> = Nothing;

pub value struct uvTcp { internal: intptr_t }
pub value struct uvOsSock { internal: intptr_t }

pub inline fun getaddrinfo(host : string, callback: (list<addrInfo>) -> iouv-noexn (), hints : maybe<addrInfo> = Nothing): iouv-noexn ()
  xgetaddrinfo(host, "", hints, callback)

extern xgetaddrinfo(node : string, service : string, hints : maybe<addrInfo>, callback: (list<addrInfo>) -> iouv-noexn ()): iouv-noexn ()
  c "kk_get_addrinfo"

pub extern tcp-init(): iouv-noexn error<uvTcp>
  c "kk_uv_tcp_init"

pub extern bind(tcp: uvTcp, addr: sockAddr, flags: int32): iouv-noexn uvStatusCode
  c "kk_uv_tcp_bind"

// Not all tcp handles are streams (e.g. servers)
// Ensure there are safe wrappers
pub inline extern as-stream(tcp: uvTcp): iouv-noexn uvStream
  c inline "kk_std_os_stream__new_UvStream(#1.internal, kk_context())"

// Any uv can be a handle, so it's always safe to cast
pub inline extern uvHandle(tcp: uvTcp): iouv-noexn uvHandle
  c inline "kk_std_os_uv__new_UvHandle(#1.internal, kk_context())"

// Streams are not necessarily always tcp -- they could be files, pipes, udp, etc.
// Ensure there are safe wrappers
pub inline extern as-tcp(tcp: uvStream): iouv-noexn uvTcp
  c inline "kk_std_os_net__new_UvTcp(#1.internal, kk_context())"

pub extern connect(tcp: uvTcp, addr: sockAddr, callback: (uvStatusCode) -> iouv-noexn ()): iouv-noexn uvStatusCode
  c "kk_uv_tcp_connect"
