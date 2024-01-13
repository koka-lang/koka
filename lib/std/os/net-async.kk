/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/// This module provides a set of functions for asynchronous network I/O.
module std/os/net-async

pub import std/async
import std/os/net

pub fun tcp(): io-eventx uvTcp
  tcp-init().untry

pub fun bind(tcp: uvTcp, addr: string, port: int): asyncx ()
  net/bind(tcp, SockAddr(AF_INET, addr, Just(port.int32)), 0.int32).untry

pub fun listen(tcp: uvTcp): <async,io-noexn> channel<error<uvStream>>
  val ch = channel()
  await-to-channel(
    fn(cb)
      stream/listen(tcp.stream, 0.int32) fn(err)
        match tcp-init()
          Ok(t) ->
            val str = t.stream
            val err2 = tcp.stream.accept(str)
            match err2
              UV_OK -> cb(Ok(str), False)
              _ -> cb(Error(Exception(err2.message, AsyncExn(err2))), True)
          _ -> cb(Error(Exception(err.message, AsyncExn(err))), True)
      Just({
        // If canceled close the stream (also closes the handle)
        // ignoring errors for now
        tcp.stream.shutdown(fn(e) ())
      }) 
    ,ch,
    fn(e) e
  )

pub fun write(stream: uvStream, bts: bytes): asyncx ()
  await fn(cb)
    stream.write([bts], fn(err)
      match err
        UV_OK -> cb(Ok(()))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    )
    Nothing

pub fun read(stream: uvStream): asyncx bytes
  await fn(cb)
    stream.read-start(fn(bts)
      cb(Ok(bts))
    )
    Just({
      stream.read-stop()
      ()
    })

pub fun addr/connect(tcp: uvTcp, address: sockAddr): asyncx uvStream
  await fn(cb)
    tcp.connect(address) fn(err)
      match err
        UV_OK -> cb(Ok(tcp.stream))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    Nothing

pub fun connect(tcp: uvTcp, address: string, port: int=80): asyncx uvStream
  await fn(cb)
    tcp.connect(SockAddr(AF_INET, address, Just(port.int32))) fn(err)
      match err
        UV_OK -> cb(Ok(tcp.stream))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    Nothing

pub fun stream/shutdown(stream: uvStream): asyncx ()
  await fn(cb)
    stream.shutdown(fn(err)
      match err
        UV_OK -> cb(Ok(()))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    )
    Nothing
  stream.tcp.shutdown()

// Closes the tcp handle
pub fun tcp/shutdown(tcp: uvTcp): asyncx ()  
  await fn(cb)
    tcp.uv-handle.close fn()
      cb(Ok(()))
    Nothing
