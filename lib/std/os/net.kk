/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/// This module provides a set of functions for asynchronous network I/O.
module std/os/net

pub import std/async
import uv/net

pub fun tcp(): io uv-tcp
  tcp-init().untry

pub fun tcp/bind(tcp: uv-tcp, addr: string, port: int): <async,io> ()
  net/bind(tcp, Sock-addr(AF_INET, addr, Just(port.int32)), 0.int32).untry

pub fun listen(tcp: uv-tcp): <async,io-noexn> channel<error<uv-stream>>
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

pub fun write(stream: uv-stream, bts: bytes): asyncx ()
  await fn(cb)
    stream.write([bts], fn(err)
      match err
        UV_OK -> cb(Ok(()))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    )
    Nothing

pub fun read(stream: uv-stream): asyncx bytes
  await fn(cb)
    stream.read-start(fn(bts)
      cb(Ok(bts))
    )
    Just({
      stream.read-stop()
      ()
    })

pub fun stream(stream: uv-stream): asyncx channel<bytes>
  val ch = channel()
  await-to-channel(fn(cb) {
      stream.read-start(fn(bts)
        cb(Ok(bts), False)
      )
      Just({
        stream.read-stop()
        ()
      })
    },
    ch,
    fn(e) e.default("".bytes))

pub fun addr/connect(tcp: uv-tcp, address: sock-addr): asyncx uv-stream
  await fn(cb)
    tcp.connect(address) fn(err)
      match err
        UV_OK -> cb(Ok(tcp.stream))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    Nothing

pub fun connect(tcp: uv-tcp, address: string, port: int=80): asyncx uv-stream
  await fn(cb)
    tcp.connect(Sock-addr(AF_INET, address, Just(port.int32))) fn(err)
      match err
        UV_OK -> cb(Ok(tcp.stream))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    Nothing

pub fun stream/shutdown(stream: uv-stream): <async,io> ()
  await fn(cb)
    stream.shutdown(fn(err)
      match err
        UV_OK -> cb(Ok(()))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    )
    Nothing
  stream.tcp.shutdown()

// Closes the tcp handle
pub fun tcp/shutdown(tcp: uv-tcp): asyncx ()  
  await fn(cb)
    tcp.uv-handle.close fn()
      cb(Ok(()))
    Nothing
