/*---------------------------------------------------------------------------
  Copyright 2012-2023, Microsoft Research, Daan Leijen. Brigham Young University, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/// This module provides a set of functions for asynchronous network I/O.
module std/os/net-async

pub import std/async
import std/os/net

pub fun tcp(): iouv uvTcp
  tcp-init().untry

pub fun bind(tcp: uvTcp, addr: string, port: int): iouv ()
  net/bind(tcp, SockAddr(AF_INET, addr, Just(port.int32)), 0.int32).untry

pub fun listen(tcp: uvTcp): <async,iouv-noexn> channel<error<uvStream>>
  val ch = channel()
  await-to-channel(
    fn(cb)
      stream/listen(tcp.as-stream, 0.int32) fn(err)
        match tcp-init()
          Ok(t) ->
            val str = t.as-stream
            val err2 = tcp.as-stream.accept(str)
            match err2
              UV_OK -> cb(Ok(str), False)
              _ -> cb(Error(Exception(err2.message, AsyncExn(err2))), True)
          _ -> cb(Error(Exception(err.message, AsyncExn(err))), True)
      Just({
        // If canceled close the stream (also closes the handle)
        // ignoring errors for now
        tcp.as-stream.shutdown(fn(e) ())
      }) 
    ,ch,
    fn(e) e
  )

pub fun write(stream: uvStream, bts: bytes): <async,iouv> ()
  await fn(cb)
    stream.write([bts], fn(err)
      match err
        UV_OK -> cb(Ok(()))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    )
    Nothing

pub fun read(stream: uvStream): <async,iouv> bytes
  await fn(cb)
    stream.read-start(fn(bts)
      cb(Ok(bts))
    )
    Just({
      stream.read-stop()
      ()
    })

pub fun connect(tcp: uvTcp, address: sockAddr): <async,iouv> uvStream
  await fn(cb)
    tcp.connect(address) fn(err)
      match err
        UV_OK -> cb(Ok(tcp.as-stream))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    Nothing

pub fun connect(tcp: uvTcp, address: string, port: int=80): <async,iouv> uvStream
  await fn(cb)
    tcp.connect(SockAddr(AF_INET, address, Just(port.int32))) fn(err)
      match err
        UV_OK -> cb(Ok(tcp.as-stream))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    Nothing

pub fun shutdown(stream: uvStream): <async,iouv> ()
  await fn(cb)
    stream.shutdown(fn(err)
      match err
        UV_OK -> cb(Ok(()))
        _ -> cb(Error(Exception(err.message, AsyncExn(err))))
    )
    Nothing
  stream.as-tcp.shutdown()

// Closes the tcp handle
pub fun shutdown(tcp: uvTcp): <async,iouv> ()  
  await fn(cb)
    tcp.uvHandle.close fn()
      cb(Ok(()))
    Nothing
