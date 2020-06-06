#pragma once
#ifndef __ATOMIC_H__
#define __ATOMIC_H__

/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

#if defined(_MSC_VER)
#define _Atomic(tp)         tp
#define ATOMIC_VAR_INIT(x)  x
#elif defined(__cplusplus)
#include <atomic>
#define  _Atomic(tp)        std::atomic<tp>
#else
#include <stdatomic.h>
#endif

static inline uint32_t atomic_increment32(volatile _Atomic(uint32_t)* p); 
static inline uint32_t atomic_decrement32(volatile _Atomic(uint32_t)* p);


#if defined(_MSC_VER) && (LONG_MAX == INT32_MAX)
#include <intrin.h>
static inline uint32_t atomic_increment32(volatile _Atomic(uint32_t)* p) {
 return (uint32_t)_InterlockedIncrement((volatile long*)p);
}
static inline uint32_t atomic_decrement32(volatile _Atomic(uint32_t)* p) {
 return (uint32_t)_InterlockedDecrement((volatile long*)p);
}
#else
static inline uint32_t atomic_increment32(volatile _Atomic(uint32_t)* p) {
  return atomic_fetch_add(p, (uint32_t)(1));
}
static inline uint32_t atomic_decrement32(volatile _Atomic(uint32_t)* p) {
  return atomic_fetch_sub(p, (uint32_t)(1));
}
#endif


#endif // include guard
