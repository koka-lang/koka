#pragma once
#ifndef __ATOMIC_H__
#define __ATOMIC_H__

/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

#if defined(__cplusplus)
// Use C++ atomics
#include <atomic>
#define  _Atomic(tp)         std::atomic<tp>
#define  atomic(name)        std::atomic_##name
#define  memory_order(name)  std::memory_order_##name
#define  memory_order_t      std::memory_order
#elif defined(_MSC_VER)
// Use MSVC C wrapper for C11 atomics
#define  _Atomic(tp)         tp
#define  ATOMIC_VAR_INIT(x)  x
#define  atomic(name)        atomic_##name
#define  memory_order(name)  memory_order_##name
#define  memory_order_t      memory_order
#else
// Use C11 atomics
#include <stdatomic.h>
#define  atomic(name)        atomic_##name
#define  memory_order(name)  memory_order_##name
#define  memory_order_t      memory_order
#endif

#define atomic_add32_relaxed(p,x)         atomic_fetch_add32_explicit(p,x,memory_order(relaxed))
#define atomic_sub32_relaxed(p,x)         atomic_fetch_sub32_explicit(p,x,memory_order(relaxed))
#define atomic_add32_acq_rel(p,x)         atomic_fetch_add32_explicit(p,x,memory_order(acq_rel))
#define atomic_sub32_acq_rel(p,x)         atomic_fetch_sub32_explicit(p,x,memory_order(acq_rel))

#define atomic_inc32_relaxed(p)           atomic_add32_relaxed(p,1)
#define atomic_dec32_relaxed(p)           atomic_sub32_relaxed(p,1)
#define atomic_inc32_acq_rel(p)           atomic_add32_acq_rel(p,1)
#define atomic_dec32_acq_rel(p)           atomic_sub32_acq_rel(p,1)


#if defined(__cplusplus) || !defined(_MSC_VER)
// C++ or standard C11

#define atomic_fetch_add32_explicit(p,x,mo)      atomic(fetch_add_explicit)(p,x,mo)
#define atomic_fetch_sub32_explicit(p,x,mo)      atomic(fetch_sub_explicit)(p,x,mo)

#elif defined(_MSC_VER)
// MSVC C compilation wrapper that uses Interlocked operations to model C11 atomics.
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <intrin.h>
#ifdef _WIN64
typedef LONG64      msc_intptr_t;
#define WRAP64(f)   f##64
#else
typedef LONG        msc_intptr_t;
#define WRAP64(f)   f
#endif

typedef enum memory_order_e {
  memory_order_relaxed,
  memory_order_consume,
  memory_order_acquire,
  memory_order_release,
  memory_order_acq_rel,
  memory_order_seq_cst
} memory_order;

static inline uintptr_t atomic_fetch_add_explicit(_Atomic(uintptr_t)*p, uintptr_t add, memory_order_t mo) {
  UNUSED(mo);
  return (uintptr_t)WRAP64(_InterlockedExchangeAdd)((volatile msc_intptr_t*)p, (msc_intptr_t)add);
}
static inline uintptr_t atomic_fetch_sub_explicit(_Atomic(uintptr_t)*p, uintptr_t sub, memory_order_t mo) {
  UNUSED(mo);
  return (uintptr_t)WRAP64(_InterlockedExchangeAdd)((volatile msc_intptr_t*)p, -((msc_intptr_t)sub));
}

static inline uint32_t atomic_fetch_add32_explicit(_Atomic(uint32_t)*p, uint32_t add, memory_order_t mo) {
  if (mo == memory_order_relaxed) {
    return (uint32_t)InterlockedExchangeAddNoFence((volatile LONG*)p, (LONG)add);
  }
  else {
    return (uint32_t)InterlockedExchangeAdd((volatile LONG*)p, (LONG)add);
  }
}
static inline uint32_t atomic_fetch_sub32_explicit(_Atomic(uint32_t)*p, uint32_t sub, memory_order_t mo) {
  UNUSED(mo);
  const LONG add = -((LONG)sub);
  if (mo == memory_order_relaxed) {
    return (uint32_t)InterlockedExchangeAddNoFence((volatile LONG*)p, add);
  }
  else {
    return (uint32_t)InterlockedExchangeAdd((volatile LONG*)p, add);
  }
}
#endif


#endif // include guard
