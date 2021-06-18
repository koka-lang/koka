#pragma once
#ifndef KK_ATOMIC_H
#define KK_ATOMIC_H

/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

#if defined(__cplusplus)
// Use C++ atomics
#include <atomic>
#define  _Atomic(tp)            std::atomic<tp>
#define  kk_atomic(name)        std::atomic_##name
#define  kk_memory_order(name)  std::memory_order_##name
#define  kk_memory_order_t      std::kk_memory_order
#elif defined(_MSC_VER)
// Use MSVC C wrapper for C11 atomics
#define  _Atomic(tp)            tp
#define  ATOMIC_VAR_INIT(x)     x
#define  kk_atomic(name)        kk_atomic_##name
#define  kk_memory_order(name)  kk_memory_order_##name
#define  kk_memory_order_t      kk_memory_order
#else
// Use C11 atomics
#include <stdatomic.h>
#define  kk_atomic(name)        atomic_##name
#define  kk_memory_order(name)  memory_order_##name
#define  kk_memory_order_t      memory_order
#endif


#define kk_atomic_add32_relaxed(p,x)          kk_atomic_fetch_add32_explicit(p,x,kk_memory_order(relaxed))
#define kk_atomic_sub32_relaxed(p,x)          kk_atomic_fetch_sub32_explicit(p,x,kk_memory_order(relaxed))
#define kk_atomic_add32_acq_rel(p,x)          kk_atomic_fetch_add32_explicit(p,x,kk_memory_order(acq_rel))
#define kk_atomic_sub32_acq_rel(p,x)          kk_atomic_fetch_sub32_explicit(p,x,kk_memory_order(acq_rel))

#define kk_atomic_load_relaxed(p)             kk_atomic(load_explicit)(p,kk_memory_order(relaxed))
#define kk_atomic_load_acquire(p)             kk_atomic(load_explicit)(p,kk_memory_order(acquire))
#define kk_atomic_store_relaxed(p,x)          kk_atomic(store_explicit)(p,x,kk_memory_order(relaxed))
#define kk_atomic_store_release(p,x)          kk_atomic(store_explicit)(p,x,kk_memory_order(release))

#define kk_atomic_cas_weak_relaxed(p,exp,des) kk_atomic(compare_exchange_weak_explicit)(p,exp,des,kk_memory_order(relaxed),kk_memory_order(relaxed))
#define kk_atomic_cas_weak_acq_rel(p,exp,des) kk_atomic(compare_exchange_weak_explicit)(p,exp,des,kk_memory_order(acq_rel),kk_memory_order(acquire))
#define kk_atomic_cas_strong_relaxed(p,exp,des) kk_atomic(compare_exchange_strong_explicit)(p,exp,des,kk_memory_order(relaxed),kk_memory_order(relaxed))
#define kk_atomic_cas_strong_acq_rel(p,exp,des) kk_atomic(compare_exchange_strong_explicit)(p,exp,des,kk_memory_order(acq_rel),kk_memory_order(acquire))

#define kk_atomic_inc32_relaxed(p)            kk_atomic_add32_relaxed(p,1)
#define kk_atomic_dec32_relaxed(p)            kk_atomic_sub32_relaxed(p,1)
#define kk_atomic_inc32_acq_rel(p)            kk_atomic_add32_acq_rel(p,1)
#define kk_atomic_dec32_acq_rel(p)            kk_atomic_sub32_acq_rel(p,1)


#if defined(__cplusplus) || !defined(_MSC_VER)
// C++ or standard C11

#define kk_atomic_fetch_add32_explicit(p,x,mo)      kk_atomic(fetch_add_explicit)(p,x,mo)
#define kk_atomic_fetch_sub32_explicit(p,x,mo)      kk_atomic(fetch_sub_explicit)(p,x,mo)

#elif defined(_MSC_VER)
// MSVC C compilation wrapper that uses Interlocked operations to model C11 atomics.
#define MICROSOFT_WINDOWS_WINBASE_H_DEFINE_INTERLOCKED_CPLUSPLUS_OVERLOADS 1  // to avoid warnings
#include <Windows.h>
#include <intrin.h>
#ifdef _WIN64
typedef LONG64      msc_intptr_t;
#define WRAP64(f)   f##64
#else
typedef LONG        msc_intptr_t;
#define WRAP64(f)   f
#endif

typedef enum kk_memory_order_e {
  kk_memory_order_relaxed,
  kk_memory_order_consume,
  kk_memory_order_acquire,
  kk_memory_order_release,
  kk_memory_order_acq_rel,
  kk_memory_order_seq_cst
} kk_memory_order;

static inline uintptr_t kk_atomic_fetch_add_explicit(_Atomic(uintptr_t)*p, uintptr_t add, kk_memory_order_t mo) {
  KK_UNUSED(mo);
  return (uintptr_t)WRAP64(_InterlockedExchangeAdd)((volatile msc_intptr_t*)p, (msc_intptr_t)add);
}
static inline uintptr_t kk_atomic_fetch_sub_explicit(_Atomic(uintptr_t)*p, uintptr_t sub, kk_memory_order_t mo) {
  KK_UNUSED(mo);
  return (uintptr_t)WRAP64(_InterlockedExchangeAdd)((volatile msc_intptr_t*)p, -((msc_intptr_t)sub));
}
static inline uintptr_t kk_atomic_load_explicit(_Atomic(uintptr_t)*p, kk_memory_order_t mo) {
  KK_UNUSED(mo);
#if defined(_M_X64) || defined(_M_IX86)
  if (mo == kk_memory_order_relaxed) {
    return *p;
  }
#endif
  return (uintptr_t)WRAP64(_InterlockedOr)((volatile msc_intptr_t*)p, (msc_intptr_t)0);
}
static inline void kk_atomic_store_explicit(_Atomic(uintptr_t)*p, uintptr_t x, kk_memory_order_t mo) {
  KK_UNUSED(mo);
#if defined(_M_X64) || defined(_M_IX86)
  if (mo == kk_memory_order_relaxed) {
    *p = x;
    return;
  }
#endif
  WRAP64(_InterlockedExchange)((volatile msc_intptr_t*)p, (msc_intptr_t)x);
}

static inline bool kk_atomic_compare_exchange_weak_explicit(_Atomic(uintptr_t)*p, uintptr_t* expected, uintptr_t desired, kk_memory_order_t mo, kk_memory_order_t mofail) {
  KK_UNUSED(mo); KK_UNUSED(mofail);
  uintptr_t prev;
#ifdef InterlockedCompareExchangeNoFence
  if (mo == kk_memory_order_relaxed) {
    prev = (uintptr_t)WRAP64(InterlockedCompareExchangeNoFence)((volatile msc_intptr_t*)p, (msc_intptr_t)desired, (msc_intptr_t)(*expected));
  }
  else
#endif
  {
    prev = (uintptr_t)WRAP64(InterlockedCompareExchange)((volatile msc_intptr_t*)p, (msc_intptr_t)desired, (msc_intptr_t)(*expected));
  }
  if (prev==*expected) return true;
  *expected = prev;
  return false;
}
static inline bool kk_atomic_compare_exchange_strong_explicit(_Atomic(uintptr_t)*p, uintptr_t* expected, uintptr_t desired, kk_memory_order_t mo, kk_memory_order_t mofail) {
  return kk_atomic_compare_exchange_weak_explicit(p, expected, desired, mo, mofail);
}

static inline uint32_t kk_atomic_fetch_add32_explicit(_Atomic(uint32_t)*p, uint32_t add, kk_memory_order_t mo) {
#ifdef InterlockedExchangeAddNoFence
  if (mo == kk_memory_order_relaxed) {
    return (uint32_t)InterlockedExchangeAddNoFence((volatile LONG*)p, (LONG)add);
  }
  else 
#endif
  {
    return (uint32_t)InterlockedExchangeAdd((volatile LONG*)p, (LONG)add);
  }
}
static inline uint32_t kk_atomic_fetch_sub32_explicit(_Atomic(uint32_t)*p, uint32_t sub, kk_memory_order_t mo) {
  KK_UNUSED(mo);
  const LONG add = -((LONG)sub);
#ifdef InterlockedExchangeAddNoFence
  if (mo == kk_memory_order_relaxed) {
    return (uint32_t)InterlockedExchangeAddNoFence((volatile LONG*)p, add);
  }
  else 
#endif
  {
    return (uint32_t)InterlockedExchangeAdd((volatile LONG*)p, add);
  }
}
#endif


#endif // include guard
