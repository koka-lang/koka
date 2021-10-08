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
#else
// Use C11 atomics
#include <stdatomic.h>
#define  kk_atomic(name)        atomic_##name
#define  kk_memory_order(name)  memory_order_##name
#define  kk_memory_order_t      memory_order
#endif

#define kk_atomic_load_relaxed(p)             kk_atomic(load_explicit)(p,kk_memory_order(relaxed))
#define kk_atomic_load_acquire(p)             kk_atomic(load_explicit)(p,kk_memory_order(acquire))
#define kk_atomic_store_relaxed(p,x)          kk_atomic(store_explicit)(p,x,kk_memory_order(relaxed))
#define kk_atomic_store_release(p,x)          kk_atomic(store_explicit)(p,x,kk_memory_order(release))

#define kk_atomic_cas_weak_relaxed(p,exp,des)   kk_atomic(compare_exchange_weak_explicit)(p,exp,des,kk_memory_order(relaxed),kk_memory_order(relaxed))
#define kk_atomic_cas_weak_acq_rel(p,exp,des)   kk_atomic(compare_exchange_weak_explicit)(p,exp,des,kk_memory_order(acq_rel),kk_memory_order(acquire))
#define kk_atomic_cas_strong_relaxed(p,exp,des) kk_atomic(compare_exchange_strong_explicit)(p,exp,des,kk_memory_order(relaxed),kk_memory_order(relaxed))
#define kk_atomic_cas_strong_acq_rel(p,exp,des) kk_atomic(compare_exchange_strong_explicit)(p,exp,des,kk_memory_order(acq_rel),kk_memory_order(acquire))

#define kk_atomic_add32_relaxed(p,x)          kk_atomic(fetch_add_explicit)(p,x,kk_memory_order(relaxed))
#define kk_atomic_sub32_relaxed(p,x)          kk_atomic(fetch_sub_explicit)(p,x,kk_memory_order(relaxed))
#define kk_atomic_add32_acq_rel(p,x)          kk_atomic(fetch_add_explicit)(p,x,kk_memory_order(acq_rel))
#define kk_atomic_sub32_acq_rel(p,x)          kk_atomic(fetch_sub_explicit)(p,x,kk_memory_order(acq_rel))
#define kk_atomic_sub_relaxed(p,x)            kk_atomic(fetch_sub_explicit)(p,x,kk_memory_order(relaxed))

#define kk_atomic_inc32_relaxed(p)            kk_atomic_add32_relaxed(p,1)
#define kk_atomic_dec32_relaxed(p)            kk_atomic_sub32_relaxed(p,1)
#define kk_atomic_inc32_acq_rel(p)            kk_atomic_add32_acq_rel(p,1)
#define kk_atomic_dec32_acq_rel(p)            kk_atomic_sub32_acq_rel(p,1)
#define kk_atomic_dec_relaxed(p)              kk_atomic_sub_relaxed(p,1)

#endif // include guard
