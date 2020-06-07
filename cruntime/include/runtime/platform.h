#pragma once
#ifndef __PLATFORM_H__
#define __PLATFORM_H__

/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------
  Platform: we assume:
  - C99 as C compiler (syntax and library), with possible C11 extensions for threads and atomics.
  - either a 32- or 64-bit platform (but others should be possible with few changes)
  - the compiler can do a great job on small static inline definitions (and we avoid #define's).
  - the compiler will inline small structs (like `struct box_s{ uintptr_t u; }`) without
    overhead (e.g. pass it in a register).
  - (>>) on signed integers is an arithmetic right shift (i.e. sign extending)
  - a char/byte is 8 bits
  - either little-endian, or big-endian
  - carefully code with strict aliasing in mind
--------------------------------------------------------------------------------------*/
#ifdef __cplusplus
#define decl_externc    extern "C"
#else
#define decl_externc    extern
#endif

#if (__cplusplus >= 201103L) || (_MSC_VER > 1900)  // C++11
#define _constexpr      constexpr
#else
#define _constexpr
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)
#if !defined(SHARED_LIB)
#define decl_export     decl_externc
#elif defined(SHARED_LIB_EXPORT)
#define decl_export     __declspec(dllexport)
#else
#define decl_export     __declspec(dllimport)
#endif
#elif defined(__GNUC__) // includes clang and icc
#define decl_export     decl_externc __attribute__((visibility("default"))) 
#else
#define decl_export     decl_externc
#endif

#if defined(__GNUC__)
#define unlikely(h)     __builtin_expect((h),0)
#define likely(h)       __builtin_expect((h),1)
#define decl_const      __attribute__((const))
#define decl_pure       __attribute__((pure))
#define noinline        __attribute__((noinline))
#define decl_thread     __thread
#elif defined(_MSC_VER)
#pragma warning(disable:4214)  // using bit field types other than int
#pragma warning(disable:4101)  // unreferenced local variable
#define unlikely(x)     (x)
#define likely(x)       (x)
#define decl_const
#define decl_pure
#define noinline        __declspec(noinline)
#define decl_thread     __declspec(thread)
#else
#define unlikely(h)     (h)
#define likely(h)       (h)
#define decl_const
#define decl_pure
#define noinline   
#define decl_thread     __thread
#endif

#define assert_internal assert

#ifndef UNUSED
#define UNUSED(x)  ((void)(x))
#ifdef NDEBUG
#define UNUSED_RELEASE(x)  UNUSED(x);
#else
#define UNUSED_RELEASE(x)  
#endif
#endif

// Defining constants of a specific size
#if LONG_MAX == INT64_MAX
# define LONG_SIZE 8
# define I32(i)  (i)
# define I64(i)  (i##L)
# define U32(i)  (i##U)
# define U64(i)  (i##UL)
#elif LONG_MAX == INT32_MAX
# define LONG_SIZE 4
# define I32(i)  (i##L)
# define I64(i)  (i##LL)
# define U32(i)  (i##UL)
# define U64(i)  (i##ULL)
#else
#error size of a `long` must be 32 or 64 bits
#endif

// Define size of intptr_t
#if INTPTR_MAX == INT64_MAX           // 9223372036854775807LL
# define INTPTR_SIZE 8
# define IP(i)  I64(i)
# define UP(i)  U64(i)
#elif INTPTR_MAX == INT32_MAX         // 2147483647LL
# define INTPTR_SIZE 4
# define IP(i)  I32(i)
# define UP(i)  U32(i)
#else
#error platform must be 32 or 64 bits
#endif
#define INTPTR_BITS (8*INTPTR_SIZE)
#define INTPTR_ALIGNUP(x)  ((((x)+INTPTR_SIZE-1)/INTPTR_SIZE)*INTPTR_SIZE)

// Define size of size_t
#if SIZE_MAX == UINT64_MAX           // 18446744073709551615LL
# define SIZE_SIZE 8
# define IZ(i)  I64(i)
# define UZ(i)  U64(i)
#elif SIZE_MAX == UINT32_MAX         // 4294967295LL
# define SIZE_SIZE 4
# define IZ(i)  I32(i)
# define UZ(i)  U32(i)
#else
#error size of a `size_t` must be 32 or 64 bits
#endif
#define SIZE_BITS (8*SIZE_SIZE)


// Abstract over the "natural machine word" as `intx_t`. This is useful when
// targeting architectures where `sizeof(void*) < sizeof(long)` (like x32), or 
// `sizeof(void*) > sizeof(size_t)` (like segmented architectures). 
// We have `sizeof(intx_t) >= sizeof(void*)` and `sizeof(intx_t) >= sizeof(long)`.
#if (LONG_MAX <= INTPTR_MAX)
typedef intptr_t       intx_t;
typedef uintptr_t      uintx_t;
#define UX(i)          (i##ULL)
#define IX(i)          (i##LL)
#define INTX_SIZE      INTPTR_SIZE
#else 
typedef long           intx_t;
typedef unsigned long  uintx_t;
#define UX(i)          (i##UL)
#define IX(i)          (i##L)
#define INTX_SIZE      LONG_SIZE
#endif
#define INTX_BITS  (8*INTX_SIZE)


// Distinguish unsigned shift right and signed arithmetic shift right.
static inline intx_t  sar(intx_t i, intx_t shift) { return (i >> shift); }
static inline uintx_t shr(uintx_t i, uintx_t shift) { return (i >> shift); }

// Limited reference counts can be more efficient
#if PTRDIFF_MAX <= INT32_MAX
#undef  REFCOUNT_LIMIT_TO_32BIT
#define REFCOUNT_LIMIT_TO_32BIT  1
#endif


#endif // include guard
