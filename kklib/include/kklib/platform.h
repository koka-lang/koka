#pragma once
#ifndef KK_PLATFORM_H
#define KK_PLATFORM_H

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
  - the compiler will inline small structs (like `struct kk_box_s{ uintptr_t u; }`) without
    overhead (e.g. pass it in a register). This allows for better static checking.
  - (>>) on signed integers is an arithmetic right shift (i.e. sign extending)
  - a char/byte is 8 bits
  - either little-endian, or big-endian
  - carefully code with strict aliasing in mind
--------------------------------------------------------------------------------------*/
#ifdef __cplusplus
#define kk_decl_externc    extern "C"
#else
#define kk_decl_externc    extern
#endif

#ifdef __STDC_VERSION__
#if (__STDC_VERSION__ >= 201112L)
#define KK_C11
#elif (__STDC_VERSION__ >= 199901L) 
#define KK_C99
#endif
#endif

#ifdef __cplusplus
#if (__cplusplus >= 201703L)
#define KK_CPP17
#elif (__cplusplus >= 201402L)
#define KK_CPP14
#elif (__cplusplus >= 201103L)
#define KK_CPP11
#endif
#endif

#if defined(_WIN32) && !defined(WIN32)
#define WIN32  1
#endif

#if ((defined(__cplusplus) && __cplusplus >= 201103L)) || (_MSC_VER > 1900)  // C++11
#define kk_constexpr      constexpr
#else
#define kk_constexpr
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)
#if !defined(SHARED_LIB)
#define kk_decl_export     kk_decl_externc
#elif defined(SHARED_LIB_EXPORT)
#define kk_decl_export     __declspec(dllexport)
#else
#define kk_decl_export     __declspec(dllimport)
#endif
#elif defined(__GNUC__) // includes clang and icc
#define kk_decl_export     kk_decl_externc __attribute__((visibility("default"))) 
#else
#define kk_decl_export     kk_decl_externc
#endif

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-value"
#define kk_unlikely(h)     __builtin_expect((h),0)
#define kk_likely(h)       __builtin_expect((h),1)
#define kk_decl_const      __attribute__((const))    // reads no global state at all
#define kk_decl_pure       __attribute__((pure))     // may read global state but has no observable side effects
#define kk_decl_noinline   __attribute__((noinline))
#define kk_decl_align(a)   __attribute__((aligned(a)))
#define kk_decl_thread     __thread
#elif defined(_MSC_VER)
#pragma warning(disable:4214)  // using bit field types other than int
#pragma warning(disable:4101)  // unreferenced local variable
#pragma warning(disable:4204)  // non-constant aggregate initializer
#pragma warning(disable:4068)  // unknown pragma
#pragma warning(disable:4996)  // POSIX name deprecated
#define kk_unlikely(x)     (x)
#define kk_likely(x)       (x)
#define kk_decl_const
#define kk_decl_pure
#define kk_decl_noinline   __declspec(noinline)
#define kk_decl_align(a)   __declspec(align(a))
#define kk_decl_thread     __declspec(thread)
#else
#define kk_unlikely(h)     (h)
#define kk_likely(h)       (h)
#define kk_decl_const
#define kk_decl_pure
#define kk_decl_noinline   
#define kk_decl_align(a)   
#define kk_decl_thread     __thread
#endif

// Assertions; kk_assert_internal is only enabled when KK_DEBUG_FULL is defined
#define kk_assert(x)          assert(x)
#ifdef KK_DEBUG_FULL
#define kk_assert_internal(x) kk_assert(x)
#else
#define kk_assert_internal(x) 
#endif

#ifndef KK_UNUSED
#define KK_UNUSED(x)          ((void)(x))
#ifdef NDEBUG
#define KK_UNUSED_RELEASE(x)  KK_UNUSED(x)
#else
#define KK_UNUSED_RELEASE(x)  
#endif
#ifndef KK_DEBUG_FULL
#define KK_UNUSED_INTERNAL(x)  KK_UNUSED(x)
#else
#define KK_UNUSED_INTERNAL(x)  
#endif
#endif

// Defining constants of a specific size
#if LONG_MAX == INT64_MAX
# define KK_LONG_SIZE   8
# define KI32(i)        (i)
# define KI64(i)        (i##L)
# define KU32(i)        (i##U)
# define KU64(i)        (i##UL)
#elif LONG_MAX == INT32_MAX
# define KK_LONG_SIZE 4
# define KI32(i)        (i##L)
# define KI64(i)        (i##LL)
# define KU32(i)        (i##UL)
# define KU64(i)        (i##ULL)
#else
#error size of a `long` must be 32 or 64 bits
#endif

// Define size of intptr_t
#if INTPTR_MAX == INT64_MAX           // 9223372036854775807LL
# define KK_INTPTR_SIZE 8
# define KIP(i)         KI64(i)
# define KUP(i)         KU64(i)
#elif INTPTR_MAX == INT32_MAX         // 2147483647LL
# define KK_INTPTR_SIZE 4
# define KIP(i)         KI32(i)
# define KUP(i)         KU32(i)
#else
#error platform must be 32 or 64 bits
#endif
#define KK_INTPTR_BITS        (8*KK_INTPTR_SIZE)
#define KK_INTPTR_ALIGNUP(x)  ((((x)+KK_INTPTR_SIZE-1)/KK_INTPTR_SIZE)*KK_INTPTR_SIZE)

// Define size of size_t and kk_ssize_t  (ssize_t is POSIX only and not C99)
#if SIZE_MAX == UINT64_MAX           // 18446744073709551615LL
# define KK_SIZE_SIZE   8
# define KIZ(i)         KI64(i)
# define KUZ(i)         KU64(i)
typedef int64_t         kk_ssize_t;  
#define KK_SSIZE_MAX    INT64_MAX
#define KK_SSIZE_MIN    INT64_MIN
#elif SIZE_MAX == UINT32_MAX         // 4294967295LL
# define KK_SIZE_       SIZE 4
# define KIZ(i)         KI32(i)
# define KUZ(i)         KU32(i)
typedef int32_t         kk_ssize_t;
#define KK_SSIZE_MAX    INT32_MAX
#define KK_SSIZE_MIN    INT32_MIN
#else
#error size of a `size_t` must be 32 or 64 bits
#endif
#define KK_SIZE_BITS   (8*KK_SIZE_SIZE)

// off_t
typedef int64_t     kk_off_t;
#define KK_OFF_MAX  INT64_MAX
#define KK_OFF_MIN  INT64_MIN

// Abstract over the "natural machine word" as `kk_intx_t` such 
// that `sizeof(kk_intx_t) == max(sizeof(long),sizeof(size_t))`. 
// Note: we cannot use `long` for this as it is sometimes too short 
// (as on Windows on 64-bit where it is 32 bits)
// Similarly, `size_t` is sometimes too short on segmented architectures.
// Also, on some architectures `sizeof(void*) < sizeof(long)` (like the x32 ABI), or 
// `sizeof(void*) > sizeof(size_t)` (with segmented architectures).
#if (ULONG_MAX < SIZE_MAX)
typedef kk_ssize_t     kk_intx_t;
typedef size_t         kk_uintx_t;
#define KUX(i)         KUZ(i)
#define KIX(i)         KIZ(i)
#define KK_INTX_SIZE   KK_SIZE_SIZE
#define KK_INTX_MAX    KK_SSIZE_MAX
#define KK_INTX_MIN    KK_SSIZE_MIN
#define PRIxUX         "%zx"
#define PRIXUX         "%zX"
#else 
typedef long           kk_intx_t;
typedef unsigned long  kk_uintx_t;
#define KUX(i)         (i##UL)
#define KIX(i)         (i##L)
#define KK_INTX_SIZE   KK_LONG_SIZE
#define KK_INTX_MAX    LONG_MAX
#define KK_INTX_MIN    LONG_MIN
#define PRIxUX         "%lx"
#define PRIXUX         "%lX"
#endif
#define KK_INTX_BITS   (8*KK_INTX_SIZE)


// Distinguish unsigned shift right and signed arithmetic shift right.
static inline kk_intx_t   kk_sar(kk_intx_t i, kk_intx_t shift)   { return (i >> shift); }
static inline kk_uintx_t  kk_shr(kk_uintx_t u, kk_uintx_t shift) { return (u >> shift); }
static inline int32_t     kk_sar32(int32_t i, kk_intx_t shift)   { return (i >> shift); }
static inline uint32_t    kk_shr32(uint32_t u, kk_uintx_t shift) { return (u >> shift); }
static inline int64_t     kk_sar64(int64_t i, kk_intx_t shift)   { return (i >> shift); }
static inline uint64_t    kk_shr64(uint64_t u, kk_uintx_t shift) { return (u >> shift); }

// Architecture assumptions
#define KK_ARCH_LITTLE_ENDIAN   1
//#define KK_ARCH_BIG_ENDIAN       1

#define KK_FUNPTR_SIZE          KK_INTPTR_SIZE    // the size of function pointer: `void (*f)(void)`


#endif // include guard
