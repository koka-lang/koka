#pragma once
#ifndef KK_PLATFORM_H
#define KK_PLATFORM_H

/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------
  Platform: we assume:
  - C99 as C compiler (syntax and library), with possible C11 extensions for threads and atomics.
  - Write code such that it can be compiled with a C++ compiler as well (used with msvc)
  - Either a 32, 64, or 128-bit platform (but others should be possible with few changes).
  - The compiler can do a great job on small static inline definitions (and we avoid #define's
    to get better static type checks).
  - The compiler will inline small structs (like `struct kk_box_s{ uintptr_t u; }`) without
    overhead (e.g. pass it in a register). This also allows for better static type checking.
  - (>>) on signed integers is an arithmetic right shift (i.e. sign extending).
  - Integers use two's complement representation.
  - A char/byte is 8 bits.
  - Either little-endian, or big-endian (no PDP-11 middle endian :-)).
  - Carefully code with strict aliasing in mind.
  - Always prefer signed integers over unsigned ones, and use `kk_ssize_t` for sizes (see comments below).
    Only use unsigned for bitfields or masks.
--------------------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------
  Object size and signed/unsigned:
  We limit the maximum object size (and array sizes) to at most `SIZE_MAX/2` (`PTRDIFF_MAX`) bytes
  so we can always use the signed `kk_ssize_t` (instead of `size_t`) to specify sizes
  and do indexing in arrays. This avoids:
  - Signed/unsigned conversion (especially when mixing pointer arithmetic and lengths).
  - Subtle loop bound errors (consider `for(unsigned u = 0; u < len()-1; u++)` if `len()`
    happens to be `0` etc.).
  - Performance degradation -- modern compilers can compile signed loop variables
    better (as signed overflow is undefined).
  - Wrong API usage (passing a negative value is easier to detect)

  A drawback is that this limits object sizes to half the address space-- for 64-bit
  this is not a problem but string lengths for example on 32-bit are limited to be
  "just" 2^31 bytes at most. Nevertheless, we feel this is an acceptible trade-off
  (especially since the largest object is nowadays is already limited in practice
   to `PTRDIFF_MAX` e.g. <https://gcc.gnu.org/bugzilla//show_bug.cgi?id=67999>).

  We also need some helpers to deal with API's (like `strlen`) that use `size_t`
  results or arguments, where we clamp the values into the `kk_ssize_t` range
  (but again, on modern systems no clamping will ever happen as these already limit the size of objects to PTRDIFF_MAX)
--------------------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------
  Integer sizes and portability

  Here are some architectures with the bit size of various integers, where
  - `intptr_t` for addresses (where `sizeof(intptr_t) == sizeof(void*)`), 
  - `size_t` for object sizes, 
  - `kk_intx_t` for the natural largest register size (for general arithmetic),
  
  We always have: 
  - `|intptr_t| >= |size_t| >= |int|`. 
  - `|kk_intx_t| >= |int|`.
  
        system         intptr_t   size_t   int   long   intx    notes
 ------------------ ----------- -------- ----- ------ ------  -----------
  x86, arm32                32       32    32     32     32 
  x64, arm64, etc.          64       64    32     64     64 
  x64 windows               64       64    32     32     64   size_t   > long
  x32 linux                 32       32    32     32     64   intx_t   > size_t,intptr_t
  arm CHERI                128       64    32     64     64   intptr_t > size_t
  riscV 128-bit            128      128    32     64    128   
  x86 16-bit small          16       16    16     32     16   long > size_t
  x86 16-bit large          32       16    16     32     16   intptr_t/long > size_t
  x86 16-bit huge           32       32    16     32     16   size_t > intx_t

  We use a signed `size_t` as `kk_ssize_t` (see earlier comments) 

  We also have:
  - `kk_intb_t` (boxed integer) as the integer size that can hold a boxed value
  - `kk_intf_t` (field integer) as the largest integer such that `|kk_intf_t| <= min(|kk_intb_t|,|kk_intx_t|)`.

  Usually `kk_intb_t` is equal to `kk_intptr_t` but it can smaller if heap
  compression is used. This is controlled by the `KK_INTB_SIZE` define.

        system                  intptr_t   size_t   intx   intb   intf    notes
 ----------------------------- --------- -------- ------ ------ ------  -----------
  x64, arm64,                        64       64     64     64     64
  x64, arm64 compressed 32-bit       64       64     64     32     32   limit heap to 2^32 (*4)

  arm CHERI                         128       64     64    128     64   |intb| > |intf|
  arm CHERI compressed 64-bit       128       64     64     64     64   store addresses only in a box
  arm CHERI compressed 32-bit       128       64     64     32     32   compress address as well

  riscV 128-bit                     128      128    128    128    128
  riscV 128-bit compressed 64-bit   128      128    128     64     64   limit heap to 2^64 (*4)
  riscV 128-bit compressed 32-bit   128      128    128     32     32   limit heap to 2^32 (*4)
  x32 linux                          32       32     64     32     32   |intx| > |intb|

--------------------------------------------------------------------------------------*/


#if defined(__clang_major__) && __clang_major__ < 9
#error koka requires at least clang version 9 (due to atomics support)
#endif

#ifdef __cplusplus
#define kk_decl_externc    extern "C"
#else
#define kk_decl_externc    extern
#define nullptr            NULL
#endif

#ifdef __STDC_VERSION__
#if (__STDC_VERSION__ >= 201710L)
#define KK_C17  1
#endif
#if (__STDC_VERSION__ >= 201112L)
#define KK_C11  1
#endif
#if (__STDC_VERSION__ >= 199901L) 
#define KK_C99  1
#endif
#endif

#ifdef __cplusplus
#if (__cplusplus >= 202002L) || (defined(_MSVC_LANG) && _MSVC_LANG >= 202002L)
#define KK_CPP20  1
#endif
#if (__cplusplus >= 201703L) || (defined(_MSVC_LANG) && _MSVC_LANG >= 201703L)
#define KK_CPP17  1
#endif
#if (__cplusplus >= 201402L) || (defined(_MSVC_LANG) && _MSVC_LANG >= 201402L)
#define KK_CPP14  1
#endif
#if (__cplusplus >= 201103L) || (_MSC_VER > 1900) 
#define KK_CPP11  1
#endif
#endif

#if defined(_WIN32) && !defined(WIN32)
#define WIN32  1
#endif

#if defined(KK_CPP11)
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
#pragma GCC diagnostic ignored "-Warray-bounds"         // gives wrong warnings in std/os/path for string literals
#pragma GCC diagnostic ignored "-Waddress-of-packed-member"
#define kk_decl_const         __attribute__((const))    // reads no global state at all
#define kk_decl_pure          __attribute__((pure))     // may read global state but has no observable side effects
#define kk_decl_noinline       __attribute__((noinline))
#define kk_decl_align(a)      __attribute__((aligned(a)))
#define kk_decl_thread        __thread
#define kk_struct_packed      struct __attribute__((__packed__))
#define kk_struct_packed_end 
#define KK_HAS_STRUCT_PACKING 1
#elif defined(_MSC_VER)
#pragma warning(disable:4214)  // using bit field types other than int
#pragma warning(disable:4101)  // unreferenced local variable
#pragma warning(disable:4204)  // non-constant aggregate initializer
#pragma warning(disable:4068)  // unknown pragma
#pragma warning(disable:4996)  // POSIX name deprecated
#pragma warning(disable:26812) // the enum type is unscoped (in C++)
#define kk_decl_const
#define kk_decl_pure
#define kk_decl_noinline      __declspec(noinline)
#define kk_decl_align(a)      __declspec(align(a))
#define kk_decl_thread        __declspec(thread)
#define kk_struct_packed      __pragma(pack(push,1)) struct
#define kk_struct_packed_end  __pragma(pack(pop))
#define KK_HAS_STRUCT_PACKING 1
#ifndef __cplusplus  // need c++ compilation for correct atomic operations on msvc
#error "when using cl (the Microsoft Visual C++ compiler), use the /TP option to always compile in C++ mode."
#endif
#else
#define kk_decl_const
#define kk_decl_pure
#define kk_decl_noinline   
#define kk_decl_align(a)   
#define kk_decl_thread        __thread
#define kk_struct_packed      struct
#define kk_struct_packed_end  
#define KK_HAS_STRUCT_PACKING 0
#endif

#if defined(__GNUC__) || defined(__clang__)
#define kk_unlikely(x)     (__builtin_expect(!!(x),false))
#define kk_likely(x)       (__builtin_expect(!!(x),true))
#elif defined(KK_CPP20)
#define kk_unlikely(x)     (x) [[unlikely]]
#define kk_likely(x)       (x) [[likely]]
#else
#define kk_unlikely(x)     (x)
#define kk_likely(x)       (x)
#endif

// assign const field in a struct
#define kk_assign_const(tp,field) ((tp*)&(field))[0]

// Assertions; kk_assert_internal is only enabled when KK_DEBUG_FULL is defined
#define kk_assert(x)          assert(x)
#ifdef KK_DEBUG_FULL
#define kk_assert_internal(x) kk_assert(x)
#else
#define kk_assert_internal(x) 
#endif


#ifndef kk_unused
#define kk_unused(x)          ((void)(x))
#ifdef NDEBUG
#define kk_unused_release(x)  kk_unused(x)
#else
#define kk_unused_release(x)  
#endif
#ifndef KK_DEBUG_FULL
#define kk_unused_internal(x)  kk_unused(x)
#else
#define kk_unused_internal(x)  
#endif
#endif

// Defining constants of a specific size (as not all platforms define the INTXX_C macros)
#if LONG_MAX == INT64_MAX
# define KK_LONG_SIZE   8
# define KK_I32(i)      (i)
# define KK_I64(i)      (i##L)
# define KK_U32(i)      (i##U)
# define KK_U64(i)      (i##UL)
#elif LONG_MAX == INT32_MAX
# define KK_LONG_SIZE   4
# define KK_I32(i)      (i##L)
# define KK_I64(i)      (i##LL)
# define KK_U32(i)      (i##UL)
# define KK_U64(i)      (i##ULL)
#else
#error size of a `long` must be 32 or 64 bits
#endif

#ifdef _MSC_VER
# define KK_I128(i)      (i##i128)
# define KK_U128(i)      (i##ui128)
#else
# define KK_I128(i)      (INT128_C(i))
# define KK_U128(i)      (UINT128_C(i))
#endif

#define KK_KiB        (1024)
#define KK_MiB        (KK_I32(1024)*KK_KiB)
#define KK_GiB        (KK_I32(1024)*KK_MiB)


// Define size of intptr_t
#if INTPTR_MAX == INT128_MAX
# define KK_INTPTR_SIZE   16
# define KK_INTPTR_SHIFT  4
# define KK_IP(i)         KK_I128(i)
# define KK_UP(i)         KK_U128(i)
#elif INTPTR_MAX == INT64_MAX         
# define KK_INTPTR_SIZE   8
# define KK_INTPTR_SHIFT  3
# define KK_IP(i)         KK_I64(i)
# define KK_UP(i)         KK_U64(i)
#elif INTPTR_MAX == INT32_MAX
# define KK_INTPTR_SIZE   4
# define KK_INTPTR_SHIFT  2
# define KK_IP(i)         KK_I32(i)
# define KK_UP(i)         KK_U32(i)
#elif INTPTR_MAX == INT16_MAX
# define KK_INTPTR_SIZE   2
# define KK_INTPTR_SHIFT  1
# define KK_IP(i)         i
# define KK_UP(i)         i
#else
#error platform pointers must be 16, 32, 64, or 128 bits
#endif
#define KK_INTPTR_BITS        (8*KK_INTPTR_SIZE)
#define KK_INTPTR_ALIGNUP(x)  ((((x)+KK_INTPTR_SIZE-1)/KK_INTPTR_SIZE)*KK_INTPTR_SIZE)

// Define size of size_t and kk_ssize_t 
#if SIZE_MAX == UINT128_MAX
# define KK_SIZE_SIZE   16
# define KK_IZ(i)       KK_I128(i)
# define KK_UZ(i)       KK_U128(i)
# define KK_SSIZE_MAX   INT64_MAX
# define KK_SSIZE_MIN   INT64_MIN
typedef int64_t         kk_ssize_t;
#elif SIZE_MAX == UINT64_MAX
# define KK_SIZE_SIZE   8
# define KK_IZ(i)       KK_I64(i)
# define KK_UZ(i)       KK_U64(i)
# define KK_SSIZE_MAX   INT64_MAX
# define KK_SSIZE_MIN   INT64_MIN
typedef int64_t         kk_ssize_t;
#elif SIZE_MAX == UINT32_MAX         
# define KK_SIZE_SIZE   4
# define KK_IZ(i)       KK_I32(i)
# define KK_UZ(i)       KK_U32(i)
# define KK_SSIZE_MAX   INT32_MAX
# define KK_SSIZE_MIN   INT32_MIN
typedef int32_t         kk_ssize_t;
#elif SIZE_MAX == UINT16_MAX         
# define KK_SIZE_SIZE   2
# define KK_IZ(i)       i
# define KK_UZ(i)       i
# define KK_SSIZE_MAX   INT16_MAX
# define KK_SSIZE_MIN   INT16_MIN
typedef int16_t         kk_ssize_t;
#else
#error size of a `size_t` must be 16, 32, 64 or 128 bits
#endif
#define KK_SSIZE_SIZE   KK_SIZE_SIZE
#define KK_SIZE_BITS    (8*KK_SIZE_SIZE)


// off_t: we use signed 64-bit file offsets (unless on a 16-bit platform)
#if (INT_MAX > INT16_MAX)
typedef int64_t     kk_off_t;
#define KK_OFF_MAX  INT64_MAX
#define KK_OFF_MIN  INT64_MIN
#else
typedef int32_t     kk_off_t;
#define KK_OFF_MAX  INT32_MAX
#define KK_OFF_MIN  INT32_MIN
#endif

// kk_addr_t: a signed integer that can hold a plain address (usually intptr_t but may be smaller on capability architectures)
#if defined(KK_CHERI) 
typedef kk_ssize_t    kk_addr_t;
typedef kk_size_t     kk_uaddr_t;
#define KK_ADDR_MAX   KK_SSIZE_MAX
#define KK_ADDR_MIN   KK_SSIZE_MIN
#define KK_ADDR_BITS  KK_SIZE_BITS
#else
typedef intptr_t      kk_addr_t;
typedef uintptr_t     kk_uaddr_t;
#define KK_ADDR_MAX   INTPTR_MAX
#define KK_ADDR_MIN   INTPTR_MIN
#define KK_ADDR_BITS  KK_INTPTR_BITS
#endif


// We limit the maximum object size (and array sizes) to at most `SIZE_MAX/2` bytes.
static inline kk_ssize_t kk_to_ssize_t(size_t sz) {
  kk_assert(sz <= KK_SSIZE_MAX);
  return kk_likely(sz <= KK_SSIZE_MAX) ? (kk_ssize_t)sz : KK_SSIZE_MAX;
}
static inline size_t kk_to_size_t(kk_ssize_t sz) {
  kk_assert(sz >= 0);
  return kk_likely(sz >= 0) ? (size_t)sz : 0;
}

#if defined(NDEBUG)
#define kk_ssizeof(tp)   ((kk_ssize_t)(sizeof(tp)))
#else
#define kk_ssizeof(tp)   (kk_to_ssize_t(sizeof(tp)))
#endif


// We define `kk_intx_t` as an integer with the natural (fast) machine register size. 
// We define it such that `sizeof(kk_intx_t)` is, with `m = max(sizeof(long),sizeof(size_t))`
//   (m==8 || x32) ? 8 : ((m == 4 && sizeof(int) > 2)  ? 4 : sizeof(int))
// (We cannot use just `long` as it is sometimes too short (as on Windows 64-bit or x32 where a `long` is 32 bits).
#if (LONG_MAX == INT64_MAX) || (SIZE_MAX == UINT64_MAX) || (defined(__x86_64__) && SIZE_MAX == UINT32_MAX) /* x32 */
typedef int64_t        kk_intx_t;
typedef uint64_t       kk_uintx_t;
#define KK_IX(i)       KK_I64(i)
#define KK_UX(i)       KK_U64(i)
#define KK_INTX_SIZE   8
#define KK_INTX_MAX    INT64_MAX
#define KK_INTX_MIN    INT64_MIN
#define KK_UINTX_MAX   UINT64_MAX
#define PRIdIX         PRId64
#define PRIuUX         PRIu64
#define PRIxUX         PRIx64
#define PRIXUX         PRIX64
#elif (INT_MAX > INT16_MAX && LONG_MAX == INT32_MAX) || (SIZE_MAX == UINT32_MAX)
typedef int32_t        kk_intx_t;
typedef uint32_t       kk_uintx_t;
#define KK_IX(i)       KK_I32(i)
#define KK_UX(i)       KK_U32(i)
#define KK_INTX_SIZE   4
#define KK_INTX_MAX    INT32_MAX
#define KK_INTX_MIN    INT32_MIN
#define KK_UINTX_MAX   UINT32_MAX
#define PRIdIX         PRId32
#define PRIuUX         PRIu32
#define PRIxUX         PRIx32
#define PRIXUX         PRIX32
#elif (INT_MAX == INT16_MAX)
typedef int            kk_intx_t;
typedef unsigned       kk_uintx_t;
#define KK_IX(i)       i
#define KK_UX(i)       i
#define KK_INTX_SIZE   2
#define KK_INTX_MAX    INT_MAX
#define KK_INTX_MIN    INT_MIN
#define KK_UINTX_MAX   UINT_MAX
#define PRIdIX         "d"
#define PRIuUX         "u"
#define PRIxUX         "x"
#define PRIXUX         "X"
#else
#error "platform cannot be determined to have natural 16, 32, or 64 bit registers"
#endif
#define KK_INTX_BITS   (8*KK_INTX_SIZE)


// a boxed value is by default the size of an `intptr_t`.
#if !defined(KK_INTB_SIZE)
#define KK_INTB_SIZE   KK_INTPTR_SIZE
#endif
#define KK_INTB_BITS   (8*KK_INTB_SIZE)

// define `kk_intb_t` (the integer that can hold a boxed value)
#if (KK_INTB_SIZE == KK_INTPTR_SIZE)
#define KK_COMPRESS 0
typedef intptr_t       kk_intb_t;
typedef uintptr_t      kk_uintb_t;
#define KK_INTB_MAX    INTPTR_MAX
#define KK_INTB_MIN    INTPTR_MIN
#define KK_UINTB_MAX   UINTPTR_MAX
#define KK_IB(i)       KK_IP(i)
#define KK_UB(i)       KK_UP(i)
#define PRIdIB         "zd"
#elif (KK_INTB_SIZE == 8 && KK_INTB_SIZE < KK_INTPTR_SIZE)  
// 128-bit systems with 64-bit compressed pointers
#define KK_COMPRESS 1
typedef int64_t        kk_intb_t;
typedef uint64_t       kk_uintb_t;
#define KK_INTB_MAX    INT64_MAX
#define KK_INTB_MIN    INT64_MIN
#define KK_UINTB_MAX   UINT64_MAX
#define KK_IB(i)       KK_I64(i)
#define KK_UB(i)       KK_U64(i)
#define PRIdIB         PRIdI64
#elif (KK_INTB_SIZE == 4 && KK_INTB_SIZE < KK_INTPTR_SIZE)  
// 64- or 128-bit systems with 32-bit compressed pointers (and a 4*4GiB heap)
#define KK_COMPRESS 1
typedef int32_t        kk_intb_t;
typedef uint32_t       kk_uintb_t;
#define KK_INTB_MAX    INT32_MAX
#define KK_INTB_MIN    INT32_MIN
#define KK_UINTB_MAX   UINT32_MAX
#define KK_IB(i)       KK_I32(i)
#define KK_UB(i)       KK_U32(i)
#define PRIdIB         PRIdI32
#else
#error "the given platform boxed integer size is (currently) not supported"
#endif

#if KK_COMPRESS && !KK_HAS_STRUCT_PACKING
#error "pointer compression can only be used with C compilers that support struct packing"
#endif

// A "field" integer is the largest natural integer that fits into a boxed value
#if (KK_INTB_SIZE > KK_INTX_SIZE)   // ensure it fits the natural register size
typedef kk_intx_t      kk_intf_t;
typedef kk_uintx_t     kk_uintf_t;
#define KK_IF(i)       KK_IX(i)
#define KK_INTF_SIZE   KK_INTX_SIZE
#define KK_INTF_MAX    KK_INTX_MAX
#define KK_INTF_MIN    KK_INTX_MIN
#define KK_UINTF_MAX   KK_UINTX_MAX
#define PRIdIF         PRIdIX
#else
typedef kk_intb_t      kk_intf_t;
typedef kk_uintb_t     kk_uintf_t;
#define KK_IF(i)       KK_IB(i)
#define KK_INTF_SIZE   KK_INTB_SIZE
#define KK_INTF_MAX    KK_INTB_MAX
#define KK_INTF_MIN    KK_INTB_MIN
#define KK_UINTF_MAX   KK_UINTB_MAX
#define PRIdIF         PRIdIB
#endif
#define KK_INTF_BITS   (8*KK_INTF_SIZE)


// Distinguish unsigned shift right and signed arithmetic shift right.
// (Here we assume >> is arithmetic right shift). Avoid UB by always masking the shift.
static inline kk_intx_t   kk_sar(kk_intx_t i, int shift)      { return (i >> (shift & (KK_INTX_BITS - 1))); }
static inline kk_uintx_t  kk_shr(kk_uintx_t u, int shift)     { return (u >> (shift & (KK_INTX_BITS - 1))); }
static inline kk_intf_t   kk_sarf(kk_intf_t i, int shift)     { return (i >> (shift & (KK_INTF_BITS - 1))); }
static inline kk_uintf_t  kk_shrf(kk_uintf_t u, int shift)    { return (u >> (shift & (KK_INTF_BITS - 1))); }
static inline kk_intb_t   kk_sarb(kk_intb_t i, int shift)     { return (i >> (shift & (KK_INTB_BITS - 1))); }
static inline kk_addr_t   kk_sara(kk_addr_t i, int shift)     { return (i >> (shift & (KK_ADDR_BITS - 1))); }

static inline uintptr_t   kk_shrp(uintptr_t u, int shift)     { return (u >> (shift & (KK_INTPTR_BITS - 1))); }
static inline intptr_t    kk_sarp(intptr_t u, int shift)      { return (u >> (shift & (KK_INTPTR_BITS - 1))); }
static inline int32_t     kk_sar32(int32_t i, int32_t shift)  { return (i >> (shift & 31)); }
static inline uint32_t    kk_shr32(uint32_t u, int32_t shift) { return (u >> (shift & 31)); }
static inline int64_t     kk_sar64(int64_t i, int64_t shift)  { return (i >> (shift & 63)); }
static inline uint64_t    kk_shr64(uint64_t u, int64_t shift) { return (u >> (shift & 63)); }

// Avoid UB by left shifting on unsigned integers (and masking the shift).
static inline kk_intx_t   kk_shl(kk_intx_t i, int shift)      { return (kk_intx_t)((kk_uintx_t)i << (shift & (KK_INTX_BITS - 1))); }
static inline kk_intf_t   kk_shlf(kk_intf_t i, int shift)     { return (kk_intf_t)((kk_uintf_t)i << (shift & (KK_INTF_BITS - 1))); }
static inline kk_intb_t   kk_shlb(kk_intb_t i, int shift)     { return (kk_intb_t)((kk_uintb_t)i << (shift & (KK_INTB_BITS - 1))); }
static inline kk_addr_t   kk_shla(kk_addr_t i, int shift)     { return (kk_addr_t)((kk_uaddr_t)i << (shift & (KK_ADDR_BITS - 1))); }
static inline intptr_t    kk_shlp(intptr_t i, int shift)      { return (intptr_t)((uintptr_t)i << (shift & (KK_INTPTR_BITS - 1))); }
static inline int32_t     kk_shl32(int32_t i, int32_t shift)  { return (int32_t)((uint32_t)i << (shift & 31)); }
static inline int64_t     kk_shl64(int64_t i, int64_t shift)  { return (int64_t)((uint64_t)i << (shift & 63)); }



// Architecture assumptions
#define KK_ARCH_LITTLE_ENDIAN   1
//#define KK_ARCH_BIG_ENDIAN       1

// the size of function pointer: `void (*f)(void)`
#define KK_FUNPTR_SIZE    KK_INTPTR_SIZE    


#endif // include guard
