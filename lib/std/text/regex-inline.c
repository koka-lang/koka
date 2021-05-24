

/*---------------------------------------------------------------------------
  Copyright 2021 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

#pragma clang diagnostic ignored "-Wdisabled-macro-expansion"
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

/* -----------------------------------------------------------------------
  Init/Done
------------------------------------------------------------------------*/
#define KK_CUSTOM_INIT  kk_regex_custom_init
#define KK_CUSTOM_DONE  kk_regex_custom_done

static pcre2_general_context* gen_ctx;
static pcre2_compile_context* cmp_ctx;
static pcre2_match_context*   match_ctx; 

static void* kk_pcre2_malloc( PCRE2_SIZE size, void* data ) {
  return kk_malloc( kk_to_ssize_t(size), kk_get_context() );  
}
static void kk_pcre2_free( void* p, void* data ) {
  return kk_free(p);
}

static void kk_regex_custom_init( kk_context_t* ctx ) {
  gen_ctx = pcre2_general_context_create( &kk_pcre2_malloc, &kk_pcre2_free, NULL );
  if (gen_ctx != NULL) {
    match_ctx = pcre2_match_context_create( gen_ctx );
    cmp_ctx = pcre2_compile_context_create( gen_ctx );
    if (cmp_ctx != NULL) {
      pcre2_set_newline( cmp_ctx, PCRE2_NEWLINE_ANYCRLF );
      pcre2_set_bsr( cmp_ctx, PCRE2_BSR_ANYCRLF );
    }
  }
}

static void kk_regex_custom_done( kk_context_t* ctx ) {
  if (cmp_ctx != NULL) {
    pcre2_compile_context_free(cmp_ctx);
    cmp_ctx = NULL;
  }
  if (match_ctx != NULL) {
    pcre2_match_context_free(match_ctx);
    match_ctx = NULL;
  }
  if (gen_ctx != NULL) {
    pcre2_general_context_free(gen_ctx);
    gen_ctx = NULL;
  }
}

static kk_string_t kk_regex_version(kk_context_t* ctx) {
  char ver[256];
  int res = pcre2_config(PCRE2_CONFIG_VERSION, &ver);
  return kk_string_alloc_from_qutf8(ver, ctx);
}


/* -----------------------------------------------------------------------
  Compile
------------------------------------------------------------------------*/

static void kk_regex_free( void* pre, kk_block_t* b ) {
  pcre2_code* re = (pcre2_code*)pre;
  kk_info_message( "free regex at %p\n", re );
  if (re != NULL) pcre2_code_free(re);
}

#define KK_REGEX_OPTIONS  (PCRE2_ALT_BSUX | PCRE2_EXTRA_ALT_BSUX | PCRE2_MATCH_UNSET_BACKREF /* javascript compat */ \
                          | PCRE2_NEVER_BACKSLASH_C | PCRE2_NEVER_UCP | PCRE2_UTF /* utf-8 safety */ \
                          )

static kk_box_t kk_regex_create( kk_string_t pat, int32_t ignore_case, int32_t multi_line, kk_context_t* ctx ) {
  kk_ssize_t len;
  const uint8_t* cpat = kk_string_buf_borrow( pat, &len );
  PCRE2_SIZE errofs = 0;
  int        errnum = 0;
  uint32_t   options = KK_REGEX_OPTIONS;
  if (ignore_case != 0) options |= PCRE2_CASELESS;
  if (multi_line != 0)  options |= PCRE2_MULTILINE;
  pcre2_code* re = pcre2_compile( cpat, PCRE2_ZERO_TERMINATED, options, &errnum, &errofs, cmp_ctx);
  kk_info_message( "create regex: err:%i, at %p\n", (re==NULL ? 0 : errnum), re );
  kk_string_drop(pat,ctx);
  return kk_cptr_raw_box( &kk_regex_free, re, ctx );
}


/* -----------------------------------------------------------------------
  Match
------------------------------------------------------------------------*/

/*
static void kk_match_data_free( void* pmd, kk_block_t* b ) {
  kk_info_message( "free match_data\n" );
  pcre2_match_data* md = (pcre2_match_data*)pmd;
  if (md != NULL) pcre2_match_data_free(md);
}
*/

static kk_std_core__list kk_regex_exec( kk_box_t bre, kk_string_t str, kk_ssize_t start, kk_context_t* ctx ) {
  // match
  pcre2_match_data* match_data = NULL;
  kk_std_core__list hd  = kk_std_core__new_Nil(ctx);
  pcre2_code* re = (pcre2_code*)kk_cptr_raw_unbox(bre);
  if (re == NULL) goto done;    
  match_data = pcre2_match_data_create_from_pattern(re, gen_ctx);
  kk_info_message( "create match data: %p\n", match_data );
  if (match_data==NULL) goto done;  
  kk_ssize_t len;
  const uint8_t* cstr = kk_string_buf_borrow(str, &len );  
  kk_info_message( "match...\n" );
  int rc = pcre2_match( re, cstr, len, start, 0 /* options */, match_data, match_ctx );
  if (rc <= 0) {
    goto done;
  }
  // extract captures
  uint32_t    gcount = pcre2_get_ovector_count(match_data);
  PCRE2_SIZE* groups = pcre2_get_ovector_pointer(match_data);
  for( uint32_t i = gcount; i > 0; ) {
    i--;
    kk_ssize_t sstart = kk_to_ssize_t(groups[i*2]);
    kk_ssize_t send   = kk_to_ssize_t(groups[i*2 + 1]);
    kk_assert_internal(end >= start);
    kk_std_core__sslice sslice = kk_std_core__new_Sslice( kk_string_dup(str), sstart, send - sstart, ctx ); 
    hd = kk_std_core__new_Cons(kk_reuse_null,kk_std_core__sslice_box(sslice,ctx), hd, ctx);
  }

done:  
  if (match_data != NULL) {
    kk_info_message( "free match data: %p\n", match_data );
    pcre2_match_data_free(match_data);
  }
  kk_string_drop(str,ctx);
  kk_box_drop(bre,ctx);
  return hd;
}
