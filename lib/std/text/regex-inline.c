

/*---------------------------------------------------------------------------
  Copyright 2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

#pragma clang diagnostic ignored "-Wdisabled-macro-expansion"
#define PCRE2_CODE_UNIT_WIDTH 8
#define PCRE2_STATIC
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
  kk_free(p,kk_get_context());
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


/* -----------------------------------------------------------------------
  Compile
------------------------------------------------------------------------*/

static void kk_regex_free( void* pre, kk_block_t* b, kk_context_t* ctx ) {
  kk_unused(ctx);
  pcre2_code* re = (pcre2_code*)pre;
  //kk_info_message( "free regex at %p\n", re );
  if (re != NULL) { pcre2_code_free(re); }
}

#define KK_REGEX_OPTIONS  (PCRE2_ALT_BSUX | PCRE2_EXTRA_ALT_BSUX | PCRE2_MATCH_UNSET_BACKREF /* javascript compat */ \
                          | PCRE2_NEVER_BACKSLASH_C | PCRE2_NEVER_UCP | PCRE2_UTF /* utf-8 safety */ \
                          )

static kk_box_t kk_regex_create( kk_string_t pat, bool ignore_case, bool multi_line, kk_context_t* ctx ) {
  kk_ssize_t len;
  const uint8_t* cpat = kk_string_buf_borrow( pat, &len, ctx );
  PCRE2_SIZE errofs = 0;
  int        errnum = 0;
  uint32_t   options = KK_REGEX_OPTIONS;
  if (ignore_case) options |= PCRE2_CASELESS;
  if (multi_line)  options |= PCRE2_MULTILINE;
  pcre2_code* re = pcre2_compile( cpat, PCRE2_ZERO_TERMINATED, options, &errnum, &errofs, cmp_ctx);
  //kk_info_message( "create regex: err:%i, at %p\n", (re==NULL ? 0 : errnum), re );
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

static kk_std_core__list kk_regex_exec_ex( pcre2_code* re, pcre2_match_data* match_data, 
                                           kk_string_t str_borrow, const uint8_t* cstr, kk_ssize_t len, bool allow_empty, 
                                           kk_ssize_t start, kk_ssize_t* mstart, kk_ssize_t* end, int* res, kk_context_t* ctx ) 
{
  // match
  kk_std_core__list hd  = kk_std_core__new_Nil(ctx);
  uint32_t options = 0;
  if (!allow_empty) options |= (PCRE2_NOTEMPTY_ATSTART | PCRE2_ANCHORED);
  int rc = pcre2_match( re, cstr, len, start, options, match_data, match_ctx );
  if (res != NULL) *res = rc;    
  if (rc > 0) {    
    // extract captures
    uint32_t    gcount = pcre2_get_ovector_count(match_data);
    PCRE2_SIZE* groups = pcre2_get_ovector_pointer(match_data);
    for( uint32_t i = gcount; i > 0; ) {
      i--;
      kk_ssize_t sstart = groups[i*2];       // on no-match, sstart and send == -1.
      kk_ssize_t send   = groups[i*2 + 1];
      kk_assert(send >= sstart);
      kk_std_core__sslice sslice = kk_std_core__new_Sslice( kk_string_dup(str_borrow,ctx), kk_integer_from_ssize_t(sstart,ctx), kk_integer_from_ssize_t(send - sstart,ctx), ctx ); 
      hd = kk_std_core__new_Cons(kk_reuse_null,kk_std_core__sslice_box(sslice,ctx), hd, ctx);
      if (i == 0) {
        if (mstart != NULL) { *mstart = sstart; }
        if (end    != NULL) { *end = send; }
      }
    }
  }
  return hd;
}


static kk_std_core__list kk_regex_exec( kk_box_t bre, kk_string_t str, kk_ssize_t start, kk_context_t* ctx ) 
{
  // unpack
  pcre2_match_data* match_data = NULL;
  kk_std_core__list res = kk_std_core__new_Nil(ctx);
  pcre2_code* re = (pcre2_code*)kk_cptr_raw_unbox(bre,ctx);
  kk_ssize_t len = 0;
  const uint8_t* cstr = NULL;
  if (re == NULL) goto done;    
  match_data = pcre2_match_data_create_from_pattern(re, gen_ctx);
  if (match_data==NULL) goto done;  
  cstr = kk_string_buf_borrow(str, &len, ctx );  

  // and match
  res = kk_regex_exec_ex( re, match_data, str, cstr, len, true, start, NULL, NULL, NULL, ctx );

done:  
  if (match_data != NULL) {
    //kk_info_message( "free match data: %p\n", match_data );
    pcre2_match_data_free(match_data);
  }
  kk_string_drop(str,ctx);
  kk_box_drop(bre,ctx);
  return res;
}

static kk_std_core__list kk_regex_exec_all( kk_box_t bre, kk_string_t str, kk_ssize_t start, kk_ssize_t atmost, kk_context_t* ctx ) {
  // unpack
  if (atmost < 0) atmost = KK_SSIZE_MAX;
  pcre2_match_data* match_data = NULL;
  kk_std_core__list res = kk_std_core__new_Nil(ctx);
  pcre2_code* re = (pcre2_code*)kk_cptr_raw_unbox(bre,ctx);
  if (re == NULL) goto done;    
  match_data = pcre2_match_data_create_from_pattern(re, gen_ctx);
  if (match_data==NULL) goto done;  
  {
    kk_ssize_t len;
    const uint8_t* cstr = kk_string_buf_borrow(str, &len, ctx );  

    // and match
    kk_std_core__list* tail = NULL;
    bool allow_empty = true;
    int rc = 1;    
    kk_ssize_t next = start;
    while( rc > 0 && start < len && atmost > 0) {
      atmost--;
      rc = 0;
      kk_ssize_t mstart = start;
      kk_std_core__list cap = kk_regex_exec_ex( re, match_data, str, cstr, len, allow_empty, start, &mstart, &next, &rc, ctx );
      if (rc > 0) {
        // found a match; 
        // push string up to match, and the actual matched regex
        kk_std_core__sslice pre = kk_std_core__new_Sslice( kk_string_dup(str,ctx), kk_integer_from_ssize_t(start,ctx), kk_integer_from_ssize_t(mstart - start,ctx), ctx ); 
        kk_std_core__list   prelist = kk_std_core__new_Cons( kk_reuse_null, kk_std_core__sslice_box(pre,ctx), kk_std_core__new_Nil(ctx), ctx );
        kk_std_core__list   capcons = kk_std_core__new_Cons( kk_reuse_null, kk_std_core__list_box(cap,ctx), kk_std_core__new_Nil(ctx) /*tail*/, ctx );
        kk_std_core__list   cons = kk_std_core__new_Cons( kk_reuse_null, kk_std_core__list_box(prelist,ctx), capcons, ctx );
        if (tail==NULL) res = cons;
                  else *tail = cons;
        tail = &kk_std_core__as_Cons(capcons,ctx)->tail;
        allow_empty = (next > start);
        start = next;
      }
      else if (rc <= 0 && !allow_empty) {
        // skip one character and try again
        // todo: handle cr/lf pairs better?
        const uint8_t* p = kk_utf8_next( cstr + start );
        start = (p - cstr);
        allow_empty = true;
        rc = 1;
      }
    }
    
    // push final string part as well and end the list
    kk_std_core__sslice post    = kk_std_core__new_Sslice( kk_string_dup(str,ctx), kk_integer_from_ssize_t(next,ctx), kk_integer_from_ssize_t(len - next,ctx), ctx ); 
    kk_std_core__list   postlist= kk_std_core__new_Cons( kk_reuse_null, kk_std_core__sslice_box(post,ctx), kk_std_core__new_Nil(ctx), ctx );
    kk_std_core__list   cons    = kk_std_core__new_Cons( kk_reuse_null, kk_std_core__list_box(postlist,ctx), kk_std_core__new_Nil(ctx), ctx );
    if (tail==NULL) res = cons;
              else *tail = cons;  
  }

done:  
  if (match_data != NULL) {
    //kk_info_message( "free match data: %p\n", match_data );
    pcre2_match_data_free(match_data);
  }
  kk_string_drop(str,ctx);
  kk_box_drop(bre,ctx);
  return res;
}

/*
kk_std_core__sslice kk_slice_upto( struct kk_std_core_Sslice slice1, struct kk_std_core_Sslice slice2, kk_context_t* ctx ) {
  kk_ssize_t start = slice1.start;
  kk_ssize_t len   = (slice1.start <= slice2.start ? slice2.start - slice1.start : -1);
  return kk_std_core__new_Sslice(slice1.str, start, len, ctx);
}
*/