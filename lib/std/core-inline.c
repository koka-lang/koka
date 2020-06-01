/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

datatype_t vector_to_list(vector_t v, datatype_t tail, context_t* ctx) {
  // todo: avoid boxed_dup if v is unique
  size_t n;
  box_t* p = vector_buf(v, &n);
  if (n == 0) {
    vector_drop(v,ctx);
    return tail;
  }
  datatype_t nil  = __std_core__new_Nil(ctx);
  struct __std_core_Cons* cons = NULL;
  datatype_t list;  
  for( size_t i = 0; i < n; i++ ) {
    datatype_t hd = __std_core__new_Cons(boxed_dup(p[i]), nil, ctx);
    if (cons==NULL) {
      list = hd;
    }
    else {
      cons->tail = hd;
    }    
    cons = __std_core__as_Cons(hd);
  }
  cons->tail = tail;
  vector_drop(v,ctx);
  return list;
}

vector_t list_to_vector(datatype_t xs, context_t* ctx) {
  // todo: avoid boxed_dup if xs is unique
  // find the length
  uint_t len = 0;
  __std_core__list ys = xs;
  while (__std_core__is_Cons(ys)) {
    struct __std_core_Cons* cons = __std_core__as_Cons(ys);    
    len++;
    ys = cons->tail;
  }
  // alloc the vector and copy
  vector_t v = vector_alloc(len,0,ctx);
  box_t* p = vector_buf(v,NULL);
  ys = xs;
  for( uint_t i = 0; i < len; i++) {
    struct __std_core_Cons* cons = __std_core__as_Cons(ys);        
    ys = cons->tail;
    p[i] = boxed_dup(cons->head);
  }
  return v;
}


datatype_t string_to_list(string_t s, context_t* ctx) {
  const uint8_t* p = string_buf_borrow(s);
  struct __std_core_Cons* cons = NULL;
  datatype_t nil  = __std_core__new_Nil(ctx);
  datatype_t list = nil;
  size_t count;
  char_t c;
  while( (c = utf8_read(p,&count), count != 0) ) {
    p += count;
    cons->tail = __std_core__new_Cons(box_char_t(c,ctx), nil, ctx);
    cons = datatype_data_as(struct __std_core_Cons, cons->tail);
  }
  cons->tail = nil;
  return list;
}

string_t string_from_list(datatype_t cs, context_t* ctx) {
  // TODO: optimize for short strings to write directly into a local buffer?
  // find total UTF8 length
  uint_t len = 0;
  __std_core__list xs = cs;
  while (__std_core__is_Cons(xs)) {
    struct __std_core_Cons* cons = __std_core__as_Cons(xs);    
    len += utf8_len(unbox_char_t(cons->head,ctx));
    xs = cons->tail;
  }
  // allocate and copy the characters
  string_t s = string_alloc_len(len,0,ctx);
  uint8_t* p = (uint8_t*)string_buf_borrow(s);
  xs = cs;
  while (__std_core__is_Cons(xs)) {
    struct __std_core_Cons* cons = __std_core__as_Cons(xs);    
    size_t count;
    utf8_write( unbox_char_t(cons->head,ctx), p, &count );
    p += count;
    xs = cons->tail;
  }
  assert_internal(*p == 0);
  datatype_drop(cs,ctx);
  return s;
}

static inline void sslice_start_end_borrow( __std_core__sslice sslice, const uint8_t** start, const uint8_t** end) {
  const uint8_t* s = string_buf_borrow(sslice.str);
  *start = s + sslice.start;
  *end = s + sslice.start + sslice.len;
}

integer_t slice_count( __std_core__sslice sslice, context_t* ctx ) {
  // TODO: optimize this by extending string_count 
  const uint8_t* start;
  const uint8_t* end; 
  sslice_start_end_borrow(sslice, &start, &end);
  size_t count = 0;
  while( start < end && *start != 0 ) {
    const char* next = utf8_next(start);
    count++;
    start = next;
  }
  return integer_from_size_t(count,ctx);
} 

string_t slice_to_string( __std_core__sslice  sslice, context_t* ctx ) {
  const uint8_t* start;
  const uint8_t* end; 
  sslice_start_end_borrow(sslice, &start, &end);
  // is it the full string?
  if (sslice.start == 0 && sslice.len == string_len(string_dup(sslice.str))) { // todo: limit strlen search to len
    // TODO: drop sslice?    
    return string_dup(sslice.str);
  }
  else {
    // if not, we copy
    string_t s = string_alloc_len(sslice.len, start, ctx);
    // TODO: drop sslice?
    return s;
  }
}

__std_core__sslice slice_first( string_t str, context_t* ctx ) {
  const uint8_t* s = string_buf_borrow(str);
  const uint8_t* next = utf8_next(s);
  return __std_core__new_Sslice(str, 0, (int32_t)(next - s), ctx);
}

__std_core__sslice slice_last( string_t str, context_t* ctx ) {
  const uint8_t* s = string_buf_borrow(str);
  const uint8_t* end = s + string_len(string_dup(str));
  const uint8_t* prev = (s==end ? s : utf8_prev(end));
  return __std_core__new_Sslice(str, (int32_t)(end - s), (int32_t)(end - prev), ctx);
}
