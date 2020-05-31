/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

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
