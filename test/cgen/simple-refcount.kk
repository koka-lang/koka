import std/num/int32

extern create-box(): io-noexn any
  c inline "kk_cptr_raw_box(&kk_free_fun, kk_malloc(sizeof(int), _ctx), _ctx)"

extern get-refcount(^box: any): io-noexn int32
  c inline "kk_block_refcount(kk_box_to_ptr(box, _ctx))"

effect ctl catch(): ()

fun main()
  with handler ctl catch() 
    resume(())
    resume(())
  val box = create-box()
  val count0 = box.get-refcount()
  catch()
  val count1 = box.get-refcount()
  println((count0.int, count1.int).show)
  // Expected output: 
  // (1,1) 
  // (1,0) 