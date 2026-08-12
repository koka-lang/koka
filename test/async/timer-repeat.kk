// Regression tests for repeated libuv handle callbacks
// (`kk_uv_handle_callback_repeat`).
//
// The one-shot `kk_uv_handle_callback` consumes the callback stored in the
// handle `data` field and closes the handle. Repeating libuv operations
// (like `uv_listen`, `uv_read_start`, fs watchers, or a repeating timer)
// invoke their callback multiple times: `kk_uv_handle_callback_repeat`
// duplicates the stored callback for each invocation, keeps the handle open,
// and ignores pending invocations once the handle is closed or disposed.
//
// Here we exercise the lifecycle with a repeating timer:
//  - the callback fires exactly three times and disposes the timer from
//    inside the third invocation (no use-after-free);
//  - disposing before the event loop starts yields zero invocations;
//  - the repeat *interval* (not a plain "repeat" flag) is honored: a 100ms
//    repeating timer stopped by a one-shot timer at 150ms fires exactly once.
//
// The disposal function returned by `timer-setup-repeat` is single-use: it
// consumes the handle and the stored callback on its first call (the Koka
// runtime drops the function itself), so `uv_close` can never be applied to
// an already-closed handle from the same disposal function. The
// `uv_is_closing` guard in `kk_uv_handle_close` additionally protects against
// double `uv_close` from concurrent close paths while the handle is still
// alive (i.e. between handle init and the close callback).
//
// If a handle leaks, the event loop reports `event loop closed with open
// child handles` on stderr when the program exits.
import std/async/api/uv/evloop
import std/num/int64

fun main()
  // 1. a repeating handle invokes its callback exactly three times (at 10ms,
  //    20ms and 30ms) and can be disposed from the third callback
  val loop1 = event-loop-init()
  val count1 = ref(0)
  val dispose1 = ref( (fn() () : () -> ioc ()) )
  match timer-setup-repeat(loop1, 10.int64, fn()
    count1 := !count1 + 1
    if !count1 == 3 then (!dispose1)()
  )
    Ok(d)       -> dispose1 := d
    Error(exn)  -> println("setup1 error: " ++ show(exn))
  event-loop-run(loop1)
  println("repeat count: " ++ show(!count1))

  // 2. disposing before the event loop starts produces zero invocations
  val loop2 = event-loop-init()
  val count2 = ref(0)
  val dispose2 = ref( (fn() () : () -> ioc ()) )
  match timer-setup-repeat(loop2, 10.int64, fn()
    count2 := !count2 + 1
  )
    Ok(d)       -> dispose2 := d
    Error(exn)  -> println("setup2 error: " ++ show(exn))
  (!dispose2)()
  event-loop-run(loop2)
  println("disposed count: " ++ show(!count2))

  // 3. the repeat interval is respected: a 100ms repeating timer disposed by
  //    a one-shot timer at 150ms fires exactly once (with a bogus 1ms repeat
  //    interval this would fire ~50 times instead)
  val loop3 = event-loop-init()
  val count3 = ref(0)
  val dispose3 = ref( (fn() () : () -> ioc ()) )
  match timer-setup-repeat(loop3, 100.int64, fn()
    count3 := !count3 + 1
  )
    Ok(d)       -> dispose3 := d
    Error(exn)  -> println("setup3 error: " ++ show(exn))
  ignore(timer-setup(loop3, 150.int64, fn()
    (!dispose3)()
  ))
  event-loop-run(loop3)
  println("single count: " ++ show(!count3))