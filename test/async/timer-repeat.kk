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
//  - disposing before the event loop starts yields zero invocations, and
//    disposing twice does not call `uv_close` twice on the same handle.
//
// If a handle leaks, the event loop reports `event loop closed with open
// child handles` on stderr when the program exits.
import std/async/api/uv/evloop
import std/num/int64

fun main()
  // 1. a repeating handle invokes its callback exactly three times and can
  //    be disposed from the third callback
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

  // 2. disposing before the event loop starts produces zero invocations;
  //    disposing twice must not close the same handle twice
  val loop2 = event-loop-init()
  val count2 = ref(0)
  val dispose2 = ref( (fn() () : () -> ioc ()) )
  match timer-setup-repeat(loop2, 10.int64, fn()
    count2 := !count2 + 1
  )
    Ok(d)       -> dispose2 := d
    Error(exn)  -> println("setup2 error: " ++ show(exn))
  (!dispose2)()
  (!dispose2)()
  event-loop-run(loop2)
  println("disposed count: " ++ show(!count2))
