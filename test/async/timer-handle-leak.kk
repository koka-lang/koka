// Regression test: a `timeout(d, action)` should release both timer handles
// (the wait-side and the action-side) by the time the event loop closes.
//
// Previously the cancelled strand's `clear-timeout` leaked because
// `kk_clear_timeout` unboxed the timer with `KK_OWNED` but `kk_timer_stop`
// only borrows it (`^t`), so the unboxed timer was never dropped — leaving
// `Event loop closed with open child handles` warnings.
//
// On the natural-completion side the disposer captured by `cancel-hook`
// in `handle-await` was never released (the ref kept holding it), so its
// captured `tid` stayed alive for the lifetime of the await machinery.
//
// If either bug regresses, libuv prints "Event loop closed with open
// child handles" on stderr; this test fails because the expected output
// is `done` and nothing else.
import std/async
import std/time/duration

fun main()
  match timeout(100.milli-seconds, { wait(50.milli-seconds); 1 })
    Nothing -> println("timed out")
    Just(n) -> println("waited for " ++ show(n))
