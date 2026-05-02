
module uv/event-loop

pub import std/time/duration
pub import std/time/timestamp
pub import uv/timer
pub import std/async/async
import uv/utils

extern import
  c { conan="libuv[>=1.47.0]"; vcpkg="libuv"; library="uv" }

extern import
  c file "inline/event-loop.c"

// Runs an async action on the default uv/js/wasm event loop.
// Propagates top-level exceptions thrown by the action.
pub fun default-async-uv(action: () -> <async,io-noexn> ()): io ()   
  val initial-result = 
        if host() == "libc" then
          Error(Exception("libuv event loop returned without fully executing koka code (missing callback or deadlock?)", ExnInternal("Unreachable")))
        else Ok(()) // Javascript always will return immediately after running the action, since we cannot wait for the end of the event loop, so we return Ok by default
  val result = ref(initial-result)
  val _ =
    with default-event-loop
    with @default-async
    result := try(action)
  (!result).untry
    
// Handles a uv loop
pub fun default-event-loop(action)
  if host() == "libc" then
    handle-loop(action)
  else // Javascript doesn't have an api to start or wait for the end of the loop
    action()

// Configures uv with Koka's allocators when importing this file
val @initialize = init-uv-alloc()

// Runs a UV loop
fun handle-loop(action)
  init-loop()
  val res = action()
  run-loop()
  close-loop()
  res

// Runs a uv loop (or the emscripten loop on wasm)
extern run-loop(): io-noexn ()
  c "kk_async_loop_run"
  js inline ""

// Initializes a uv loop
extern init-loop(): io-noexn ()
  c "kk_async_loop_init"
  js inline ""

// initializes only the allocators (not an event loop)
// needed for some file operations which alloc - kk_uv_fs_mkdtemp, kk_uv_fs_mkstemp
pub extern init-uv-alloc(): ()
  c "kk_async_alloc_init"
  js inline ""

// Closes a uv loop
extern close-loop(): io-noexn ()
  c "kk_async_loop_close"
  js inline ""
