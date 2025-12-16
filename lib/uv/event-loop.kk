
module uv/event-loop

pub import std/time/duration
pub import std/time/timestamp
pub import uv/timer
pub import std/async/async
import std/num/int32
import uv/utils

extern import
  c { conan="libuv[>=1.47.0]"; vcpkg="libuv"; library="uv" }

extern import
  c file "inline/event-loop.c"

// Sets a timeout for libuv / javascript / C# event loops
pub extern set-timeout( cb : () -> io-noexn (), ms : int32 ) : io-noexn any
  js "setTimeout"
  c "kk_set_timeout"

// Clears a timeout for libuv / javascript / C# event loops
pub extern clear-timeout( tid : any) : io-noexn ()
  js "clearTimeout"
  c "kk_clear_timeout"

// Runs an async action on the default uv event loop, and exceptions at the top level exit the async loop.
pub fun default-async-uv(action: () -> <async,io> a): io a
  val result = ref(Error(Exception("Unreachable", ExnInternal("Unreachable"))))
  val _ =
    with default-event-loop
    with @default-async
    result := try(action)
  (!result).untry

// Handles a uv loop
pub fun default-event-loop(action)
  if host() == "libc" then
    handle-loop(action)
  else // TODO: Support event loop on other platforms
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
