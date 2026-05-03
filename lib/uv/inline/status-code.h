#include <uv.h>

#define UV_OK 0
#define kk_uv_status_code_t kk_uv_status_dash_code__uv_status_code

// Map a libuv status code to a Koka Error value with uv status code enum
kk_std_core_exn__error kk_uv_error_from_errno( int err, kk_context_t* ctx );
