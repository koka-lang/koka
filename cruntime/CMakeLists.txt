cmake_minimum_required(VERSION 3.16)
project(runtime C CXX)

enable_testing()

# -----------------------------------------------------------------------------
# Language options
# -----------------------------------------------------------------------------

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_C_EXTENSIONS NO)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

set(CMAKE_CXX_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

# -----------------------------------------------------------------------------
# Convenience: set default build type depending on the build directory
# -----------------------------------------------------------------------------

get_property(isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)

if (NOT isMultiConfig AND NOT CMAKE_BUILD_TYPE)
  if ("${CMAKE_BINARY_DIR}" MATCHES ".*(D|d)ebug$")
    message(STATUS "No build type selected, default to: Debug")
    set(CMAKE_BUILD_TYPE "Debug")
  else()
    message(STATUS "No build type selected, default to: Release")
    set(CMAKE_BUILD_TYPE "Release")
  endif()
endif()

# -----------------------------------------------------------------------------
# Dependencies
# -----------------------------------------------------------------------------

set(THREADS_PREFER_PTHREAD_FLAG YES)
find_package(Threads REQUIRED)

# -----------------------------------------------------------------------------
# Main target
# -----------------------------------------------------------------------------

set(rt_sources
    src/box.c
    src/integer.c
    src/random.c
    src/refcount.c
    src/runtime.c
    src/string.c
    )

# properties library
add_library(runtime-flags INTERFACE)

# library
add_library(runtime STATIC ${rt_sources})
set_target_properties(runtime PROPERTIES OUTPUT_NAME runtime)

target_compile_definitions(runtime PRIVATE RT_STATIC_LIB)
target_include_directories(runtime PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
target_link_libraries(runtime PRIVATE runtime-flags Threads::Threads)

# -----------------------------------------------------------------------------
# Testing
# -----------------------------------------------------------------------------

set(test_sources
    test/main.c
    test/time.c
    )

add_executable(runtime-test ${test_sources})

target_link_libraries(runtime-test PRIVATE runtime runtime-flags)

add_test(NAME runtime-test COMMAND runtime-test)

# -----------------------------------------------------------------------------
# Extended configuration
# -----------------------------------------------------------------------------

# Compiler flags
target_compile_options(
  runtime-flags
  INTERFACE
    $<$<COMPILE_LANG_AND_ID:CXX,AppleClang,Clang,GNU,Intel>:
      -Wall
      -Wextra
      -Wno-unknown-pragmas
      -Wpointer-arith
      -Wshadow
      -Wstrict-aliasing
      -pedantic
      -ftls-model=initial-exec>)

# Architecture flags
if(CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "arm")
  target_compile_options(runtime-flags INTERFACE -march=native)
endif()

# Additional libraries
target_link_libraries(runtime-flags INTERFACE $<$<PLATFORM_ID:Linux,Darwin>:m>)
