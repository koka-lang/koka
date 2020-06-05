cmake_minimum_required(VERSION 3.0)
project(runtime C CXX)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

option(RT_USE_CXX           "Use the C++ compiler to compile the library (instead of the C compiler)" OFF)
option(RT_SEE_ASM           "Generate assembly files" OFF)

set(rt_sources
    src/integer.c
    src/refcount.c
    src/runtime.c
    src/string.c)

# -----------------------------------------------------------------------------
# Converience: set default build type depending on the build directory
# -----------------------------------------------------------------------------

if (NOT CMAKE_BUILD_TYPE)
  if ("${CMAKE_BINARY_DIR}" MATCHES ".*(D|d)ebug$" OR  MI_DEBUG_FULL MATCHES "ON")
    message(STATUS "No build type selected, default to: Debug")
    set(CMAKE_BUILD_TYPE "Debug")
  else()
    message(STATUS "No build type selected, default to: Release")
    set(CMAKE_BUILD_TYPE "Release")
  endif()
endif()


# -----------------------------------------------------------------------------
# Process options
# -----------------------------------------------------------------------------

if(CMAKE_C_COMPILER_ID MATCHES "MSVC|Intel")
  set(RT_USE_CXX "ON")
endif()

if(RT_SEE_ASM MATCHES "ON")
  message(STATUS "Generate assembly listings (RT_SEE_ASM=ON)")
  list(APPEND rt_cflags -save-temps)
endif()

if(RT_USE_CXX MATCHES "ON")
  message(STATUS "Use the C++ compiler to compile (RT_USE_CXX=ON)")
  set_source_files_properties(${rt_sources} PROPERTIES LANGUAGE CXX )
  set_source_files_properties(test/main.c test/time.c PROPERTIES LANGUAGE CXX )
  if(CMAKE_CXX_COMPILER_ID MATCHES "AppleClang|Clang")
    list(APPEND rt_cflags -Wno-deprecated)
  endif()
  if(CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    list(APPEND rt_cflags -Kc++)
  endif()
endif()

# Compiler flags
if(CMAKE_C_COMPILER_ID MATCHES "AppleClang|Clang|GNU|Intel")
  list(APPEND rt_cflags -Wall -Wextra -Wno-unknown-pragmas -fstrict-aliasing -fvisibility=hidden -ftls-model=initial-exec)    
endif()

# Architecture flags
if(${CMAKE_HOST_SYSTEM_PROCESSOR} MATCHES "arm")
    list(APPEND rt_cflags -march=native)
endif()

# extra needed libraries
if(WIN32)
  list(APPEND rt_libraries psapi shell32 user32 bcrypt)
else()
  if(NOT ${CMAKE_C_COMPILER} MATCHES "android")
    list(APPEND rt_libraries pthread m)  # pthreads and math
    find_library(LIBRT rt)               # realtime
    if(LIBRT)
      list(APPEND rt_libraries ${LIBRT})
    endif()
  endif()
endif()

# -----------------------------------------------------------------------------
# Install and output names
# -----------------------------------------------------------------------------

set(rt_basename runtime)

message(STATUS "")
message(STATUS "Library base name: ${rt_basename}")
message(STATUS "Build type       : ${CMAKE_BUILD_TYPE_LC}")
message(STATUS "")

# -----------------------------------------------------------------------------
# Main targets
# -----------------------------------------------------------------------------

# library
add_library(runtime STATIC ${rt_sources})
target_compile_definitions(runtime PRIVATE ${rt_defines} RT_STATIC_LIB)
target_compile_options(runtime PRIVATE ${rt_cflags})
target_link_libraries(runtime PUBLIC ${rt_libraries})
target_include_directories(runtime PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
set_target_properties(runtime PROPERTIES OUTPUT_NAME runtime)
# install(TARGETS mimalloc-static EXPORT mimalloc DESTINATION ${mi_install_dir})

# -----------------------------------------------------------------------------
# Testing
# -----------------------------------------------------------------------------

add_executable(runtime-test test/main.c test/time.c)
target_compile_definitions(runtime-test PRIVATE ${rt_defines})
target_compile_options(runtime-test PRIVATE ${rt_cflags})
target_include_directories(runtime-test PRIVATE include)
target_link_libraries(runtime-test PRIVATE runtime ${rt_libraries})
