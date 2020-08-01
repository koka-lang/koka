cmake_minimum_required(VERSION 3.8)
project(runtime C)
include(cmake/runtime-config-version.cmake)
enable_testing()

# -----------------------------------------------------------------------------
# Language options
# -----------------------------------------------------------------------------

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_C_EXTENSIONS NO)

set(CMAKE_C_VISIBILITY_PRESET hidden)
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

set(rt_basename "runtime")
string(TOLOWER "${CMAKE_BUILD_TYPE}" CMAKE_BUILD_TYPE_LC)
if(NOT(CMAKE_BUILD_TYPE_LC MATCHES "^(release|relwithdebinfo|minsizerel)$"))
  set(rt_basename "${rt_basename}-${CMAKE_BUILD_TYPE_LC}") #append build type (e.g. -debug) if not a release version
endif()

# -----------------------------------------------------------------------------
# Dependencies
# -----------------------------------------------------------------------------

message(STATUS "Library base name: ${rt_basename}")
message(STATUS "Build type       : ${CMAKE_BUILD_TYPE_LC}")


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
set_target_properties(runtime PROPERTIES VERSION ${rt_version} OUTPUT_NAME ${rt_basename})
target_compile_definitions(runtime PRIVATE RT_STATIC_LIB)
target_include_directories(runtime PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include> $<INSTALL_INTERFACE:include>)
target_link_libraries(runtime PUBLIC runtime-flags)

install(TARGETS runtime runtime-flags EXPORT runtime)
install(EXPORT runtime DESTINATION cmake)
install(DIRECTORY include/ DESTINATION include FILES_MATCHING PATTERN "*.h*")
install(FILES cmake/runtime-config.cmake DESTINATION cmake)
install(FILES cmake/runtime-config-version.cmake DESTINATION cmake)

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
    $<$<COMPILE_LANG_AND_ID:C,AppleClang,Clang,GNU,Intel>:
      -Wall
      -Wextra
      -Wno-unknown-pragmas
      -Wno-unused-parameter
      -Wno-missing-field-initializers
      -Wpointer-arith
      -Wshadow
      -Wstrict-aliasing
      -ftls-model=initial-exec>)

# Platform-specific definitions
target_compile_definitions(runtime-flags INTERFACE $<$<PLATFORM_ID:Linux>:_GNU_SOURCE>)

# Architecture flags
if(CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "arm")
  target_compile_options(runtime-flags INTERFACE -march=native)
endif()

# Additional libraries
target_link_libraries(runtime-flags INTERFACE $<$<PLATFORM_ID:Linux,Darwin>:m>)
