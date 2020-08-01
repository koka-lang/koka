cmake_minimum_required(VERSION 3.8)
project(runtime VERSION 1.0 LANGUAGES C)
enable_testing()

include(CMakePackageConfigHelpers)

# -----------------------------------------------------------------------------
# Language options
# -----------------------------------------------------------------------------

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_C_EXTENSIONS NO)

set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

set(CMAKE_DEBUG_POSTFIX "-debug")

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
target_compile_definitions(runtime PRIVATE RT_STATIC_LIB)
target_include_directories(runtime PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
target_link_libraries(runtime PUBLIC runtime-flags)

install(TARGETS runtime runtime-flags
        EXPORT runtime
        INCLUDES DESTINATION include)

install(EXPORT runtime
        DESTINATION cmake
        FILE runtime-config.cmake)

install(DIRECTORY include/ DESTINATION include FILES_MATCHING PATTERN "*.h*")

write_basic_package_version_file(runtime-config-version.cmake COMPATIBILITY SameMajorVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/runtime-config-version.cmake DESTINATION cmake)

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
      -Wstrict-aliasing>)

# Platform-specific definitions
target_compile_definitions(runtime-flags INTERFACE $<$<PLATFORM_ID:Linux>:_GNU_SOURCE>)

# Architecture flags
if(CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "arm")
  target_compile_options(runtime-flags INTERFACE -march=native)
endif()

# Additional libraries
target_link_libraries(runtime-flags INTERFACE $<$<PLATFORM_ID:Linux,Darwin>:m>)
