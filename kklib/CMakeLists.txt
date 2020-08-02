cmake_minimum_required(VERSION 3.8)
project(kklib VERSION 1.0 LANGUAGES C)
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
# Main target
# -----------------------------------------------------------------------------

set(kklib_sources
    src/bits.c
    src/box.c
    src/integer.c
    src/random.c
    src/refcount.c
    src/init.c
    src/string.c
    )

# properties library
add_library(kklib-flags INTERFACE)

# library
add_library(kklib STATIC ${kklib_sources})
target_compile_definitions(kklib PRIVATE KKLIB_STATIC_LIB)
target_include_directories(kklib PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
target_link_libraries(kklib PUBLIC kklib-flags)

install(TARGETS kklib kklib-flags EXPORT kklib INCLUDES DESTINATION include)
install(EXPORT kklib DESTINATION cmake FILE kklib-config.cmake)
install(DIRECTORY include/ DESTINATION include FILES_MATCHING PATTERN "*.h*")

write_basic_package_version_file(kklib-config-version.cmake COMPATIBILITY SameMajorVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/kklib-config-version.cmake DESTINATION cmake)


# -----------------------------------------------------------------------------
# Testing
# -----------------------------------------------------------------------------

set(test_sources
    test/main.c
    test/time.c
    )

add_executable(kklib-test ${test_sources})
target_link_libraries(kklib-test PRIVATE kklib kklib-flags)
add_test(NAME kklib-test COMMAND kklib-test)


# -----------------------------------------------------------------------------
# Extended configuration
# -----------------------------------------------------------------------------

# Compiler flags
target_compile_options(kklib-flags INTERFACE 
   $<$<COMPILE_LANG_AND_ID:C,AppleClang,Clang,GNU,Intel>:
      -Wall
      -Wextra
      -Wno-unknown-pragmas
      -Wno-unused-parameter
      -Wno-unused-variable
      -Wno-unused-value
      -Wno-unused-but-set-variable
      -Wno-missing-field-initializers
      -Wpointer-arith
      -Wshadow
      -Wstrict-aliasing>)

# Platform-specific definitions
target_compile_definitions(kklib-flags INTERFACE $<$<PLATFORM_ID:Linux>:_GNU_SOURCE>)

# Architecture flags
if(CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "arm")
  target_compile_options(kklib-flags INTERFACE -march=native)
endif()

# Additional libraries
target_link_libraries(kklib-flags INTERFACE $<$<PLATFORM_ID:Linux,Darwin>:m>)
