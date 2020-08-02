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
target_include_directories(kklib PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include> $<INSTALL_INTERFACE:include>)
target_link_libraries(kklib PUBLIC kklib-flags)

install(TARGETS kklib kklib-flags EXPORT kklib INCLUDES DESTINATION include ARCHIVE DESTINATION lib)
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
set(kklib_cflags "")

if(CMAKE_C_COMPILER_ID MATCHES "AppleClang|Clang|GNU|Intel")
  list(APPEND kklib_cflags 
    -Wall
    -Wextra
    -Wno-unknown-pragmas
    -Wno-unused-parameter
    -Wno-unused-variable
    -Wno-unused-value
    -Wno-missing-field-initializers
    -Wpointer-arith
    -Wshadow
    -Wstrict-aliasing)
endif()

if(CMAKE_C_COMPILER_ID MATCHES "GNU")
  list(APPEND kklib_cflags  -Wno-unused-but-set-variable)
endif()

if(CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "arm")
  list(APPEND kklib_cflags -march=native)
endif()

target_compile_options(kklib-flags INTERFACE ${kklib_cflags})

# Platform-specific definitions
if(UNIX)
  target_compile_definitions(kklib-flags INTERFACE _GNU_SOURCE)
endif()

# Additional libraries
if(UNIX)
  target_link_libraries(kklib-flags INTERFACE m)
endif()  
