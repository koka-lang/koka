cmake_minimum_required(VERSION 3.8)
project(kklib VERSION 1.0 LANGUAGES C)

enable_testing()


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


# -----------------------------------------------------------------------------
# Packaging
# -----------------------------------------------------------------------------

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

install(TARGETS kklib kklib-flags
        EXPORT kklib
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        INCLUDES DESTINATION include)

install(EXPORT kklib
        DESTINATION cmake
        FILE kklib-config.cmake)

install(DIRECTORY include/
        DESTINATION include
        FILES_MATCHING PATTERN "*.h")

write_basic_package_version_file(kklib-config-version.cmake COMPATIBILITY SameMajorVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/kklib-config-version.cmake
        DESTINATION cmake)


# -----------------------------------------------------------------------------
# Testing
# -----------------------------------------------------------------------------

set(test_sources
    test/main.c
    test/time.c
    )

add_executable(kklib-test ${test_sources})
target_link_libraries(kklib-test PRIVATE kklib)

add_test(NAME kklib-test COMMAND kklib-test)
set_tests_properties(kklib-test PROPERTIES PASS_REGULAR_EXPRESSION "Success!")


# -----------------------------------------------------------------------------
# Extended configuration
# -----------------------------------------------------------------------------

# CMake 3.8-compatible generator expressions
set(gnu_like $<OR:$<C_COMPILER_ID:AppleClang>,$<C_COMPILER_ID:Clang>,$<C_COMPILER_ID:GNU>,$<C_COMPILER_ID:Intel>>)
set(unix     $<OR:$<PLATFORM_ID:Linux>,$<PLATFORM_ID:Darwin>>)

target_compile_options(
  kklib-flags
  INTERFACE
    $<${gnu_like}:
      -Wall
      -Wextra
      -Wno-unknown-pragmas
      -Wno-unused-parameter
      -Wno-unused-variable
      -Wno-unused-value
      -Wno-missing-field-initializers
      -Wpointer-arith
      -Wshadow
      -Wstrict-aliasing>
    $<$<C_COMPILER_ID:GNU>:
      -Wno-unused-but-set-variable>
)

# Platform-specific definitions
target_compile_definitions(kklib-flags INTERFACE $<${unix}:_GNU_SOURCE>)

# Additional libraries
target_link_libraries(kklib-flags INTERFACE $<${unix}:m>)
