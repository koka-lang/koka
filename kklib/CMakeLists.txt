# -----------------------------------------------------------------------------
# Build the kklib library.
# Note: this is unused -- the koka compiler will use src/all.c to compile
# kklib into a single object file.
# -----------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.7)
project(kklib VERSION 1.0 LANGUAGES C)
enable_testing()

option(KK_MIMALLOC          "Use the mimalloc allocator" ON)
option(KK_MIMALLOC_INLINE   "Use the inlined branch of mimalloc allocator" OFF)
option(KK_DEBUG_SAN         "Compile with specified sanitizer (thread,memory,address,undefined) (clang only)" OFF)
option(KK_DEBUG_FULL        "Use full internal debug assertions" OFF)
option(KK_BUILD_TEST        "Build test target" OFF)

if(NOT DEFINED KK_COMP_VERSION)
  set(KK_COMP_VERSION "2.x.x")
endif()  

# -----------------------------------------------------------------------------
# Language options
# -----------------------------------------------------------------------------

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_C_EXTENSIONS NO)

set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

# set(CMAKE_DEBUG_POSTFIX "-debug")
# set(CMAKE_RELWITHDEBINFO_POSTFIX "d")
set(LIBUV_BUILD_SHARED OFF CACHE BOOL "Build libuv only as a static library" FORCE)
add_subdirectory(libuv)
include_directories(libuv/include)

# -----------------------------------------------------------------------------
# Main target
# -----------------------------------------------------------------------------
set(kklib_targets kklib kklib-flags)
set(kklib_sources
    src/bits.c
    src/box.c
    src/bytes.c
    src/init.c
    src/integer.c
    src/os.c
    src/process.c
    src/random.c
    src/refcount.c
    src/ref.c
    src/string.c
    src/thread.c
    src/time.c
    src/vector.c
    )

# properties library; these are used for all koka compiled C sources
add_library(kklib-flags INTERFACE)

# extra option configuration
include("${CMAKE_CURRENT_LIST_DIR}/../kklib-options.cmake" OPTIONAL)

if(NOT (KK_DEBUG_SAN MATCHES OFF))
  if(CMAKE_C_COMPILER_ID MATCHES "Clang")
    message(STATUS "Build with ${KK_DEBUG_SAN} sanitizer (and switch KK_MIMALLOC to OFF)")
    target_compile_options(kklib-flags INTERFACE -fsanitize=${KK_DEBUG_SAN} -g -O1)
    target_link_libraries(kklib-flags INTERFACE -fsanitize=${KK_DEBUG_SAN})
  else()
    message(WARNING "Can only use ${KK_DEBUG_SAN} sanitizer with clang (KK_DEBUG_SAN is ignored)")
  endif()
endif()

if(KK_DEBUG_FULL MATCHES ON)
  target_compile_definitions(kklib-flags INTERFACE KK_DEBUG_FULL=1)
endif()

if(KK_MIMALLOC MATCHES ON)
  list(APPEND kklib_sources mimalloc/src/static.c)
endif()


# -----------------------------------------------------------------------------
# Library
# -----------------------------------------------------------------------------
# support library
add_library(kklib STATIC ${kklib_sources})

target_compile_definitions(kklib PUBLIC KK_STATIC_LIB=1 KK_COMP_VERSION="${KK_COMP_VERSION}")
target_include_directories(kklib PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
target_link_libraries(kklib PUBLIC kklib-flags)
if(WIN32 AND CMAKE_C_COMPILER_ID MATCHES "Clang" AND "x${CMAKE_C_SIMULATE_ID}" STREQUAL "xMSVC")
  target_compile_definitions(kklib PRIVATE __clang_msvc__=1)
endif()

target_include_directories(kklib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/libuv/include)
target_link_libraries(kklib PRIVATE uv_a)

if(KK_MIMALLOC MATCHES ON)
target_include_directories(kklib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/mimalloc/include)
target_compile_definitions(kklib PRIVATE MI_MAX_ALIGN_SIZE=8)
if(WIN32)
  target_compile_definitions(kklib PRIVATE _CRT_SECURE_NO_WARNINGS=1)
endif()
target_compile_definitions(kklib-flags INTERFACE KK_MIMALLOC=1)
  if(KK_MIMALLOC_INLINE MATCHES ON)
    target_compile_definitions(kklib-flags INTERFACE KK_MIMALLOC_INLINE=1)
  endif()
  if(WIN32)
     target_link_libraries(kklib-flags INTERFACE psapi bcrypt)
  else()
     target_link_libraries(kklib-flags INTERFACE pthread)
  endif()
endif()


# -----------------------------------------------------------------------------
# Packaging
# -----------------------------------------------------------------------------

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

install(TARGETS ${kklib_targets}
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

if(KK_MIMALLOC MATCHES ON)
if(KK_MIMALLOC_INLINE MATCHES ON)
install(DIRECTORY mimalloc/include/
        DESTINATION include
        FILES_MATCHING PATTERN "mimalloc*.h")
else()
install(DIRECTORY mimalloc/include/
        DESTINATION include
        FILES_MATCHING PATTERN "mimalloc.h")
endif()
endif()


install(DIRECTORY libuv/include/
        DESTINATION include
        FILES_MATCHING PATTERN "uv/*.h")
install(DIRECTORY libuv/include/
        DESTINATION include
        FILES_MATCHING PATTERN "uv.h")


write_basic_package_version_file(kklib-config-version.cmake COMPATIBILITY SameMajorVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/kklib-config-version.cmake
        DESTINATION cmake)


# -----------------------------------------------------------------------------
# Testing
# -----------------------------------------------------------------------------
if(KK_BUILD_TEST MATCHES ON)
  set(test_sources
      test/main.c
      test/time.c
      )

  add_executable(kklib-test ${test_sources})
  target_compile_definitions(kklib-test PRIVATE KK_STATIC_LIB)
  target_link_libraries(kklib-test PRIVATE kklib)

  add_test(NAME kklib-test COMMAND kklib-test)
  set_tests_properties(kklib-test PROPERTIES PASS_REGULAR_EXPRESSION "Success!")
endif()

# -----------------------------------------------------------------------------
# Extended configuration
# -----------------------------------------------------------------------------

# CMake 3.8-compatible generator expressions
set(gnu_like $<OR:$<C_COMPILER_ID:AppleClang>,$<C_COMPILER_ID:Clang>,$<C_COMPILER_ID:GNU>,$<C_COMPILER_ID:Intel>>)
set(unix     $<OR:$<PLATFORM_ID:Linux>,$<PLATFORM_ID:Darwin>>)

target_compile_options(
  kklib-flags
  INTERFACE
    $<$<C_COMPILER_ID:Clang>:-Wno-everything> # for clang-cl, next line switches to Wall again
    $<${gnu_like}:
      -Wall -Wextra
      -Wno-unknown-pragmas
      -Wno-unused-parameter
      -Wno-unused-variable
      -Wno-unused-value
      -Wno-missing-field-initializers
      -Wpointer-arith
      -Wshadow
      -Wstrict-aliasing>
#    $<$<C_COMPILER_ID:GNU>:
#      -Wno-unused-but-set-variable>
#    $<$<C_COMPILER_ID:Clang>:
#      -Wno-cast-qual -Wno-undef -Wno-reserved-id-macro -Wno-unused-macros
#      -Wno-cast-align -Wno-format-nonliteral -Wno-float-equal
#      -Wno-missing-noreturn>
)

# Use O1 even in debug mode
#if (NOT KK_DEBUG_FULL)
target_compile_options(kklib-flags INTERFACE $<$<AND:$<CONFIG:Debug>,${gnu_like}>:-O1>)
#endif()

# Platform-specific definitions
target_compile_definitions(kklib-flags INTERFACE $<${unix}:_GNU_SOURCE>)

# Additional libraries: math
target_link_libraries(kklib-flags INTERFACE $<${unix}:m>)

include("${CMAKE_CURRENT_LIST_DIR}/../kklib.cmake" OPTIONAL)
