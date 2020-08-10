cmake_minimum_required(VERSION 3.8)
project(kklib VERSION 1.0 LANGUAGES C)

enable_testing()

option(KK_MIMALLOC          "Use the mimalloc allocator" ON)
option(KK_MIMALLOC_INLINE   "Use the inlined branch of mimalloc allocator" OFF)

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
set(kklib_targets kklib kklib-flags)
set(kklib_sources
    src/bits.c
    src/box.c
    src/integer.c
    src/random.c
    src/refcount.c
    src/init.c
    src/string.c
    )
    
if(KK_MIMALLOC MATCHES ON)
  list(APPEND kklib_sources mimalloc/src/static.c)      
endif()

# properties library
add_library(kklib-flags INTERFACE)

# library
add_library(kklib STATIC ${kklib_sources})
target_compile_definitions(kklib PRIVATE KK_STATIC_LIB)
target_include_directories(kklib PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
target_link_libraries(kklib PUBLIC kklib-flags)


if(KK_MIMALLOC MATCHES ON)
target_include_directories(kklib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/mimalloc/include)
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
target_compile_definitions(kklib-test PRIVATE KK_STATIC_LIB)
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

include("${CMAKE_CURRENT_LIST_DIR}/../kklib.cmake" OPTIONAL)
