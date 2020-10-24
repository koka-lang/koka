# -----------------------------------------------------------------------------
# Koka top-level cmake file
#
# Options `-D<option>=<value>|ON`
#   kk_invokedir    : koka compiler invokation directory (= user current directory) (default: {CMAKE_CURRENT_LIST_DIR}/..)
#   kk_kklibdir     : required, but default to ${kk_libdir}/kklib)
#   kk_target       : optional: can specify a specific module target (instead of all available ones)
# ----------------------------------------------------------------------------- 
cmake_minimum_required(VERSION 3.8)
project(kkmain VERSION 1.0 LANGUAGES C)

option(KK_USE_KKLIB_PACKAGE "Use installed kklib package" OFF)
option(KK_REBUILD           "Rebuild all" OFF)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_C_EXTENSIONS NO)

set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)
set(CMAKE_DEBUG_POSTFIX "-debug")
set(CMAKE_RELWITHDEBINFO_POSTFIX "d")

# -----------------------------------------------------------------------------
# Directories
# -----------------------------------------------------------------------------

if(NOT DEFINED kk_invokedir)
  set(kk_invokedir "${CMAKE_CURRENT_LIST_DIR}/..")
endif()
if(NOT DEFINED kk_kklibdir)
  set(kk_kklibdir "${kk_invokedir}/kklib")
endif()
if(NOT DEFINED kk_libdir)
  set(kk_libdir "${kk_kklibdir}/..")
endif()

# message(STATUS "local build dir : ${CMAKE_CURRENT_LIST_DIR}")

# -----------------------------------------------------------------------------
# kklib support library
# -----------------------------------------------------------------------------

if (KK_USE_KKLIB_PACKAGE MATCHES ON) 
  # use installed kklib as a package
  find_package(kklib 1.0 REQUIRED)
else()  
  # We copy the sources and compile as part of the modules (so all configuration is consistent).
  if ((NOT EXISTS "${CMAKE_CURRENT_LIST_DIR}/kklib/CMakeLists.txt") OR
      (KK_REBUILD MATCHES ON) OR
      ("${kk_kklibdir}/CMakeLists.txt" IS_NEWER_THAN "${CMAKE_CURRENT_LIST_DIR}/kklib/CMakeLists.txt"))
    message(STATUS "install kklib from: ${kk_kklibdir}" )
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy_directory "${kk_kklibdir}/src" "${CMAKE_CURRENT_LIST_DIR}/kklib/src")
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy_directory "${kk_kklibdir}/include" "${CMAKE_CURRENT_LIST_DIR}/kklib/include")
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy_directory "${kk_kklibdir}/mimalloc" "${CMAKE_CURRENT_LIST_DIR}/kklib/mimalloc")
    file(COPY "${kk_kklibdir}/CMakeLists.txt" DESTINATION "${CMAKE_CURRENT_LIST_DIR}/kklib")
  endif ()
  add_subdirectory(kklib)
endif()

# -----------------------------------------------------------------------------
# modules: include all `<module>.cmake` files
# -----------------------------------------------------------------------------

if (DEFINED kk_target)
  include("${kk_target}.cmake")
else()  
  file(GLOB kkmain_cmake_modules "*.cmake")
  foreach (kk_cmake_module IN LISTS kkmain_cmake_modules)
    get_filename_component(kk_module "${kk_cmake_module}" NAME_WE)
    if (EXISTS "${CMAKE_CURRENT_LIST_DIR}/${CMAKE_BUILD_TYPE}/${kk_module}.c") # only include cmake's that have been koka compiled for this config
      message(STATUS "module : ${kk_module}")
      include("${kk_cmake_module}")
    endif()
  endforeach ()  
endif()

# -----------------------------------------------------------------------------
# extra option configuration
# -----------------------------------------------------------------------------
include("${kk_invokedir}/kk-options.cmake" OPTIONAL)
