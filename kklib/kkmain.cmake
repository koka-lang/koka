# -----------------------------------------------------------------------------
# Koka main CMakeLists.txt file
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
# -Dkk_invokedir    : koka compiler invokation directory (= user current directory)
# -Dkk_installdir   : koka installation directory
# -Dkklib_installdir: normally ${kk_installdir}/kklib
# -----------------------------------------------------------------------------

if(NOT DEFINED kk_invokedir)
  set(kk_invokedir "${CMAKE_CURRENT_LIST_DIR}/..")
endif()
if(NOT DEFINED kk_installdir)
  set(kk_installdir "${kk_invoke_dir}")
endif()
if(NOT DEFINED kklib_installdir)
  set(kklib_installdir "${kk_installdir}/kklib")
endif()
message(STATUS "kklib_installdir=${kklib_installdir})")

# -----------------------------------------------------------------------------
# kklib support library
# -----------------------------------------------------------------------------

if (KK_USE_KKLIB_PACKAGE MATCHES ON)
  # use installed kklib as a package
  find_package(kklib 1.0 REQUIRED)
else()  
  # We copy the sources and compile as part of the modules (so all configuration is consistent).
  if (NOT EXISTS "${CMAKE_CURRENT_LIST_DIR}/kklib/CMakeLists.txt" OR (KK_REBUILD MATCHES ON))
    message(STATUS "copying kklib")
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy_directory "${kklib_installdir}/src" "${CMAKE_CURRENT_LIST_DIR}/kklib/src")
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy_directory "${kklib_installdir}/include" "${CMAKE_CURRENT_LIST_DIR}/kklib/include")
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy_directory "${kklib_installdir}/mimalloc" "${CMAKE_CURRENT_LIST_DIR}/kklib/mimalloc")
    file(COPY "${kklib_installdir}/CMakeLists.txt" DESTINATION "${CMAKE_CURRENT_LIST_DIR}/kklib")
  endif ()
  add_subdirectory(kklib)
endif()

# -----------------------------------------------------------------------------
# modules: include all `<module>.cmake` files
# -----------------------------------------------------------------------------

file(GLOB kkmain_modules CONFIGURE_DEPENDS "*.cmake")
foreach (kkmodule IN LISTS kkmain_modules)
  include("${kkmodule}")
endforeach ()

# -----------------------------------------------------------------------------
# extra option configuration
# -----------------------------------------------------------------------------
include("${kk_invokedir}/kk-options.cmake" OPTIONAL)
