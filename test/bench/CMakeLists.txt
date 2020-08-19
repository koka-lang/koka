cmake_minimum_required(VERSION 3.16)
project(KokaBench)

get_property(isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if (NOT isMultiConfig AND NOT DEFINED CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif ()

enable_testing()

add_subdirectory(cpp)
add_subdirectory(haskell)
add_subdirectory(koka)
add_subdirectory(ocaml)

