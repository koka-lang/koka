set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

foreach (source IN ITEMS rbtree.cpp rbtree-ck.cpp nqueens.cpp deriv.cpp cfold.cpp) # binarytrees.cpp)
  get_filename_component(name "${source}" NAME_WE)
  set(name "cpp-${name}")

  add_executable(${name} ${source})
  if(source MATCHES "binarytrees.cpp") 
    target_link_libraries(${name} pthread)
    if(APPLE)
      target_include_directories(${name} PRIVATE "/opt/homebrew/Cellar/boost/1.76.0/include/")
    endif()
  endif()

  add_test(NAME ${name} COMMAND ${name})
  set_tests_properties(${name} PROPERTIES LABELS cpp)
endforeach ()

# target_compile_options(cpp-rbtreec PUBLIC -fpermissive)
