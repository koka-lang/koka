find_program(javac "javac" REQUIRED)

set(sources rbtree.java rbtreeck.java nqueens.java deriv.java cfold.java binarytrees.java)
foreach (source IN LISTS sources)
  get_filename_component(name "${source}" NAME_WE)

  add_custom_command(
    OUTPUT  ${name}.class
    COMMAND ${javac} --enable-preview -source 17 -d "${CMAKE_CURRENT_BINARY_DIR}" "$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}/${source}>"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name}.class)

  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/${name}.class")

  add_test(NAME ${name} COMMAND "java" "--enable-preview" ${name} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
  set_tests_properties(${name} PROPERTIES LABELS java)
endforeach ()
