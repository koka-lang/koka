find_program(ocamlopt "ocamlopt" REQUIRED)

set(sources cfold.ml deriv.ml nqueens.ml rbtree.ml rbtree_ck.ml)
foreach (source IN LISTS sources)
  get_filename_component(name "${source}" NAME_WE)
  set(name "ml-${name}")

  add_custom_command(
    OUTPUT  ${name}
    COMMAND ${ocamlopt} -O2 -o ${name} "$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}/${source}>"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name})

  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/${name}")

  add_test(NAME ${name} COMMAND ${name}-exe)
  set_tests_properties(${name} PROPERTIES LABELS ocaml)
endforeach ()
