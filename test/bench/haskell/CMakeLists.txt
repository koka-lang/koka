find_program(GHC "stack")
if (GHC)
  list(APPEND GHC ghc --)
else ()
  find_program(GHC ghc REQUIRED)
endif ()

set(sources cfold.hs deriv.hs nqueens.hs rbtree.hs rbtree2.hs rbtree_ck.hs)
foreach (source IN LISTS sources)
  get_filename_component(name "${source}" NAME_WE)
  set(name "hs-${name}")

  add_custom_command(
    OUTPUT  ${name}
    COMMAND ${GHC} -O2 -o ${name} "$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}/${source}>"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name})

  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/${name}")

  add_test(NAME ${name} COMMAND ${name}-exe)
  set_tests_properties(${name} PROPERTIES LABELS haskell)
endforeach ()



