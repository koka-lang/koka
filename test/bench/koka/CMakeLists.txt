set(sources cfold.kk deriv.kk nqueens.kk nqueens32.kk 
            rbtree-poly.kk rbtree.kk rbtree32.kk 
            rbtree_ck.kk rbtreeg.kk)

# stack exec koka -- --target=c -O2 -c $(readlink -f ../cfold.kk) -o cfold
find_program(koka "stack" REQUIRED)
set(koka ${koka} exec koka --)

foreach (source IN LISTS sources)
  get_filename_component(basename "${source}" NAME_WE)
  set(name "kk-${basename}")

  set(src_path "${CMAKE_CURRENT_SOURCE_DIR}/${source}")

  # We really need a -o option.
  string(REPLACE "_" "__" koka_name "${basename}")
  string(REPLACE "-" "_dash_" koka_name "${koka_name}")
  string(MAKE_C_IDENTIFIER "${koka_name}" koka_name)

  add_custom_command(
    OUTPUT  ${name}
    COMMAND ${koka} --target=c -O2 "-i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}>" -c "$<SHELL_PATH:${src_path}>"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "$<SHELL_PATH:${CMAKE_CURRENT_BINARY_DIR}/out/${koka_name}/relwithdebinfo/${koka_name}>" ${name}
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name})

  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/${name}")

  add_test(NAME ${name} COMMAND ${name}-exe)
  set_tests_properties(${name} PROPERTIES LABELS koka)
endforeach ()
