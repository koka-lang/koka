set(sources cfold.kk deriv.kk nqueens.kk nqueens32.kk 
            rbtree-poly.kk rbtree.kk rbtree32.kk 
            rbtree_ck.kk rbtreeg.kk)

# stack exec koka -- --target=c -O2 -c $(readlink -f ../cfold.kk) -o cfold
find_program(koka "stack" REQUIRED)
set(koka ${koka} exec koka --)

foreach (source IN LISTS sources)
  get_filename_component(name "${source}" NAME_WE)
  set(name "kk-${name}")

  set(out_path "${CMAKE_CURRENT_BINARY_DIR}/out/RelWithDebInfo/cbuild/${name}")

  add_custom_command(
    OUTPUT  ${out_path}
    COMMAND ${koka} --target=c --outname=${name} -O2 -c "$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}/${source}>"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS "${out_path}")

  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${out_path}")

  add_test(NAME ${name} COMMAND ${name}-exe)
  set_tests_properties(${name} PROPERTIES LABELS koka)
endforeach ()
