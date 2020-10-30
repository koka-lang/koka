set(sources cfold.kk deriv.kk nqueens.kk nqueens-int.kk
            rbtree-poly.kk rbtree.kk rbtree-int.kk
            rbtree-ck.kk rbtreeg.kk)

# stack exec koka -- --target=c -O2 -c $(readlink -f ../cfold.kk) -o cfold
find_program(koka "stack" REQUIRED)
set(koka ${koka} exec koka --)

foreach (source IN LISTS sources)
  get_filename_component(basename "${source}" NAME_WE)
  set(name "kk-${basename}")
  set(namex "kkx-${basename}")

  set(out_path "${CMAKE_CURRENT_BINARY_DIR}/out/RelWithDebInfo/cbuild/${name}")
  set(outx_path "${CMAKE_CURRENT_BINARY_DIR}/outx/RelWithDebInfo/cbuild/${namex}")

  add_custom_command(
    OUTPUT  ${out_path}
    COMMAND ${koka} --target=c --outname=${name} -v -O2 -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> -c "${source}"
    DEPENDS ${source}
    VERBATIM)

  add_custom_command(
    OUTPUT  ${outx_path}
    COMMAND ${koka} --target=c --outdir=outx --outname=${namex} -v -O2 --fno-parcreuse --fno-parcspec -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> -c "${source}"
    DEPENDS ${source}
    VERBATIM)


  add_custom_target(update-${name} ALL DEPENDS "${out_path}")
  add_custom_target(update-${namex} ALL DEPENDS "${outx_path}")

  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${out_path}")

  add_executable(${namex}-exe IMPORTED)
  set_target_properties(${namex}-exe PROPERTIES IMPORTED_LOCATION "${outx_path}")

  add_test(NAME ${name} COMMAND ${name}-exe)
  set_tests_properties(${name} PROPERTIES LABELS koka)
endforeach ()
