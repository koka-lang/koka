set(sources cfold.kk deriv.kk nqueens.kk nqueens-int.kk
            rbtree-poly.kk rbtree.kk rbtree-int.kk
            rbtree-ck.kk)

# stack exec koka -- --target=c -O2 -c $(readlink -f ../cfold.kk) -o cfold
find_program(koka "stack" REQUIRED)
set(koka ${koka} exec koka --)

foreach (source IN LISTS sources)
  get_filename_component(basename "${source}" NAME_WE)
  set(name "kk-${basename}")
  set(namex "kkx-${basename}")

  set(out_dir   "${CMAKE_CURRENT_BINARY_DIR}/out/bench")
  set(outx_dir  "${CMAKE_CURRENT_BINARY_DIR}/outx/bench")
  set(out_path  "${out_dir}/${name}")
  set(outx_path "${outx_dir}/${namex}")

  add_custom_command(
    OUTPUT  ${out_path}
    COMMAND ${koka} --target=c --builddir=${out_dir} --outname=${name} -v -O2 -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> -c "${source}"
    DEPENDS ${source}
    VERBATIM)

  # --fno-optctail
  add_custom_command(
    OUTPUT  ${outx_path}
    COMMAND ${koka} --target=c --builddir=${outx_dir} --outname=${namex} -v -O2 --fno-parcreuse --fno-parcspec -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> -c "${source}"
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
