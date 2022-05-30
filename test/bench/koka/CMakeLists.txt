set(sources cfold.kk deriv.kk nqueens.kk nqueens-int.kk
            rbtree-poly.kk rbtree.kk rbtree-int.kk
            rbtree-ck.kk binarytrees.kk)

set(koka "/build/koka-v2.3.3/local/bin/koka")
set(kokaold "/build/koka-v2.3.3-old/local/bin/koka")

foreach (source IN LISTS sources)
  get_filename_component(basename "${source}" NAME_WE)
  set(name    "kk-${basename}")        # koka with reuse
  set(namex   "kkx-${basename}")       # koka without trmc
  set(nameold "kkold-${basename}")     # old koka reuse algorithm

  set(out_dir     "${CMAKE_CURRENT_BINARY_DIR}/out/bench")
  set(outx_dir    "${CMAKE_CURRENT_BINARY_DIR}/outx/bench")
  set(outold_dir  "${CMAKE_CURRENT_BINARY_DIR}/outold/bench")
  set(out_path    "${out_dir}/${name}")
  set(outx_path   "${outx_dir}/${namex}")
  set(outold_path "${outold_dir}/${nameold}")

  if("${source}" MATCHES "rbtree*")
    set(ccomp "clang")
  else()
    set(ccomp "gcc")
  endif()

  add_custom_command(
    OUTPUT  ${out_path}
    COMMAND ${koka} --target=c --cc=${ccomp} --stack=128M --outputdir=${out_dir} --buildname=${name} -v -O2 -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> "${source}"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS "${out_path}")
  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${out_path}")

  # --fno-trmc
  # --fno-reuse
  add_custom_command(
    OUTPUT  ${outx_path}
    COMMAND ${koka} --target=c --cc=${ccomp} --stack=128M --outputdir=${outx_dir} --buildname=${namex} -v -O2 --fno-opttrmc -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> "${source}"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${namex} ALL DEPENDS "${outx_path}")
  add_executable(${namex}-exe IMPORTED)
  set_target_properties(${namex}-exe PROPERTIES IMPORTED_LOCATION "${outx_path}")

  if (NOT (${kokaold} MATCHES "kokaold-NOTFOUND"))
    add_custom_command(
      OUTPUT  ${outold_path}
      COMMAND ${kokaold} --target=c --cc=${ccomp} --stack=128M --outputdir=${outold_dir} --buildname=${nameold} -v -O2 -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> "${source}"
      DEPENDS ${source}
      VERBATIM)

    add_custom_target(update-${nameold} ALL DEPENDS "${outold_path}")
    add_executable(${nameold}-exe IMPORTED)
    set_target_properties(${nameold}-exe PROPERTIES IMPORTED_LOCATION "${outold_path}")
  endif()

  add_test(NAME ${name} COMMAND ${name}-exe)
  set_tests_properties(${name} PROPERTIES LABELS koka)
endforeach ()
