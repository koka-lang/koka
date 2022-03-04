set(sources cfold.kk deriv.kk nqueens.kk nqueens-int.kk
            rbtree-poly.kk rbtree.kk rbtree-fbip.kk rbtree-fbip-poly.kk rbtree-int.kk
            rbtree-ck.kk binarytrees.kk)

find_program(kokadev "koka-dev")

# stack exec koka -- --target=c -O2 -c $(readlink -f ../cfold.kk) -o cfold
find_program(stack "stack" REQUIRED)
if(APPLE)
  set(koka ${stack} --system-ghc --skip-ghc-check exec koka --)
else()
  set(koka ${stack} exec koka --)
endif()


foreach (source IN LISTS sources)
  get_filename_component(basename "${source}" NAME_WE)
  set(name    "kk-${basename}")
  set(namex   "kkx-${basename}")
  set(namedev "kkdev-${basename}")

  set(out_dir     "${CMAKE_CURRENT_BINARY_DIR}/out/bench")
  set(outx_dir    "${CMAKE_CURRENT_BINARY_DIR}/outx/bench")
  set(outdev_dir  "${CMAKE_CURRENT_BINARY_DIR}/outdev/bench")
  set(out_path    "${out_dir}/${name}")
  set(outx_path   "${outx_dir}/${namex}")
  set(outdev_path "${outdev_dir}/${namedev}")

  if ("${source}" MATCHES "rbtree*")
    set(ccomp "clang")
  else()
    set(ccomp "gcc")
  endfi()

  add_custom_command(
    OUTPUT  ${out_path}
    COMMAND ${koka} --target=c --stack=128M --outputdir=${out_dir} --buildname=${name} -v -O2 -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> "${source}"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS "${out_path}")
  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${out_path}")

  # --fno-trmc
  # --fno-reuse
  add_custom_command(
    OUTPUT  ${outx_path}
    COMMAND ${koka} --target=c --stack=128M --outputdir=${outx_dir} --buildname=${namex} -v -O2 --fno-trmc -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> "${source}"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${namex} ALL DEPENDS "${outx_path}")
  add_executable(${namex}-exe IMPORTED)
  set_target_properties(${namex}-exe PROPERTIES IMPORTED_LOCATION "${outx_path}")

  if (NOT (${kokadev} MATCHES "kokadev-NOTFOUND"))
    add_custom_command(
      OUTPUT  ${outdev_path}
      COMMAND ${kokadev} --target=c --stack=128M --outputdir=${outdev_dir} --buildname=${namedev} -v -O2 -i$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}> "${source}"
      DEPENDS ${source}
      VERBATIM)

    add_custom_target(update-${namedev} ALL DEPENDS "${outdev_path}")
    add_executable(${namedev}-exe IMPORTED)
    set_target_properties(${namedev}-exe PROPERTIES IMPORTED_LOCATION "${outdev_path}")
  endif()

  add_test(NAME ${name} COMMAND ${name}-exe)
  set_tests_properties(${name} PROPERTIES LABELS koka)
endforeach ()
