find_program(swiftc "swiftc" REQUIRED
             HINTS /opt/swift/bin
                   $ENV{SWIFT_ROOT}/bin
                   /usr/local/swift/bin)

set(sources binarytrees.swift cfold.swift deriv.swift qsort.swift rbtree.swift rbtree-ck.swift nqueens.swift)
foreach (source IN LISTS sources)
  get_filename_component(name "${source}" NAME_WE)
  set(name "sw-${name}")

  add_custom_command(
    OUTPUT  ${name}
    COMMAND ${swiftc} -O -o ${name} "$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}/${source}>"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name})

  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/${name}")

  add_test(NAME ${name} COMMAND ${name}-exe)
  set_tests_properties(${name} PROPERTIES LABELS swift)
endforeach ()
