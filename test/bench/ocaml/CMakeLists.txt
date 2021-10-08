#see <https://github.com/ocaml-multicore/multicore-opam> for installation (including domainslib)
#> opam update
#> opam switch create 4.12.0+domains+effects --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default
#> opam install dune domainslib
#
#compile as:
#> ocamlopt -O2  -o ./mcml_bintrees -I ~/.opam/4.12.0+domains+effects/lib/domainslib/ domainslib.cmxa test/bench/ocaml/binarytrees_mc.ml


set(sources cfold.ml deriv.ml nqueens.ml rbtree.ml rbtree-ck.ml binarytrees.ml)

# find_program(ocamlopt "ocamlopt" REQUIRED)
set(ocamlopt "ocamlopt")

# no domains
set(domainslib "unix.cmxa")

# with domains
# set(domainslib "-I $ENV{HOME}/.opam/4.12.0+domains+effects/lib/domainslib/ domainslib.cmxa")
# set(sources cfold.ml deriv.ml nqueens.ml rbtree.ml rbtree-ck.ml binarytrees.ml)

foreach (source IN LISTS sources)
  get_filename_component(name "${source}" NAME_WE)
  set(name "ml-${name}")
  
  add_custom_command(
    OUTPUT  ${name}    
    COMMAND ${ocamlopt} -O2 -o ${name} ${domainslib} "$<SHELL_PATH:${CMAKE_CURRENT_SOURCE_DIR}/${source}>"
    DEPENDS ${source}
    VERBATIM)

  add_custom_target(update-${name} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name})

  add_executable(${name}-exe IMPORTED)
  set_target_properties(${name}-exe PROPERTIES IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/${name}")

  add_test(NAME ${name} COMMAND ${name}-exe)
  set_tests_properties(${name} PROPERTIES LABELS ocaml)
endforeach ()
