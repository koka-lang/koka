(* Multi-core ocaml version by KC Sivaramakrishnan
   from: https://github.com/kayceesrk/code-snippets/blob/master/binary_trees/binarytrees_parallel2.ml 
      
   see <https://github.com/ocaml-multicore/multicore-opam> for installation (including domainslib)
   > opam update
   > opam switch create 4.12.0+domains+effects --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default
   > opam install dune domainslib

   compile as:
   > ocamlopt -O2  -o ./mcml_bintrees -I ~/.opam/4.12.0+domains+effects/lib/domainslib/ domainslib.cmxa test/bench/ocaml/binarytrees_mc.ml

   "Since this benchmark was designed to stress the GC performance, it is very sensitive to the size of the minor heap.
    The default minor heap size in OCaml is 2 MB (256k words on 64-bit machines). 
    You can try other options: use OCAMLRUNPARAM="s=2M" to set the minor heap size to 16 MB"
*)
module T = Domainslib.Task

let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let max_depth = try int_of_string Sys.argv.(2) with _ -> 10
let pool = T.setup_pool ~num_additional_domains:(num_domains - 1)

type 'a tree = Empty | Node of 'a tree * 'a tree

let rec make d =
(* if d = 0 then Empty *)
  if d = 0 then Node(Empty, Empty)
  else let d = d - 1 in Node(make d, make d)

let rec check t =
  Domain.Sync.poll ();
  match t with
  | Empty -> 0
  | Node(l, r) -> 1 + check l + check r

let min_depth = 4
let max_depth = max (min_depth + 2) max_depth
let stretch_depth = max_depth + 1

let () =
  (* Gc.set { (Gc.get()) with Gc.minor_heap_size = 1024 * 1024; max_overhead = -1; }; *)
  let c = check (make stretch_depth) in
  Printf.printf "stretch tree of depth %i\t check: %i\n" stretch_depth c

let long_lived_tree = make max_depth

let loop_depths d =
  let j = ((max_depth - d) / 2 + 1) in
  let a = Array.make j "" in
  T.parallel_for pool ~start:0 ~finish:(j-1)
    ~body:(fun i ->
      let d = d + i * 2 in
      let niter = 1 lsl (max_depth - d + min_depth) in
      let csum = ref 0 in
      for i = 0 to niter do
        csum := !csum + check(make d)
      done;
      let c = !csum
      (*
      let c = T.parallel_for_reduce pool (+) 0 ~start:1 ~finish:niter
        ~body:(fun _ -> check(make d))
      *)
      in
      a.(i) <- Printf.sprintf "%i\t trees of depth %i\t check: %i\n" niter d c);
  Array.iter print_string a

let () =
  flush stdout;
  loop_depths min_depth;
  Printf.printf "long lived tree of depth %i\t check: %i\n"
    max_depth (check long_lived_tree)
