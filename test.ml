open Core_kernel.Std

open Boolean

let () =
  let open F2 in
  let module F2N = F2N ( struct let n = 3 end ) in
  let open F2N.Infix in
  let a = F2N.of_f2list_exn [One; One; Zero] in
  let b = F2N.of_f2list_exn [Zero; One; One] in
  let c = a <..> b in
  let a, b, c = F2N.print_boolvec a, F2N.print_boolvec b,
                F2.print c in
  print_endline a;
  print_endline b;
  print_endline c
