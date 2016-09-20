open Core_kernel.Std

open Boolean

let () =
  let open F2 in
  let module F2N = F2N ( struct let n = 3 end ) in
  F2N.all_func () |>
  List.iter ~f:(fun x -> F2N.print_func x |> print_endline)
