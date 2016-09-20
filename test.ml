open Core_kernel.Std

open Boolean

let () =
  let open F2 in
  let module F2N = F2N ( struct let n = 4 end ) in
  F2N.all_func () |>
  List.iter ~f:(fun f ->
      F2N.print_func f |> print_string;
      print_string " ";
      F2N.is_bent f |> Bool.to_string |> print_endline)
