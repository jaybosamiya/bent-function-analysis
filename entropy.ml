open Core_kernel.Std

open Boolean

module F2N = F2N ( struct let n = 4 end )
type f2n = F2N.t

open F2.Infix
open F2N.Infix

let () =
  F2N.all_func () |>
  List.iter ~f:(fun f ->
      F2N.print_func f |> print_string;
      print_string " ";
      F2N.spectral_entropy f |> Float.to_string |> print_endline)
