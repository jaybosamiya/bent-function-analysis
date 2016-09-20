open Core_kernel.Std

open Boolean

module F2N = F2N ( struct let n = 4 end )
type f2n = F2N.t

open F2.Infix
open F2N.Infix

let hadamard_entropy (f : f2n -> f2) : float =
  let wf = F2N.walsh_hadamard_transform f in
  let two_pow_n = Float.of_int (1 lsl F2N.n) in
  let probs = F2N.all_boolvec () |>
              List.map ~f:(fun y ->
                  let wfy = wf y in
                  let wfy = Float.of_int wfy in
                  let wfy = wfy /. two_pow_n in
                  wfy *. wfy) in
  List.fold probs ~init:0. ~f:(fun accum p ->
      if p = 0.
      then accum
      else accum -. (p *. log p)) /. log 2.

let () =
  F2N.all_func () |>
  List.iter ~f:(fun f ->
      F2N.print_func f |> print_string;
      print_string " ";
      hadamard_entropy f |> Float.to_string |> print_endline)
