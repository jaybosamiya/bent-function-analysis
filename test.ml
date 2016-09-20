open Core_kernel.Std

open Boolean

let () =
  let module F2N = F2N ( struct let n = 3 end ) in
  let open F2.Infix in
  let v =
    match In_channel.(input_all stdin) |>
          String.strip |>
          F2N.parse_func with
    | None -> eprintf "Problemo\n"; assert false
    | Some x -> x (Option.value_exn (F2N.parse_boolvec "000")) in
  v <+> F2.One |>
  F2.print |>
  print_string
