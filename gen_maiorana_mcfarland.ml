open Core_kernel.Std

open Boolean

let () =
  Random.self_init ()

let shuffle d =
  let nd = List.map ~f:(fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map ~f:snd sond

let n = try Int.of_string (Sys.argv.(1)) with
  | Invalid_argument _ -> print_endline "Argument N required"; exit 1

module F2Nby2 = F2N ( struct let n = n / 2 end )
module F2N = F2N ( struct let n = n end )

let h =
  let randf2 = fun () -> match Random.bool () with
    | true -> F2.One
    | false -> F2.Zero in
  let x = ref [] in
  for i = 0 to (1 lsl (n / 2)) - 1 do
    x := randf2 () :: !x
  done; F2Nby2.func_of_f2list_exn !x

let pi =
  let pi = F2Nby2.all_boolvec ()
           |> shuffle
           |> List.map ~f:F2Nby2.to_f2list
           |> List.transpose_exn
           |> List.map ~f:F2Nby2.func_of_f2list_exn in
  fun y ->
    List.map pi ~f:(fun f -> f y) |> F2Nby2.of_f2list_exn


let f z =
  let z = F2N.to_f2list z in
  let x = List.take z (n / 2) |> F2Nby2.of_f2list_exn in
  let y = List.drop z (n / 2) |> F2Nby2.of_f2list_exn in
  let open F2Nby2.Infix in
  let open F2.Infix in
  (x <..> (pi y)) <+> (h y)

let () =
  print_endline (F2N.print_func f)
