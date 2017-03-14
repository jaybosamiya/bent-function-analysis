open Core_kernel.Std

open Boolean

let () =
  let epsilon = try Float.of_string Sys.argv.(2) with
    | Invalid_argument _ -> 0.1 in
  let n = try Int.of_string (Sys.argv.(1)) with
    | Invalid_argument _ -> print_endline "Argument N required"; exit 1 in
  let module F2N = F2N ( struct let n = n end ) in
  let rec findfunc () =
    let f =
      let randf2 = fun () -> match Random.bool () with
        | true -> F2.One
        | false -> F2.Zero in
      let x = ref [] in
      for i = 0 to (1 lsl n) - 1 do
        x := randf2 () :: !x
      done; F2N.func_of_f2list_exn !x in
    if F2N.spectral_entropy f >. (Float.of_int n) -. epsilon
    then f
    else findfunc () in
  Random.self_init ();
  print_endline (F2N.print_func (findfunc ()))
