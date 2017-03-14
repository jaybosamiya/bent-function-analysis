open Core_kernel.Std

open Boolean

let () =
  let inps = In_channel.(input_lines stdin) in
  match inps with
  | [] ->
    print_endline "Require boolean function"
  | x :: _ ->
    let l = String.length x in
    let n = Z.(log2up ~$l) in
    let module F2N = F2N ( struct let n = n end ) in
    let process inp =
      let f = F2N.parse_func inp in
      match f with
      | None ->
        print_endline "Not a valid boolean function";
        print_string "Length = ";
        print_int l;
        print_newline ()
      | Some f ->
        F2N.spectral_entropy f |> Float.to_string |> print_endline in
    List.iter ~f:process inps
