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
    let hadamard_entropy (f : F2N.t -> f2) : float =
      let wf = F2N.walsh_hadamard_transform f in
      let two_pow_n = Float.of_int (1 lsl F2N.n) in
      let probs = F2N.all_boolvec () |>
                  List.map ~f:(fun y ->
                      let wfy = Z.to_int (wf y) in
                      let wfy = Float.of_int wfy in
                      let wfy = wfy /. two_pow_n in
                      wfy *. wfy) in
      List.fold probs ~init:0. ~f:(fun accum p ->
          if p = 0.
          then accum
          else accum -. (p *. log p)) /. log 2. in
    let process inp =
      let f = F2N.parse_func inp in
      match f with
      | None ->
        print_endline "Not a valid boolean function";
        print_string "Length = ";
        print_int l;
        print_newline ()
      | Some f ->
        hadamard_entropy f |> Float.to_string |> print_endline in
    List.iter ~f:process inps
