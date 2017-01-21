open Core_kernel.Std

module F2 = struct
  type t =
    | Zero
    | One

  let parse str : t option =
    match str with
    | "0" -> Some Zero
    | "1" -> Some One
    | _ -> None

  let print v : string =
    match v with
    | Zero -> "0"
    | One -> "1"

  module Infix = struct
    let (<&>) a b =
      match a, b with
      | (One, One) -> One
      | _ -> Zero

    let (<+>) a b =
      match a, b with
      | (One, Zero) | (Zero, One) -> One
      | _ -> Zero
  end

  let to_int x =
    match x with
    | Zero -> 0
    | One -> 1

  let of_int x =
    match x with
    | 0 -> Some Zero
    | 1 -> Some One
    | _ -> None

  let of_int_exn x =
    match of_int x with
    | Some x -> x
    | None -> raise (Failure "Not boolean")
end

type f2 = F2.t

module type t_f2n = sig
  type t
  val n : int

  val parse_boolvec : string -> t option
  val parse_func : string -> (t -> f2) option

  val print_boolvec : t -> string
  val print_func : (t -> f2) -> string

  val of_f2list_exn : f2 list -> t
  val of_f2list : f2 list -> t option
  val to_f2list : t -> f2 list

  val func_of_f2list_exn : f2 list -> (t -> f2)
  val func_of_f2list : f2 list -> (t -> f2) option
  val func_to_f2list : (t -> f2) -> f2 list

  module Infix : sig
    val (<&&>) : t -> t -> t
    val (<++>) : t -> t -> t
    val (<..>) : t -> t -> F2.t
  end

  val all_boolvec : unit -> t list
  val all_func : unit -> (t -> f2) list

  val walsh_hadamard_transform : (t -> f2) -> (t -> Z.t)
  val is_bent : (t -> f2) -> bool
end

module F2N ( N : sig val n : int end ) : t_f2n = struct
  let n = N.n
  type t = Z.t

  exception Not_boolean

  let rec boolstr_to_Zt chrs : Z.t =
    match chrs with
    | [] -> Z.zero
    | '0' :: xs -> Z.mul (Z.of_int 2) (boolstr_to_Zt xs)
    | '1' :: xs -> Z.add (Z.mul (Z.of_int 2) (boolstr_to_Zt xs)) Z.one
    | _ :: _ -> raise Not_boolean

  let parse_boolvec str : t option =
    if String.length str = n
    then try Some (boolstr_to_Zt (String.to_list_rev str))
      with Not_boolean -> None
    else None

  let parse_func str : (t -> f2) option =
    let fans =
      if String.length str = 1 lsl n (* str has length 2^n *)
      then try Some (List.map (String.to_list str) ~f:(function
          | '0' -> F2.Zero
          | '1' -> F2.One
          | _ -> raise Not_boolean))
        with Not_boolean -> None
      else None in
    match fans with
    | Some fans ->
      Some (fun n -> List.nth_exn fans (Z.to_int n))
    | None ->
      None

  let print_boolvec (v:t) : string =
    let rec f n b =
      if n = 0
      then []
      else (Z.rem b (Z.of_int 2)) :: f (n-1) (Z.div b (Z.of_int 2)) in
    List.rev (f n v) |>
    List.map ~f:Z.to_string |>
    String.concat

  let print_func (f:t -> f2) : string =
    List.range 0 (1 lsl n) |>
    List.map ~f:Z.of_int |>
    List.map ~f |>
    List.map ~f:F2.print |>
    String.concat

  let of_f2list xs =
    if List.length xs = n
    then let rec f xs : t =
           match xs with
           | [] -> Z.zero
           | F2.Zero :: xs -> Z.mul (Z.of_int 2) (f xs)
           | F2.One :: xs -> Z.add (Z.mul (Z.of_int 2) (f xs)) Z.one in
      Some (f (List.rev xs))
    else None

  let of_f2list_exn xs =
    match of_f2list xs with
    | Some x -> x
    | None -> raise (Failure "Wrong length")

  let to_f2list v =
    let rec f n b =
      if n = 0
      then []
      else Z.(b mod (of_int 2)) :: f (n-1) Z.(b / (of_int 2)) in
    List.rev (f n v) |>
    List.map ~f:Z.to_string |>
    List.map ~f:F2.parse |>
    List.map ~f:(fun x -> Option.value_exn x)

  module Infix = struct
    let (<&&>) a b : t =
      Z.(a land b)

    let (<++>) a b : t =
      Z.(a lxor b)

    let (<..>) a b : F2.t =
      let x = a <&&> b in
      let p = Z.popcount x in
      if p mod 2 = 0
      then F2.Zero
      else F2.One
  end

  let all_boolvec () : t list =
    let rec f i : t list =
      if i = Z.(one lsl n)
      then []
      else i :: f Z.(i + one)
    in
    f Z.zero

  let func_of_f2list xs : (t -> f2) option =
    if List.length xs = 1 lsl n
    then Some (fun n -> List.nth_exn xs (Z.to_int n))
    else None

  let func_of_f2list_exn xs : (t -> f2) =
    match func_of_f2list xs with
    | Some x -> x
    | None -> raise (Failure "Wrong length")

  let func_to_f2list (f : t -> f2) : f2 list =
    all_boolvec () |>
    List.map ~f

  let all_func () : (t -> f2) list =
    let to_f2list v =
      let rec f n b =
        if n = 0
        then []
        else b mod 2 :: f (n-1) (b/2) in
      List.rev (f (1 lsl n) v) |>
      List.map ~f:Int.to_string |>
      List.map ~f:F2.parse |>
      List.map ~f:(fun x -> Option.value_exn x) in
    List.range 0 (1 lsl (1 lsl n)) |>
    List.map ~f:to_f2list |>
    List.map ~f:func_of_f2list_exn

  let walsh_hadamard_transform (f : t -> f2) : (t -> Z.t) =
    let open F2.Infix in
    let open Infix in
    let f_list = all_boolvec () |>
                 List.map ~f |>
                 List.map ~f:(fun x -> match x with
                     | F2.Zero -> Z.one
                     | F2.One -> Z.(-one)) in
    let rec f l =
      let len = List.length l in
      if len = 1 then l else
        let a = f (List.sub l ~pos:0 ~len:(len / 2)) in
        let b = f (List.sub l ~pos:(len / 2) ~len:(len / 2)) in
        let c = List.map2_exn a b ~f:(Z.add) in
        let d = List.map2_exn a b ~f:(Z.sub) in
        c @ d in
    let wf_list = f f_list in
    (fun y -> List.nth_exn wf_list (Z.to_int y))

  let is_bent f : bool =
    if n mod 2 = 1
    then raise (Failure "Can check bent only for even n")
    else
      let wf = walsh_hadamard_transform f in
      let z = 1 lsl (n/2) in
      all_boolvec () |>
      List.for_all ~f:(fun y ->
          Z.abs (wf y) = Z.of_int z)

end
