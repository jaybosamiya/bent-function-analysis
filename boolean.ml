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
end

type f2 = F2.t

module type t_f2n = sig
  type t

  val parse_boolvec : string -> t option
  val parse_func : string -> (t -> f2) option

  val print_boolvec : t -> string
  val print_func : (t -> f2) -> string

  val of_f2list_exn : f2 list -> t
  val of_f2list : f2 list -> t option
  val to_f2list : t -> f2 list

  module Infix : sig
    val (<&&>) : t -> t -> t
    val (<++>) : t -> t -> t
    val (<..>) : t -> t -> F2.t
  end
end

module F2N ( N : sig val n : int end ) : t_f2n = struct
  let n = N.n
  type t = int

  exception Not_boolean

  let rec boolstr_to_int chrs : int =
    match chrs with
    | [] -> 0
    | '0' :: xs -> 2 * (boolstr_to_int xs)
    | '1' :: xs -> 2 * (boolstr_to_int xs) + 1
    | _ :: _ -> raise Not_boolean

  let parse_boolvec str : t option =
    if String.length str = n
    then try Some (boolstr_to_int (String.to_list_rev str))
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
      Some (fun n -> List.nth_exn fans n)
    | None ->
      None

  let print_boolvec (v:t) : string =
    let rec f n b =
      if n = 0
      then []
      else b mod 2 :: f (n-1) (b/2) in
    List.rev (f n v) |>
    List.map ~f:Int.to_string |>
    String.concat

  let print_func (f:t -> f2) : string =
    List.range 0 (1 lsl n) |>
    List.map ~f |>
    List.map ~f:F2.print |>
    String.concat

  let of_f2list xs =
    if List.length xs = n
    then let rec f xs : int =
           match xs with
           | [] -> 0
           | F2.Zero :: xs -> 2 * (f xs)
           | F2.One :: xs -> 2 * (f xs) + 1 in
      Some (f xs)
    else None

  let of_f2list_exn xs =
    match of_f2list xs with
    | Some x -> x
    | None -> raise (Failure "Wrong length")

  let to_f2list v =
    let rec f n b =
      if n = 0
      then []
      else b mod 2 :: f (n-1) (b/2) in
    List.rev (f n v) |>
    List.map ~f:Int.to_string |>
    List.map ~f:F2.parse |>
    List.map ~f:(fun x -> Option.value_exn x)

  module Infix = struct
    let (<&&>) a b : t =
      a land b

    let (<++>) a b : t =
      a lxor b

    let (<..>) a b : F2.t =
      let rec f a =
        match a with
        | 0 -> F2.Zero
        | 1 -> F2.One
        | x -> f ((x mod 2) lxor (x / 2)) in
      f (a <++> b)
  end
end
