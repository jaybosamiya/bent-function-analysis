open Core_kernel.Std

(** GF2 *)
module F2 : sig
  type t =
    | Zero
    | One

  val parse : string -> t option
  val print : t -> string

  module Infix : sig
    val (<&>) : t -> t -> t
    val (<+>) : t -> t -> t
  end
end

type f2 = F2.t

(** F_2^n for general n *)
module type t_f2n = sig
  type t

  (** A boolean vector goes x_1, ..., x_n *)
  val parse_boolvec : string -> t option

  (** A boolean function stores f(0), f(1), ... f(2^n-1) as a boolean
      vector *)
  val parse_func : string -> (t -> f2) option

  val print_boolvec : t -> string
  val print_func : (t -> f2) -> string

  val of_f2list_exn : f2 list -> t
  val of_f2list : f2 list -> t option
  val to_f2list : t -> f2 list

  module Infix : sig
    (** Element-wise And *)
    val (<&&>) : t -> t -> t

    (** Elementwise XOR *)
    val (<++>) : t -> t -> t

    (** Dot product of 2 vectors *)
    val (<..>) : t -> t -> F2.t
  end
end

(** Generates a [f2n] module that can then be used normally.
    Ensures that [n] remains constant within all values of
    the [f2n] module generated *)
module F2N ( N : sig val n : int end ) : t_f2n
