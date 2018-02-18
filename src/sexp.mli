open Import

include module type of struct include Usexp end with module Loc := Usexp.Loc

module Atom_set : Set.S with type elt = Atom.t
module Atom_map : Map.S with type key = Atom.t

val code_error : string -> (Atom.t * t) list -> _

val load : fname:string -> mode:'a Parser.Mode.t -> 'a
val load_many_as_one : fname:string -> Ast.t

type sexps_or_ocaml_script =
  | Sexps of Ast.t list
  | Ocaml_script

val load_many_or_ocaml_script : string -> sexps_or_ocaml_script

module type Combinators = sig
  type 'a t
  val unit       : unit                      t
  val atom       : Atom.t                    t
  val quoted_string : string                 t

  val string     : string                    t
  (** Conversion between strings and Atom or quoted strings. *)

  val int        : int                       t
  val float      : float                     t
  val bool       : bool                      t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val triple     : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val list       : 'a t -> 'a list           t
  val array      : 'a t -> 'a array          t
  val option     : 'a t -> 'a option         t

  val atom_set   : String_set.t            t
  (** [atom_set] is a conversion to/from a set of strings representing atoms. *)

  val atom_map   : 'a t -> 'a String_map.t   t
  (** [atom_map conv]: given a conversion [conv] to/from ['a], returns
     a conversion to/from a map where the keys are atoms and the
     values are of type ['a]. *)

  val atom_hashtbl : 'a t -> (string, 'a) Hashtbl.t t
  (** [atom_hashtbl conv] is similar to [atom_map] for hash tables. *)
end

module To_sexp : sig
  type sexp = t
  include Combinators with type 'a t = 'a -> t

  val record : (Atom.t * sexp) list -> sexp
end with type sexp := t

module Of_sexp : sig
  type ast = Ast.t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | List of Loc.t * ast list

  include Combinators with type 'a t = Ast.t -> 'a

  val of_sexp_error  : Ast.t -> string -> _
  val of_sexp_errorf : Ast.t -> ('a, unit, string, 'b) format4 -> 'a

  val located : 'a t -> (Loc.t * 'a) t

  (* Record parsing monad *)
  type 'a record_parser
  val return : 'a -> 'a record_parser
  val ( >>= ) : 'a record_parser -> ('a -> 'b record_parser) -> 'b record_parser

  (** Return the location of the record being parsed *)
  val record_loc : Loc.t record_parser

  val field   : Atom.t -> ?default:'a -> 'a t -> 'a record_parser
  val field_o : Atom.t -> 'a t -> 'a option record_parser
  val field_b : Atom.t -> bool record_parser

  val map_validate : 'a record_parser -> f:('a -> ('b, string) result) -> 'b record_parser

  val ignore_fields : Atom.t list -> unit record_parser

  val record : 'a record_parser -> 'a t

  module Constructor_spec : sig
    type 'a t
  end

  module Constructor_args_spec : sig
    type ('a, 'b) t
    (** Specify the type of arguments, ['a] being the type of
       arguments (e.g. [t1 -> t2 -> 'a] for two arguments, the first
       one of type [t1] and the second one of type [t2]) and ['b] the
       final return type. *)
  end

  val nil : ('a, 'a) Constructor_args_spec.t
  (** Terminal argument constructor. *)

  val ( @> )
    :  'a t
    -> ('b, 'c) Constructor_args_spec.t
    -> ('a -> 'b, 'c) Constructor_args_spec.t
  (** [c @> args] construct a new argument sequence that execute the
     combinator [c] before [args]. *)

  val cstr : Atom.t -> ('a, 'b) Constructor_args_spec.t ->
             'a -> 'b Constructor_spec.t

  val cstr_rest
    :  Atom.t
    -> ('a, 'b list -> 'c) Constructor_args_spec.t
    -> 'b t
    -> 'a
    -> 'c Constructor_spec.t

  val cstr_record : Atom.t -> 'a record_parser -> 'a Constructor_spec.t

  val cstr_loc
    :  Atom.t
    -> ('a, 'b) Constructor_args_spec.t
    -> (Loc.t -> 'a)
    -> 'b Constructor_spec.t

  val cstr_rest_loc
    :  Atom.t
    -> ('a, 'b list -> 'c) Constructor_args_spec.t
    -> 'b t
    -> (Loc.t -> 'a)
    -> 'c Constructor_spec.t

  val sum
    :  'a Constructor_spec.t list
    -> 'a t

  val enum : (Atom.t * 'a) list -> 'a t
end
