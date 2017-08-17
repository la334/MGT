(* .mli file for the module Grammar of the MGT which defines the rule-based
 * grammar used by the MGT.
 * This file contains the code which defines the modules for feature values
* and features as well the types for rules and grammars. *)


(* A [FeatVal] is the specification of a feature value. It represents the
 * specifications possible for the person ([First], [Second], [Third]), for the
 * number ([Sing], [Pl]), for the gender ([Masc], [Fem]), for the case ([Nom],
 * [Acc]), for the subordinate value ([Yes], [No]), and a clash [ClashError] as
 * well as the empty specification [Empty].*)
module FeatVal : sig
  type t =
    First | Second | Third |
    Sing | Pl |
    Masc | Fem |
    Nom | Acc |
    Yes | No |
    Empty | ClashError

  (* [equals f1 f2] is [true] if [f1] equals [f2] and [false] otherwise. *) 
  val equals : t -> t -> bool
end

(* A [Feature] is the value that a feature takes on. It can either be
 * underspecified ([Any]), only one specific value [s] ([Only s]), a misfit in
 * unification ([Clash]), only one specific value [s] used for unification
 * [Unifier s], or any value [f] that prevents further unification [UnifyBlock
 * f]. *)

module Feature : sig
  type t = Any | Only of FeatVal.t | Clash | Unifier of FeatVal.t
    | UnifyBlock of t

  (* [replace_ftval v1 v2] is the [Feature] returned by attempting to
   * replace [v1] with [v2] during unification. *)
  val replace_ftval : t -> t -> t

  (* [equals_ftval v1 v2] is [true] if [v1] and [v2] have the same core value
   * and [false] otherwise. *)
  val equals_ftval : t -> t -> bool
end

(* A [Features] is the bundle of features which represents a node in a
 * rule-based grammar.  *)
module Features : sig
  type elt = Feature.t
  type t =
    {person : elt;
    number : elt;
    gender : elt;
    case : elt;
    subordinate : elt;
    lex : string;
    entry : string}

  (* [no_ft] represents an empty feature bundle. *)
  val no_ft : t

  (* [only_entry s] represents an empty feature bundle which also has an entry
   * value of [s]. *)
  val only_entry : string -> t

  (* [get_p f] is the record field [person] for [f]. *)
  val get_p : t -> elt

  (* [get_n f] is the record field [number] for [f]. *)
  val get_n : t -> elt
  
  (* [get_g f] is the record field [gender] for [f]. *)
  val get_g : t -> elt
  
  (* [get_c f] is the record field [case] for [f]. *)
  val get_c : t -> elt
  
  (* [get_s f] is the record field [subordinate] for [f]. *)
  val get_s : t -> elt
  
  (* [get_l f] is the record field [lex] for [f]. *)
  val get_l : t -> string
  
  (* [get_e f] is the record field [entry] for [f]. *)
  val get_e : t -> string
  
  (* [equals_ft v1 v2] is [true] if [v1] and [v2] have the same core value and
   * [false] otherwise. *)
  val equals_ft : t -> t -> bool

  (* [has_clash f] is [true] if the feature bundle [f] contains at least one 
   * feature which has the value [Clash] and [false] otherwise. *)
  val has_clash : t -> bool

  (* [is_subordinate f] is [true] if the feature bundle [f] specifies that [f]
   * is subordinate and [false] otherwise. *) 
  val is_subordinate : t -> bool

  (* [set_p f v] is the feature bundle produced by setting the record field
   * [person] of [f] to [v]. *)
  val set_p : t -> elt -> t

  (* [set_n f v] is the feature bundle produced by setting the record field
   * [number] of [f] to [v]. *)
  val set_n : t -> elt -> t
  
  (* [set_g f v] is the feature bundle produced by setting the record field
   * [gender] of [f] to [v]. *)
  val set_g : t -> elt -> t

  (* [set_c f v] is the feature bundle produced by setting the record field
   * [case] of [f] to [v]. *)
  val set_c : t -> elt -> t

  (* [set_s f v] is the feature bundle produced by setting the record field
   * [subordinate] of [f] to [v]. *)
  val set_s : t -> elt -> t

  (* [set_l f v] is the feature bundle produced by setting the record field
   * [lex] of [f] to [v]. *)
  val set_l : t -> string -> t

  (* [set_e f v] is the feature bundle produced by setting the record field
   * [entry] of [f] to [v]. *)
  val set_e : t -> string -> t

  (* [replace_p f v] is the feature bundle produced by trying to replace the
   * record field [person] of [f] to [v] during unification. *)
  val replace_p : t -> elt -> t

  (* [replace_n f v] is the feature bundle produced by trying to replace the
   * record field [number] of [f] to [v] during unification. *)
  val replace_n : t -> elt -> t

  (* [replace_g f v] is the feature bundle produced by trying to replace the
   * record field [gender] of [f] to [v] during unification. *)
  val replace_g : t -> elt -> t

  (* [replace_c f v] is the feature bundle produced by trying to replace the
   * record field [case] of [f] to [v] during unification. *)
  val replace_c : t -> elt -> t

  (* [replace_s f v] is the feature bundle produced by trying to replace the
   * record field [subordinate] of [f] to [v] during unification. *)
  val replace_s : t -> elt -> t
end

(* A [rule] is an entry in a grammar in a rule-based format. The rule S -> NP VP
 * where [S], [NP], and [VP] are all [Features] is represented as (S, [NP; VP])
 * in this type. *)
type rule = Features.t * Features.t list

(* A [grammar] is a list of rules. *)
type grammar = rule list

(* [rule_categories_entries r] is the rule [r] with the [Features] list replaced
 * with a list of [entries]. *)
val rule_categories_entries : rule -> Features.t * string list

(* [rule_categories_lexes r] is the rule [r] with the [Features] list replaced
 * with a list of [lexes] and the [Features] replace with an [entry]. *)
val rule_categories_lexes : rule -> string * string list