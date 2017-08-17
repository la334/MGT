(* .mli file for the module Parser of the MGT which parses a string.
 * This file contains the code necessary to chart-parse a string as well as
 * to parse a string using the Lexical Functional Grammar theory (i.e., using
 * features and unification. *)

open Batteries
open Grammar

(* A [splitpoint] describes the splitpoint k in the chart-parsing algorithm. *)
type splitpoint = Terminal | Chain | Position of int

(* A [SymbolSet] is a set of features. *)
module SymbolSet : Set.S with type elt = Features.t

(* A [CellEntry] represents an individual cell entry in a chart. Each entry
 * [(ft, r, sp)] contains a feature bundle [ft], a rule [r], and a splitpoint
 * [sp]. *)
module CellEntry : sig
  type t = Features.t * rule * splitpoint
  val cell_feature : t -> Features.t
  val cell_rule : t -> rule
  val cell_splitpoint : t -> splitpoint
end

(* A [TripleSet] is a set of cell entries. *)
module TripleSet : Set.S with type elt = CellEntry.t

(* A chart is the type used in chart-parsing, representing a string of length
 * [n] with a matrix of cell entries of dimensions [n + 1] x [n + 1]. *)
type chart = TripleSet.t array array

(* [StrangeChart] is an exception representing that something out of the
 * ordinary is listed as a cell entry in a chart. *)
exception StrangeChart

(* [chart_parse lang s] is the chart produced at the end of the chart-parsing
 * algorithm using the grammar [lang] on string [s]. *)
val chart_parse : grammar -> string -> chart

(* A [tree] represents a parse tree in which the nodes are features. *)
type tree = Leaf of Features.t | Branch of Features.t * tree list

val make_tree : chart -> string -> int * int -> tree list list

(* [tree_to_categories t] is the list of categories described by tree [t]
 * at its topmost node. *)
val tree_to_categories : tree -> string * string list

(* [feature_parse lang s] is the list of trees produced at the end of the LFG
 * unification algorithm using the grammar [lang] on string [s]. *)
val feature_parse : grammar -> string -> tree list

(* [to_string t] is the string represented by tree [t]. *)
val to_string : tree -> string