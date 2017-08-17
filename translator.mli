(* .mli file for the module Translator which performs the translation portion
 * of the MGT.
 * This file contains the code necessary to produce a list of parsed trees
 * which represent the possible translations based on the provided parameters.*)

open Batteries
open Grammar
open Database
open Parser

(* A [translate_tree] is the tree resulting from translation from one language
 * into another. The major difference between a [translate_tree] and a
 * [tree] (from Parser) is that each leaf of a [translate_tree] contains a
 * list of feature bundles rather than just one feature bundle.
*)
type translate_tree =
	TLeaf of Features.t list | TBranch of Features.t * translate_tree list


(* [translate_to t lang] is the translate_tree representing the translation of 
 * tree [t] into the language represented by grammar [lang]. *)
val translate_to : tree -> grammar -> translate_tree

(* [german_movement t] is the translate_tree created when the rules of 
 * German verb-raising are applied to translate_tree [t]. *)
val german_movement : translate_tree -> translate_tree

(* [english_to_english s] is the result of translating string [s] from English
 * into English. Note that this function is not an identity function, but rather
 * a translation function. *)
val english_to_english : string -> string list

(* [italian_to_italian s] is the result of translating string [s] from Italian
 * into Italian. Note that this function is not an identity function, but rather
 * a translation function. *)
val italian_to_italian : string -> string list

(* [german_to_german s] is the result of translating string [s] from German
 * into German. Note that this function is not an identity function, but rather
 * a translation function. *)
val german_to_german : string -> string list

(* [english_to_italian s] is the result of translating string [s] from English
 * into Italian. *)
val english_to_italian : string -> string list

(* [italian_to_english s] is the result of translating string [s] from Italian
 * into English. *)
val italian_to_english : string -> string list

(* [english_to_german s] is the result of translating string [s] from English
 * into German. *)
val english_to_german : string -> string list

(* [italian_to_german s] is the result of translating string [s] from Italian
 * into German. *)
val italian_to_german : string -> string list

(* [german_to_english s] is the result of translating string [s] from German
 * into English. *)
val german_to_english : string -> string list

(* [german_to_italian s] is the result of translating string [s] from German
 * into Italian. *)
val german_to_italian : string -> string list