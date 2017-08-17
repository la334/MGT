(* .mli file for the module Database of the MGT which holds the database
 * of grammars and words available to MGT.
 * This file contains the code which creates and stores a rule-based grammar for
 * each available grammar. *)

open Grammar
open FeatVal
open Feature
open Features
open DatabaseCreator
open Morphology

(* [english] is the grammar which represents the MGT's version of the English
 * language. *)
val english : grammar

(* [italian] is the grammar which represents the MGT's version of the Italian
 * language. *)
val italian : grammar

(* [german] is the grammar which represents the MGT's version of the German
 * language. *)
val german : grammar

(* [available_grammars] is the list of strings to be used for selection
 * printing purposes. *)
val available_grammars : string list

(* [shortened_forms_grammar] is the association list in which the keys are the
 * shortened forms of languages and the values are the grammars which correspond
 * to the shortened forms. *)
val shortened_forms_grammars : (string * grammar) list

(* [available_lang_grammars] is the association list in which the keys are the
 * names of languages and the values are the grammars which correspond to the
 * names. *)
val available_lang_grammars : (string * grammar) list

(* [original_forms_grammars] is the association list in which the keys are the
 * original forms of languages and the values are the grammars which correspond
 * to the original forms. *)
val original_forms_grammars : (string * grammar) list

(* [grammar_to_string lang] is the name of the grammar represented by [lang]. *)
val grammar_to_string : grammar -> string

(* [string_to_grammar name] is the grammar with the name [name]. *)
val string_to_grammar : string -> grammar

(* [shortened_form_to_grammar name] is the grammar with the shortened form
 * [name]. *)
val shortened_form_to_grammar : string -> grammar

(* [original_form_to_original_form name] is the grammar with the original form
 * [name]. *)
val original_form_to_grammar : string -> grammar

(* [shortened_form_to_string sf] is the name of the grammar with the shortened
 * form [sf]. *)
val shortened_form_to_string : string -> string

(* [update_database ()] writes the new database to the file [database.json]. *)
val update_database : unit -> unit