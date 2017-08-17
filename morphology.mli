(* .mli file for the module Morphology of the MGT which defines the
 * morphological rules for the grammars used by the MGT.
 * This file contains the code which assigns morphological rules to a
 * grammar. *)

open Grammar
open FeatVal
open Feature
open Features

(* [english_morphology e] is the grammar [e] with English morphology rules
 * applied. *)
val english_morphology : grammar -> grammar

(* [italian_morphology i] is the grammar [i] with Italian morphology rules
 * applied. *)
val italian_morphology : grammar -> grammar

(* [german_morphology g] is the grammar [g] with German morphology rules
 * applied. *)
val german_morphology : grammar -> grammar
