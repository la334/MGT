(* .ml file for the module Database of the MGT which holds the database
 * of grammars and words available to MGT.
 * This file contains the code which creates and stores a rule-based grammar for
 * each available grammar. *)

open Grammar
open FeatVal
open Feature
open Features
open DatabaseCreator
open Morphology

let database =
  let j = Yojson.Basic.from_file "database.json" in
  from_json_database j

let original_english = List.assoc "English" database
let original_italian = List.assoc "Italian" database
let original_german = List.assoc "German" database

let english = english_morphology (List.assoc "English" database)
let italian = italian_morphology (List.assoc "Italian" database)
let german = german_morphology (List.assoc "German" database)

let available_grammars = ["(E)nglish";"(G)erman";"(I)talian"]

let available_lang_grammars =
  [("English", english);
    ("Italian", italian);
    ("German", german)]

let shortened_forms_grammars =
  [("e", english);
    ("i", italian);
    ("g", german)]

let original_forms_grammars =
  [("e", original_english);
    ("i", original_italian);
    ("g", original_german)]


let grammar_to_string grammar =
  let available = List.map snd available_lang_grammars in
  if List.mem grammar available then (let res = List.find
    (fun x -> snd x = grammar)
    available_lang_grammars in
    fst res)
  else ""

let string_to_grammar s =
  let available = List.map fst available_lang_grammars in
  if List.mem s available
    then (let res = List.find (fun x -> fst x = s) available_lang_grammars in
    snd res)
  else []

let shortened_form_to_grammar s =
  let available = List.map fst shortened_forms_grammars in
  if List.mem s available
    then (let res = List.find (fun x -> fst x = s) shortened_forms_grammars in
    snd res)
  else []

let original_form_to_grammar s =
  let available = List.map fst original_forms_grammars in
  if List.mem s available
    then (let res = List.find (fun x -> fst x = s) original_forms_grammars in
    snd res)
  else []

let shortened_form_to_string s =
  let grammar = shortened_form_to_grammar s in
  grammar_to_string grammar

let update_database () =
  let j = to_json_database available_lang_grammars in
  Yojson.Basic.to_file ("database.json") j