(* .mli file for the module DatabaseCreator of the MGT which creates a database
 * from the file [database.json]
 * This file contains the code necessary to store to a .json file as well as to
 * read from one. *)


open Grammar
open FeatVal
open Feature
open Features
open Yojson

(* A [language] is an assciation between a language name and its grammar. *)
type language = string * grammar

(* [to_json_featval f] is the string version of [f] for use in a JSON file. *)
val to_json_featval : FeatVal.t -> string

(* [to_json_feature f] is the json version of [f] for use in a JSON file. *)
val to_json_feature : Feature.t -> Yojson.Basic.json

(* [to_json_features f] is the json version of [f] for use in a JSON file. *)
val to_json_features : Features.t -> Yojson.Basic.json

(* [to_json_rule r] is the json version of [r] for use in a JSON file. *)
val to_json_rule : rule -> Yojson.Basic.json

(* [to_json_grammar g] is the json version of [g] for use in a JSON file. *)
val to_json_grammar : grammar -> Yojson.Basic.json

(* [to_json_lang l] is the json version of [l] for use in a JSON file. *)
val to_json_lang : language -> Yojson.Basic.json

(* [to_json_database d] is the json version of [d] for use in a JSON file. *)
val to_json_database : language list -> Yojson.Basic.json

(* [from_json_featval j] is the feature value represented by string [j]. *)
val from_json_featval : string -> FeatVal.t

(* [from_json_feature j] is the feature represented by json [j]. *)
val from_json_feature : Yojson.Basic.json -> Feature.t

(* [from_json_features j] is the feature bundle represented by json [j]. *)
val from_json_features : Yojson.Basic.json -> Features.t

(* [from_json_rule j] is the rule represented by json [j]. *)
val from_json_rule : Yojson.Basic.json -> rule

(* [from_json_grammar j] is the grammar represented by json [j]. *)
val from_json_grammar : Yojson.Basic.json -> grammar

(* [from_json_lang j] is the language represented by json [j]. *)
val from_json_lang : Yojson.Basic.json -> language

(* [from_json_database j] is the database represented by json [j]. *)
val from_json_database : Yojson.Basic.json -> language list