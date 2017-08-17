(* .ml file for the module DatabaseCreator of the MGT which creates a database
 * from the file [database.json]
 * This file contains the code necessary to store to a .json file as well as to
 * read from one. *)
open Grammar
open FeatVal
open Feature
open Features
open Yojson

type language = string * grammar

let to_json_featval fv =
	match fv with
	| First -> "1"
	| Second -> "2"
	| Third -> "3"
	| Sing ->  "Sg"
	| Pl -> "Pl"
	| Masc -> "M"
	| Fem -> "F"
	| Nom -> "Nom"
	| Acc -> "Acc"
	| Yes -> "Yes"
	| No -> "No"
	| ClashError -> "ClashError" 
	| Empty -> "Empty"

let from_json_featval j =
	match j with
	| "1" -> First
	| "2" -> Second
	| "3" -> Third
	| "Sg" -> Sing
	| "Pl" -> Pl
	| "M" -> Masc
	| "F" -> Fem
	| "Nom" -> Nom
	| "Acc" -> Acc
	| "Yes" -> Yes  
	| "No" -> No    
	| "ClashError" -> ClashError
	| _ -> Empty      

let rec to_json_feature fv =
  match fv with
  	| Any -> `String "Any"
  	| Only(s) -> `String ("Only " ^ (to_json_featval s))
  	| Clash -> `String "Clash"
  	| Unifier(s) -> `String ("Unifier " ^ (to_json_featval s))
  	| UnifyBlock(ifv) ->
  			let ifv = Yojson.Basic.Util.to_string (to_json_feature ifv) in
		  	`String ("UnifyBlock " ^ ifv)

let rec from_json_feature j =
  let js = Yojson.Basic.Util.to_string j in
  match js with
	| "Any" -> Any
	| "Clash" -> Clash
	| x ->
		let lst = Str.split (Str.regexp " +") js in
		let kind = List.hd lst in
		let lst = List.tl lst in
		  match kind with
		    | "Only" ->
		    		let s = List.hd lst in
		    		let s = from_json_featval s in
		    		Only(s)
		    | "Unifier" ->
		    		let s = List.hd lst in
		    		let s = from_json_featval s in
		    		Unifier(s)
		    | "UnifyBlock" ->
		    		let newjs = List.fold_left (fun acc x -> acc ^ " " ^ x) "" lst in
		    		let newjs = String.trim newjs in
		    		let newjs = `String newjs in
		    		let ifv = from_json_feature newjs in
		    		UnifyBlock(ifv)
		    | _ -> failwith "Invalid Yojson input" 

let to_json_features f =
	let p = get_p f in let n = get_n f in let g = get_g f in let c = get_c f in
	let s = get_s f in let l = get_l f in let e = get_e f in
	`Assoc [("person", to_json_feature p); ("number", to_json_feature n);
		("gender", to_json_feature g); ("case", to_json_feature c);
		("subordinate", to_json_feature s); ("lex", `String l);
		("entry", `String e)]

let from_json_features j =
	let p = from_json_feature (Yojson.Basic.Util.member "person" j) in
	let n = from_json_feature (Yojson.Basic.Util.member "number" j) in
	let g = from_json_feature (Yojson.Basic.Util.member "gender" j) in
	let c = from_json_feature (Yojson.Basic.Util.member "case" j) in
	let s = from_json_feature (Yojson.Basic.Util.member "subordinate" j) in
	let l = Yojson.Basic.Util.to_string (Yojson.Basic.Util.member "lex" j) in
	let e = Yojson.Basic.Util.to_string (Yojson.Basic.Util.member "entry" j) in
	{person = p; number = n; gender = g; case = c; subordinate = s; lex = l;
		entry = e}

let to_json_rule (f,lst) =
	let lst = [f] @ lst in
	`List (List.map to_json_features lst)

let from_json_rule j =
	let jlst = Yojson.Basic.Util.to_list j in
	let lst = List.map from_json_features jlst in
	let f = List.hd lst in
	let lst = List.tl lst in
	(f,lst)

let to_json_grammar g =
	`List (List.map to_json_rule g)

let from_json_grammar j =
	let jlst = Yojson.Basic.Util.to_list j in
	List.map from_json_rule jlst

let to_json_lang (name,g) =
	`List [`String name; to_json_grammar g]

let from_json_lang j =
	let jlst = Yojson.Basic.Util.to_list j in
	match jlst with
	| [name;g] -> (Yojson.Basic.Util.to_string name, from_json_grammar g)
	| _ -> failwith "Incorrect Yojson input"

let to_json_database d =
	`List (List.map to_json_lang d)

let from_json_database j =
	let jlst = Yojson.Basic.Util.to_list j in
	List.map from_json_lang jlst
