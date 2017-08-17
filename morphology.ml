(* .ml file for the module Morphology of the MGT which defines the
 * morphological rules for the grammars used by the MGT.
 * This file contains the code which assigns morphological rules to a
 * grammar. *)

open Grammar
open FeatVal
open Feature
open Features

(* [find_rules_of_cat c g] is the list of rules with head category [c] in 
 * grammar [g]. *)
let find_rules_of_cat c grammar =
	let only_words =
  	List.filter (fun x -> (get_l (List.hd (snd x))) <> "") grammar in
  List.filter (fun x -> (get_e (fst x)) = c) only_words

(* [str_ends_in s lst] is [true] if the string [s] ends in any element of [lst]
 * and [false] otherwise. *)
let rec str_ends_in s lst = try(
	match lst with
	| [] -> false
	| h::t ->
			let re = Str.regexp (Str.quote h ^ "$") in
			let _ = Str.search_forward re s 0 in
			true
) with | _ -> str_ends_in s (List.tl lst)

(* [apply_to_rule r lst g] removes rule [r] from grammar [g] and replaces it
 * with the rule list [lst]. *)
let apply_to_rule r lst grammar =
	let grammar = List.filter (fun x -> x <> r) grammar in
	grammar @ lst


(* [get_verbs g] is the list of verbs in grammar [g]. *)
let get_verbs grammar =
	List.fold_left
		(fun acc x -> acc @ (find_rules_of_cat x grammar)) [] ["TV"; "IV"; "CV"]

(* [get_nouns g] is the list of nouns in grammar [g]. *)
let get_nouns grammar =
	find_rules_of_cat "N" grammar

(* [english_verbs r g] is the grammar created by applying English verb
 * morphology to rule [r] in grammar [g]. *)
let english_verbs r grammar =
	let rule_head = fst r in
	let word_fs = List.hd (snd r) in
	let base = get_e word_fs in
	let expanded_base = base ^ "s" in
	let rule_1s = set_n (set_p word_fs (Unifier First)) (Unifier Sing) in
	let rule_2s = set_n (set_p word_fs (Unifier Second)) (Unifier Sing) in
	let rule_3s =
		set_e (set_n (set_p word_fs (Unifier Third)) (Unifier Sing)) expanded_base
	in
	let rule_1p = set_n (set_p word_fs (Unifier First)) (Unifier Pl) in
	let rule_2p = set_n (set_p word_fs (Unifier Second)) (Unifier Pl) in
	let rule_3p = set_n (set_p word_fs (Unifier Third)) (Unifier Pl) in
	let lst = [rule_1s; rule_2s; rule_3s; rule_1p; rule_2p; rule_3p] in
	let lst = List.map (fun x -> (rule_head, [x])) lst in
	apply_to_rule r lst grammar

(* [english_nouns r g] is the grammar created by applying English noun
 * morphology to rule [r] in grammar [g]. *)
let english_nouns r grammar =
	let rule_head = fst r in
	let word_fs = List.hd (snd r) in
	let base = get_e word_fs in
	let expanded_base =
		if (str_ends_in base ["s";"sh";"ch";"x";"z"]) then base ^ "es"
		else base ^ "s" in
	let rule_p = set_e (set_n word_fs (Unifier Pl)) expanded_base in
	let rule_s = set_e (set_n word_fs (Unifier Sing)) base in
	let lst = [rule_s;rule_p] in
	let lst = List.map (fun x -> (rule_head, [x])) lst in
	apply_to_rule r lst grammar

let english_morphology grammar =
	let verbs = get_verbs grammar in
	let nouns = get_nouns grammar in
	let expanded =
		List.fold_left (fun acc x -> english_verbs x acc) grammar verbs in
	List.fold_left (fun acc x -> english_nouns x acc) expanded nouns

(* [are_rule f] is the list of feature bundles meant to replace the rule
 * containing [f], which is an -are verb. *)
let are_rule word_fs =
	let stem =
		Str.global_replace
			(Str.regexp (Str.quote "are" ^ "$")) "" (get_e word_fs) in
	let fs = stem ^ "o" in
	let ss = stem ^ "i" in
	let ts = stem ^ "a" in
	let fp = stem ^ "iamo" in
	let sp = stem ^ "ate" in
	let tp = stem ^ "ano" in
	let rule_1s =
		set_e (set_n (set_p word_fs (Unifier First)) (Unifier Sing)) fs in
	let rule_2s =
		set_e (set_n (set_p word_fs (Unifier Second)) (Unifier Sing)) ss in
	let rule_3s =
		set_e (set_n (set_p word_fs (Unifier Third)) (Unifier Sing)) ts in
	let rule_1p =
		set_e (set_n (set_p word_fs (Unifier First)) (Unifier Pl)) fp in
	let rule_2p =
		set_e (set_n (set_p word_fs (Unifier Second)) (Unifier Pl)) sp in
	let rule_3p =
		set_e (set_n (set_p word_fs (Unifier Third)) (Unifier Pl)) tp in
	[rule_1s; rule_2s; rule_3s; rule_1p; rule_2p; rule_3p]

(* [ere_rule f] is the list of feature bundles meant to replace the rule
 * containing [f], which is an -ere verb. *)
let ere_rule word_fs =
	let stem =
		Str.global_replace
			(Str.regexp (Str.quote "ere" ^ "$")) "" (get_e word_fs) in
	let fs = stem ^ "o" in
	let ss = stem ^ "i" in
	let ts = stem ^ "e" in
	let fp = stem ^ "iamo" in
	let sp = stem ^ "ete" in
	let tp = stem ^ "ono" in
	let rule_1s =
		set_e (set_n (set_p word_fs (Unifier First)) (Unifier Sing)) fs in
	let rule_2s =
		set_e (set_n (set_p word_fs (Unifier Second)) (Unifier Sing)) ss in
	let rule_3s =
		set_e (set_n (set_p word_fs (Unifier Third)) (Unifier Sing)) ts in
	let rule_1p =
		set_e (set_n (set_p word_fs (Unifier First)) (Unifier Pl)) fp in
	let rule_2p =
		set_e (set_n (set_p word_fs (Unifier Second)) (Unifier Pl)) sp in
	let rule_3p =
		set_e (set_n (set_p word_fs (Unifier Third)) (Unifier Pl)) tp in
	[rule_1s; rule_2s; rule_3s; rule_1p; rule_2p; rule_3p]


(* [ire_rule f] is the list of feature bundles meant to replace the rule
 * containing [f], which is an -ire verb. *)
let ire_rule word_fs =
	let stem =
		Str.global_replace
			(Str.regexp (Str.quote "ire" ^ "$")) "" (get_e word_fs) in
	let fs = stem ^ "o" in
	let ss = stem ^ "i" in
	let ts = stem ^ "e" in
	let fp = stem ^ "iamo" in
	let sp = if (stem = "dic") then "dite" else stem ^ "ite" in
	let tp = stem ^ "ono" in
	let rule_1s =
		set_e (set_n (set_p word_fs (Unifier First)) (Unifier Sing)) fs in
	let rule_2s =
		set_e (set_n (set_p word_fs (Unifier Second)) (Unifier Sing)) ss in
	let rule_3s =
		set_e (set_n (set_p word_fs (Unifier Third)) (Unifier Sing)) ts in
	let rule_1p =
		set_e (set_n (set_p word_fs (Unifier First)) (Unifier Pl)) fp in
	let rule_2p =
		set_e (set_n (set_p word_fs (Unifier Second)) (Unifier Pl)) sp in
	let rule_3p =
		set_e (set_n (set_p word_fs (Unifier Third)) (Unifier Pl)) tp in
	[rule_1s; rule_2s; rule_3s; rule_1p; rule_2p; rule_3p]

(* [italian_verbs r g] is the grammar created by applying Italian verb
 * morphology to rule [r] in grammar [g]. *)
let italian_verbs r grammar =
	let rule_head = fst r in
	let word_fs = List.hd (snd r) in
	let base = get_e word_fs in
	let word_fs = if (base = "dire") then set_e word_fs "dicire" else word_fs in
	let base = if (base = "dire") then "dicire" else base in
	let lst =
		if (str_ends_in base ["are"]) then are_rule word_fs
		else if (str_ends_in base ["ere"]) then ere_rule word_fs
		else ire_rule word_fs in
	let lst = List.map (fun x -> (rule_head, [x])) lst in
	apply_to_rule r lst grammar

(* [italian_nouns r g] is the grammar created by applying Italian noun
 * morphology to rule [r] in grammar [g]. *)
let italian_nouns r grammar =
	let rule_head = fst r in
	let word_fs = List.hd (snd r) in
	let base = get_e word_fs in
	let gen = get_g word_fs in
	let stem = String.sub base 0 ((String.length base) - 1) in
	let ending = String.sub base ((String.length base) - 1) 1 in
	let pl =
		if ((str_ends_in base ["cia";"gia"]) && gen = Unifier Fem) then
			(let new_stem = String.sub base 0 ((String.length base) - 2) in
			 new_stem ^ "e")
		else if (ending = "o") then (stem ^ "i")
		else if (ending = "a" && gen = Unifier Masc) then (stem ^ "i")
		else if (ending = "a" && gen = Unifier Fem) then (stem ^ "e")
		else if (ending = "e") then (stem ^ "i")
	else stem
	in
	let pl =
		if (str_ends_in pl ["ci";"ce";"gi";"ge"]) then
			(let new_stem = String.sub pl 0 ((String.length pl) - 1) in
			let new_ending = String.sub pl ((String.length pl) - 1) 1 in
			new_stem ^ "h" ^ new_ending)
		else pl in
	let rule_p = set_e (set_n word_fs (Unifier Pl)) pl in
	let rule_s = set_e (set_n word_fs (Unifier Sing)) base in
	let lst = [rule_p;rule_s] in
	let lst = List.map (fun x -> (rule_head, [x])) lst in
	apply_to_rule r lst grammar

let italian_morphology grammar =
	let verbs = get_verbs grammar in
	let nouns = get_nouns grammar in
	let expanded =
		List.fold_left (fun acc x -> italian_verbs x acc) grammar verbs in
	List.fold_left (fun acc x -> italian_nouns x acc) expanded nouns

(* [german_verbs r g] is the grammar created by applying German verb morphology
 * to rule [r] in grammar [g]. *)
let german_verbs r grammar =
	let rule_head = fst r in
	let word_fs = List.hd (snd r) in
	let lst =
			let stem =
				Str.global_replace
					(Str.regexp (Str.quote "en" ^ "$")) "" (get_e word_fs) in
			let new_stem = Str.replace_first (Str.regexp "seh") "sieh" stem in
			let is_den_ten = str_ends_in (get_e word_fs) ["den";"ten"] in
			let fs = stem ^ "e" in
			let ss = if is_den_ten then new_stem ^ "et" else new_stem ^ "st" in
			let ts = if is_den_ten then new_stem ^ "et" else new_stem ^ "t" in
			let fp = stem ^ "en" in
			let sp = if is_den_ten then stem ^ "et" else stem ^ "t" in
			let tp = stem ^ "en" in
			let rule_1s =
				set_e (set_n (set_p word_fs (Unifier First)) (Unifier Sing)) fs in
			let rule_2s =
				set_e (set_n (set_p word_fs (Unifier Second)) (Unifier Sing)) ss in
			let rule_3s =
				set_e (set_n (set_p word_fs (Unifier Third)) (Unifier Sing)) ts in
			let rule_1p =
				set_e (set_n (set_p word_fs (Unifier First)) (Unifier Pl)) fp in
			let rule_2p =
				set_e (set_n (set_p word_fs (Unifier Second)) (Unifier Pl)) sp in
			let rule_3p =
				set_e (set_n (set_p word_fs (Unifier Third)) (Unifier Pl)) tp in
			[rule_1s; rule_2s; rule_3s; rule_1p; rule_2p; rule_3p] in
	let lst = List.map (fun x -> (rule_head, [x])) lst in
	apply_to_rule r lst grammar

(* [german_nouns r g] is the grammar created by applying German noun morphology
 * to rule [r] in grammar [g]. *)
let german_nouns r grammar =
	let rule_head = fst r in
	let word_fs = List.hd (snd r) in
	let base = get_e word_fs in
	let gen = get_g word_fs in
	let pl = if (gen = Unifier Masc) then base ^ "e" else base ^ "n" in
	let rule_p = set_e (set_n word_fs (Unifier Pl)) pl in
	let rule_s = set_e (set_n word_fs (Unifier Sing)) base in
	let lst = [rule_p;rule_s] in
	let lst = List.map (fun x -> rule_head, [x]) lst in
	apply_to_rule r lst grammar

let german_morphology grammar =
	let verbs = get_verbs grammar in
	let nouns = get_nouns grammar in
	let expanded =
		List.fold_left (fun acc x -> german_verbs x acc) grammar verbs in
	List.fold_left (fun acc x -> german_nouns x acc) expanded nouns