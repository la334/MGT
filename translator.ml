(* .ml file for the module Translator which performs the translation portion
 * of the MGT.
 * This file contains the code necessary to produce a list of parsed trees
 * which represent the possible translations based on the provided parameters.*)

open Batteries
open Grammar
open Database
open Parser

type translate_tree =
  TLeaf of Features.t list | TBranch of Features.t * translate_tree list

let rec translate_to t g =
  let treelex_to_cat t = (match t with
    | Leaf(x) -> Features.get_l x
    | Branch(x, lst) -> Features.get_e x) in

  let right_rule (r:rule) lst =
    (let rc = snd (rule_categories_lexes r) in
    let rc2 = snd(rule_categories_entries r) in
    let lstc = List.map treelex_to_cat lst in
    if (rc=lstc || rc2=lstc) then lst
    else if (rc=(List.rev(lstc))||rc2=(List.rev(lstc))) then List.rev lst
    else []) in

  match t with
  | Leaf(ft) ->
      let res : Grammar.rule list = List.filter
        (fun x -> let comp = List.hd(snd x) in
          (Features.equals_ft ft comp) &&
          (Features.get_l ft = Features.get_l comp))
        g in
      let res = List.map (fun x -> List.hd (snd x)) res in
      TLeaf(res)
  | Branch(ft,lst) ->
      let res = List.filter
        (fun x -> (Features.get_e ft) = (fst (rule_categories_lexes x))) g in
      let new_res = List.fold_left
        (fun acc x ->
          let addition = right_rule x lst in
            if (addition = []) then acc
            else if (List.mem (List.hd addition) acc) then acc
            else acc@addition)
        [] res in
      let new_res = List.map (fun x -> translate_to x g) new_res in
      TBranch(ft,new_res)

(* [linearize t] is the list of string lists representing translate_tree [t]. *)
let rec linearize t =
match t with
| TLeaf(lst) -> let lst = List.map Features.get_e lst in [lst]
| TBranch (_,lst) -> List.fold_left (fun acc x -> acc@(linearize x)) [] lst

(* [to_string lst] is the list of possible strings represented by the list
 * [lst]. For example, the list [[A]] becomes [A], the list [[A;B]] becomes
 * [A;B], the list [[A];[B]] becomes [A B], the list [[A;B];[C]] becomes
 * [A C; B C], the list [[A];[B;C]] becomes [A C; B C], and the list
 * [[A;B];[C;D]] becomes the list [A C; B C; A D; B D]. *)
let rec to_string lst =
  match lst with
  | [] -> []
  | [x] -> x
  | h1::h2::t ->
      let n = compare (List.length h1) (List.length h2) in
      let comb =
        if n > 0 then
          List.fold_left
            (fun acc x ->
              acc @ (List.map (fun y -> y ^ " " ^ x) h1))
            [] h2
        else
          List.fold_left
            (fun acc x ->
              acc @ (List.map (fun y -> x ^ " " ^ y) h2))
          [] h1 in
      to_string (comb::t)

let rec german_movement t =

  let tree_to_categories t =
    let treeval_to_cat t =
      match t with
        | TLeaf(x) -> ""
        | TBranch(x, lst) -> Features.get_e x in
    match t with
      | TBranch(x, lst) ->
          let daughters =
            List.map (fun x -> treeval_to_cat x) lst in
            (Features.get_e x,daughters)
      | _ -> ("",[""]) in

  match t with
  | TLeaf(f) -> TLeaf(f)
  | TBranch(f,lst) ->
      let cats = tree_to_categories t in
      let new_lst =
        if (cats = ("VP",["NP";"TV"]) && not (Features.is_subordinate f))
          then List.rev lst
        else lst in
      let new_lst = List.map german_movement new_lst in
      TBranch(f,new_lst)

let english_to_english str =
  let res = feature_parse english str in
  let res = List.map (fun x -> translate_to x english) res in
  let res = List.map linearize res in
  let res = List.sort_uniq compare (List.flatten (List.map to_string res)) in
  let res = List.map (feature_parse english) res in
  let res = List.flatten res in
  let res = List.map Parser.to_string res in
  List.sort_uniq compare res

let italian_to_italian str =
  let res = feature_parse italian str in
  let res = List.map (fun x -> translate_to x italian) res in
  let res = List.map linearize res in
  let res = List.sort_uniq compare (List.flatten (List.map to_string res)) in
  let res = List.map (feature_parse italian) res in
  let res = List.flatten res in
  let res = List.map Parser.to_string res in
  List.sort_uniq compare res

let german_to_german str =
  let res = feature_parse german str in
  let res = List.map (fun x -> translate_to x german) res in
  let res = List.map linearize res in
  let res = List.sort_uniq compare (List.flatten (List.map to_string res)) in
  let res = List.map (feature_parse german) res in
  let res = List.flatten res in
  let res = List.map Parser.to_string res in
  List.sort_uniq compare res

let english_to_italian str =
  let res = feature_parse english str in
  let res = List.sort_uniq compare res in
  let res = List.map (fun x -> translate_to x italian) res in
  let res = List.map linearize res in
  let res = List.sort_uniq compare (List.flatten (List.map to_string res)) in
  let res = List.map (feature_parse italian) res in
  let res = List.flatten res in
  let res = List.map Parser.to_string res in
  List.sort_uniq compare res

let italian_to_english str =
  let res = feature_parse italian str in
  let res = List.map (fun x -> translate_to x english) res in
  let res = List.map linearize res in
  let res = List.sort_uniq compare (List.flatten (List.map to_string res)) in
  let res = List.map (feature_parse english) res in
  let res = List.flatten res in
  let res = List.map Parser.to_string res in
  List.sort_uniq compare res

let only_not_sub lst =
  List.filter
    (fun x -> match x with
      | Leaf(ft) -> not (Features.is_subordinate ft)
      | Branch(ft,lst) -> not (Features.is_subordinate ft))
    lst

let english_to_german str =
  let res = feature_parse english str in
  let res = List.map (fun x -> translate_to x german) res in
  let res = List.map german_movement res in
  let res = List.map linearize res in
  let res = List.sort_uniq compare (List.flatten (List.map to_string res)) in
  let res = List.map (feature_parse german) res in
  let res = List.flatten res in
  let res = only_not_sub res in
  let res = List.map Parser.to_string res in
  List.sort_uniq compare res

let italian_to_german str =
  let res = feature_parse italian str in
  let res = List.map (fun x -> translate_to x german) res in
  let res = List.map german_movement res in
  let res = List.map linearize res in
  let res = List.sort_uniq compare (List.flatten (List.map to_string res)) in
  let res = List.map (feature_parse german) res in
  let res = List.flatten res in
  let res = only_not_sub res in
  let res = List.map Parser.to_string res in
  List.sort_uniq compare res

let german_to_english str =
  let res = feature_parse german str in
  let res = only_not_sub res in
  let res = List.map (fun x -> translate_to x english) res in
  let res = List.map german_movement res in
  let res = List.map linearize res in
  let res = List.sort_uniq compare (List.flatten (List.map to_string res)) in
  let res = List.map (feature_parse english) res in
  let res = List.flatten res in
  let res = List.map Parser.to_string res in
  List.sort_uniq compare res
 
let german_to_italian str =
  let res = feature_parse german str in
  let res = only_not_sub res in
  let res = List.map (fun x -> translate_to x italian) res in
  let res = List.map german_movement res in
  let res = List.map linearize res in
  let res = List.sort_uniq compare (List.flatten (List.map to_string res)) in
  let res = List.map (feature_parse italian) res in
  let res = List.flatten res in
  let res = List.map Parser.to_string res in
  List.sort_uniq compare res

