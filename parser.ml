(* .ml file for the module Parser of the MGT which parses a string.
 * This file contains the code necessary to chart-parse a string as well as
 * to parse a string using the Lexical Functional Grammar theory (i.e., using
 * features and unification. *)

open Batteries
open Grammar

type splitpoint = Terminal | Chain | Position of int

module OrderedSymbol : (Set.OrderedType with type t = Features.t) = struct
  type t = Features.t
  let compare = compare
end

module SymbolSet = BatSet.Make(OrderedSymbol)

module CellEntry = struct
  type t = Features.t * rule * splitpoint

  let cell_feature e =
  match e with
  | (ft,r,sp) -> ft
  let cell_rule e =
  match e with
  | (ft,r,sp) -> r
let cell_splitpoint e =
  match e with
  | (ft,r,sp) -> sp
end

module OrderedTriple : (Set.OrderedType with type t = CellEntry.t) =
struct
  type t = CellEntry.t
  let compare = compare
end
module TripleSet = Set.Make(OrderedTriple)

type chart = TripleSet.t array array
exception StrangeChart

(* [wordparser lang j words] assigns initial word classes as described by
 * grammar [lang] to the list of words [words] at point [j] in the chart. 
 * This function is equivalent to the line
 * {A | A -> word(sub)j} 
 * in the chart-parsing algorithm. *)
let wordparser grammar j lst =
  let (=~) s1 s2 = (String.lowercase s1) = (String.lowercase s2) in
  let word = List.nth lst (j-1) in
  let elts =
    List.filter (fun x ->
      (word =~ (Features.get_e (List.hd (snd x))) && (List.length (snd x) = 1)))
    grammar in
  List.fold_left
    (fun acc x ->
      let new_set = TripleSet.singleton(fst x, x,Terminal) in
      TripleSet.union acc new_set)
    TripleSet.empty elts

(* [get_parsed_rule lang s1 s2 k] is the set of cell entries at position [k]
 * created by taking all the rules X -> AB of G where A is any rule described
 * by [s1], B is any rule described by [s2], and G is a grammar [lang], and by
 * taking the point k and the feature bundle of X as parameters for the new set.
 * This function is equivalent to the line
 * (chart(i,j)) * (chart(k,j))
 * in the chart-parsing algorithm *)
let get_parsed_rule grammar s1 s2 k =
  let rule_parser grammar k c1 c2 =
  (let elts =
    List.filter (fun x ->
      (snd (rule_categories_entries x)) =
        [Features.get_e (CellEntry.cell_feature c1);
        Features.get_e (CellEntry.cell_feature c2)])
    grammar in
  List.fold_left (fun acc x ->
                    let new_set =
                      TripleSet.singleton ((fst x), x, Position(k)) in
                    TripleSet.union acc new_set)
  TripleSet.empty elts) in

  let allparsedpairs f set1 set2 = 
    (let rec flatten l = match l with
        ss::rest -> TripleSet.union ss (flatten rest)
      | [] -> TripleSet.empty in
    flatten
      [? List : (f x y) |  x <- TripleSet : set1 ; y <- TripleSet : set2 ?]) in

  allparsedpairs (rule_parser grammar k) s1 s2

(* [setlimit f s] applies function [f] to set [s] until no longer possible. *)
let rec setlimit f s =
  let next = f s in if (TripleSet.equal s next) then s else setlimit f next

(* [unary_rules lang s] is the function which imposes unary rules from a grammar
 * [lang] onto string [s] in order to create a set of cell entries. *)
let unary_rules grammar s =

  let find_unary_cats grammar c=
    let ans =
      List.filter (fun x ->
        (c = (Features.get_e (List.hd (snd x)))) && (List.length (snd x) = 1))
      grammar in
    List.fold_left
      (fun acc r -> let new_set = TripleSet.singleton(fst r, r, Chain) in
        TripleSet.union acc new_set)
    (TripleSet.empty) ans in

  let categories ts =
    [? SymbolSet:(CellEntry.cell_feature triple) | triple <- TripleSet: ts ?] in
  
  let catset = categories s in
  let catlst = SymbolSet.elements catset in
  let catlst = List.map Features.get_e catlst in
  List.fold_left
    (fun acc x -> let new_set = find_unary_cats grammar x in
      TripleSet.union acc new_set)
    s catlst

let chart_parse grammar str =
  try(let str = String.lowercase str in
  let lst = String.split_on_char ' ' str in
  let n = List.length lst in
  let chart = Array.make_matrix (n+1) (n+1) TripleSet.empty in
  (for j = 1 to n do
    chart.(j-1).(j) <- (wordparser grammar j lst);
    let new_set = setlimit (unary_rules grammar) (chart.(j-1).(j)) in
    chart.(j-1).(j) <- TripleSet.union chart.(j-1).(j) new_set;
    (for i = (j-2) downto 0 do
      (for k = (i+1) to (j-1) do
        let fst_set = setlimit (unary_rules grammar) (chart.(i).(j)) in
        chart.(i).(j) <- TripleSet.union chart.(i).(j) fst_set;
        let grammar_check =
          get_parsed_rule grammar (chart.(i).(k)) (chart.(k).(j)) k in
        let new_set = TripleSet.union (chart.(i).(j)) grammar_check in
        chart.(i).(j) <- new_set;
      done);
    done);
  done);
  chart) with | _ -> failwith "invid string"

module Fs = Features
module F = Feature
module FV = FeatVal

type tree = Leaf of Fs.t | Branch of Fs.t * tree list

(* [make_tree ch s (i,j)] is the tree created by converting the chart [ch]
 * at point ([i],[j]) to a tree with a topmost node category of a feature
 * bundle containing [s]. *)
let rec make_tree ch (cat : string) (i,j) =
  
  let rooted_in_cat = 
    [? List:entry | entry <- TripleSet:ch.(i).(j) ;
      (Fs.get_e (CellEntry.cell_feature entry) = cat) ?] in

  let trees_of_entry (e : TripleSet.elt) =
    let category = CellEntry.cell_feature e in
    let lst = (snd (CellEntry.cell_rule e)) in
    let point = CellEntry.cell_splitpoint e in
    match (lst, point) with
      | ([word],Terminal) -> [Branch (category,[Leaf word])]
      | ([dtr],Chain) ->
          [? List:Branch(category,t) |
            t <- List:(make_tree ch (Fs.get_e dtr) (i,j)) ?]
      | ([dtr1;dtr2],(Position k)) ->
          [? List:Branch(category,[t1;t2]) |
            t1 <- List:
              (List.flatten (make_tree ch (Fs.get_e dtr1) (i,k))) ;
            t2 <- List:
              (List.flatten (make_tree ch (Fs.get_e dtr2) (k,j))) ?]
  | _ -> raise StrangeChart in

   List.map trees_of_entry rooted_in_cat

(* [extract_fts t] is the feature bundle at the topmost node of tree [t]. *)
let extract_fts t = 
  match t with
| Leaf(f) -> f
| Branch(f,_) -> f   

let tree_to_categories t =
  let treeval_to_cat t =
    match t with
      | Leaf(x) -> Fs.get_e x
    |   Branch(x, lst) -> Fs.get_e x in
  match t with
    | Branch(x, lst) ->
        let daughters =
          List.map (fun x -> treeval_to_cat x) lst in
          (Fs.get_e x,daughters)
    | _ -> ("",[""]) 
  
(* [impose_ft t lang] is the list of trees created once features as described by
 * the grammar [lang] have been imposed onto each node of [t]. *)
let rec impose_ft t (grammar:grammar) : tree list=

  let rules_to_tree grammar =

    let rule_to_tree rule =
      let leaves = List.fold_left (fun acc x -> acc@[Leaf(x)]) [] (snd rule) in
      Branch(fst rule, leaves) in

    List.map (fun x -> rule_to_tree x) grammar in

  let add_back_categories t (grammar:grammar) =

    let change_ft t ft = 

      let insertrule t1 t2 = (match (t1,t2) with
        | (Leaf(x),Leaf(y)) -> Leaf(y)
        | (Branch(x,d1),(Leaf(y))) -> Branch(y,d1)
        | _ -> raise (Failure "Impossible Match Case")) in
      match t with
      | Leaf(x) -> Leaf(x)
      | Branch(x,lst) ->
        let kids = if (ft=[]) then lst else ft in
        let final = List.map2 (fun x y -> insertrule x y) lst kids in
        Branch(x,final) in

    let ft_of_tree t = (match t with
      | Leaf(x) -> []
      | Branch(x,lst) -> lst) in

    let ruletrees = rules_to_tree grammar in (*THIS LINE IS FINE*)
    let res = List.filter (*THIS IS THE PROBLEM*)
      (fun x -> let f1 = extract_fts t in let f2 = extract_fts x in
        let ans1 = (Fs.equals_ft f1 f2) in let ans2 =
          (tree_to_categories t = tree_to_categories x) in
          ans1 && ans2)
      ruletrees in
    List.map (fun r -> 
    match (t,r) with
    | (Leaf(x),Leaf(y)) -> Leaf(x)
    | (Branch(z,kids),Branch(_,_)) -> change_ft t (ft_of_tree r)
    | _ -> failwith "Impossible Match Case") res in

  let get_combinations lst =

    let listify lst =
      List.map (fun x -> List.map (fun y -> [y]) x) lst in

    let rec combine lst =
      match lst with
      | [] -> []
      | [x] -> x
      | h1::h2::t ->
          let n = compare (List.length h1) (List.length h2) in
          let comb =
            if n > 0 then
              List.fold_left
                (fun acc x ->
                  acc@(List.map (fun y -> y @ x) h1))
                [] h2
            else
              List.fold_left
                (fun acc x ->
                  acc@(List.map (fun y -> x @ y) h2))
              [] h1 in
          combine (comb::t) in

    let lst = listify lst in
    combine lst in

  match t with
  | Leaf(_) -> [t]
  | Branch(x,lst) ->
      let new_nodes = add_back_categories t grammar in
      List.fold_left (fun acc n -> 
        let res =
          match n with
            | Leaf(_) -> failwith "Shouldn't happen"
            | Branch(y,lst2) ->
                let new_lsts = List.map (fun x -> impose_ft x grammar) lst2 in
                let new_lsts = get_combinations new_lsts in
                let new_branches = List.map (fun x -> Branch(y,x)) new_lsts in
                List.fold_left
                  (fun acc b -> acc@(add_back_categories b grammar))
                [] new_branches in
        acc@res) [] new_nodes

(* [trickle_ft t] is the tree created by trickling each of tree [t]'s
 * node's features down the tree until it cannot trickle any further (i.e.,
 * after reaching a terminal node). *)
let rec trickle_ft t =
  match t with
  | Leaf(_) -> t
  | Branch(ft, lst) ->
      let p = Fs.get_p ft in let n = Fs.get_n ft in let g = Fs.get_g ft in
      let c = Fs.get_c ft in let su = Fs.get_s ft in
      let lst =
        if (p = F.Clash) then lst
        else List.map
          (fun x -> match x with
            | Leaf(s) -> Leaf(Fs.replace_p s p)
            | Branch(s,kids) ->
                Branch(Fs.replace_p s p,kids)) lst in
                let lst =
                  if (n = F.Clash) then lst
                  else List.map
                    (fun x -> match x with
                      | Leaf(s) -> Leaf(Fs.replace_n s n)
                      | Branch(s,kids) ->
                          Branch(Fs.replace_n s n,kids))
                    lst in                        
                let lst =
                  if (g = F.Clash) then lst
                  else List.map
                    (fun x -> match x with
                      | Leaf(s) -> Leaf(Fs.replace_g s g)
                      | Branch(s,kids) ->
                          Branch(Fs.replace_g s g,kids)) lst in                        
                let lst =
                  if (c = F.Clash) then lst
                  else List.map
                    (fun x -> match x with
                      | Leaf(s) -> Leaf(Fs.replace_c s c)
                      | Branch(s,kids) ->
                          Branch(Fs.replace_c s c,kids)) lst in                        
                let lst =
                  if (su = F.Clash) then lst
                  else List.map
                    (fun x -> match x with
                      | Leaf(s) -> Leaf(Fs.replace_s s su)
                      | Branch(s,kids) ->
                          Branch(Fs.replace_s s su,kids)) lst in                        
                let lst = List.map trickle_ft lst in
                Branch(ft, lst)

(* [unify t] is the tree created by applying the unification algorithm to tree
 * [t]. *)
let rec unify t =

  let unify_test ft lst =
    let rec unify_lst acc ft =
      (let unifiedval =
        match ft with
          | F.Unifier(s) -> s
          | _ -> FV.Empty
      in
      match acc with
        | F.Any ->
            if (unifiedval <> FV.Empty) then F.Unifier(unifiedval) else F.Any
        | F.Only(s) -> F.Clash
        | F.Unifier(s) ->
            if (unifiedval = FV.Empty) then F.Unifier(s)
            else if (s <> unifiedval) then F.Clash
            else F.Unifier(s)
        | F.Clash ->  F.Clash
        | F.UnifyBlock(f) ->
            if (unifiedval = FV.Empty) then F.UnifyBlock(f)
            else if (f <> F.Any && f <> F.Only(unifiedval) &&
              f <> F.Unifier(unifiedval) && f <> F.UnifyBlock(ft))
              then F.Clash
            else F.UnifyBlock(ft)) in
      let plst = List.map Fs.get_p lst in
      let nlst = List.map Fs.get_n lst in
      let glst = List.map Fs.get_g lst in
      let clst = List.map Fs.get_c lst in
      let slst = List.map Fs.get_s lst in
      let unifiedp = List.fold_left unify_lst F.Any plst in
      let unifiedn = List.fold_left unify_lst F.Any nlst in
      let unifiedg = List.fold_left unify_lst F.Any glst in
      let unifiedc = List.fold_left unify_lst F.Any clst in
      let unifieds = List.fold_left unify_lst F.Any slst in
      let ft = Fs.replace_p ft unifiedp in
      let ft = Fs.replace_n ft unifiedn in
      let ft = Fs.replace_g ft unifiedg in
      let ft = Fs.replace_c ft unifiedc in
      Fs.replace_s ft unifieds in

  match t with
    | Leaf(_) -> t
    | Branch(ft,lst) ->
        let new_lst = List.map unify lst in
        let ft_lst = List.map extract_fts new_lst in
        let new_ft = unify_test ft ft_lst in
        Branch(new_ft,new_lst)


(* [post_unify t] is the tree created by cleaning up a tree [t] after the
 * unification process has been completed. *)
let rec post_unify t =

  let rec remove_block ft =
    match ft with
      | F.UnifyBlock(x) -> remove_block x
      | _ -> ft in

  match t with
    | Leaf(ft) -> 
        let new_p = remove_block (Fs.get_p ft) in
        let new_n = remove_block (Fs.get_n ft) in
        let new_g = remove_block (Fs.get_g ft) in
        let new_c = remove_block (Fs.get_c ft) in
        let new_su = remove_block (Fs.get_s ft) in
        let ft = Fs.set_p ft new_p in
        let ft = Fs.set_n ft new_n in
        let ft = Fs.set_g ft new_g in
        let ft = Fs.set_c ft new_c in
        let ft = Fs.set_s ft new_su in
        Leaf(ft)
    | Branch(ft,lst) ->
        let new_p = remove_block (Fs.get_p ft) in
        let new_n = remove_block (Fs.get_n ft) in
        let new_g = remove_block (Fs.get_g ft) in
        let new_c = remove_block (Fs.get_c ft) in
        let new_su = remove_block (Fs.get_s ft) in
        let new_lst = List.map post_unify lst in
        let ft = Fs.set_p ft new_p in
        let ft = Fs.set_n ft new_n in
        let ft = Fs.set_g ft new_g in
        let ft = Fs.set_c ft new_c in
        let ft = Fs.set_s ft new_su in
        Branch(ft,new_lst)

(* [no_clash t] is [true] if a tree [t] contains no unification clashes and 
 * [false] otherwise. *)
let rec no_clash t = 
match t with
  | Leaf(f) -> not (Fs.has_clash f)
  | Branch(f,lst) ->
      let branch_clash =
        List.fold_left (fun acc x -> acc && (no_clash x)) true lst in
      let head_clash = not (Fs.has_clash f) in
      branch_clash && head_clash
(* 
let dot_of_tree title t =

  let rec dot_of_node i =
    function
    | Leaf name -> 
        (("n"^(string_of_int i)^" [label = \""^(Features.to_string name)^
          "\"];\n"),i)
    | Branch (parent,kids) ->
        let (rootbyindexlist,maximum) = List.fold_left (fun (sofar,index) kid ->
          let (result,newindex) = dot_of_node (index+1) kid in
          ((result,(index+1))::sofar,newindex)
                                        )
            ([],i)
            kids in
        let thisnode = ("n"^(string_of_int i)^" [label = \""^
          (Features.to_string parent)^"\"];\n") in
        let downarrows = List.fold_left (fun already (subtree,index) ->
          ("n"^(string_of_int i)^"-> n"^(string_of_int index)^";\n"^already)
                        )
            ""
            rootbyindexlist in
        let subtreedot = List.fold_left (fun already (subtree,index) ->
                                             subtree^already)
            ""
            rootbyindexlist in
        (thisnode^downarrows^subtreedot,maximum)

 in
  ("digraph \""^title^
    "\" {\n node [shape = plaintext]; \n edge [arrowhead = none]; \n"^
    (Pervasives.fst (dot_of_node 0 t))^"}")

let writetree name t =
  let oc = open_out name in
  output_string oc (dot_of_tree name t);
  close_out oc;;


let writetrees basename trees =
  let writetree' index tree =
    writetree (basename ^ (string_of_int index) ^ ".dot") tree in
  List.mapi writetree' trees *)

let feature_parse grammar str =
  let chart = chart_parse grammar str in
  let n = (Array.length chart) - 1 in
  let get_res cat = List.flatten (make_tree chart cat (0,n)) in
  let end_cats =
    List.sort_uniq compare
      ((List.map (fun x -> Fs.get_e (fst x))) grammar) in
  let res = List.fold_left (fun acc x -> acc @ (get_res x)) [] end_cats in
  (* let _ = writetrees "chart_parsed" res in *)
  let new_res = List.map (fun x -> impose_ft x grammar) res in
  let new_res = List.fold_left (fun acc x -> acc@x) [] new_res in
  let new_res = List.sort_uniq compare new_res in
  (* let new_res = List.flatten res in *)
  (* let _ = writetrees "imposed_ft" new_res in *)
  let new_res = List.map trickle_ft new_res in
  (* let _ = writetrees "trickled_ft" new_res in *)
  let new_res = List.map unify new_res in
  (* let _ = writetrees "unified_ft" new_res in *)
  let new_res = List.map post_unify new_res in
  (* let _ = writetrees "post_unified_ft" new_res in *)
  List.fold_left (fun acc x -> if(no_clash x) then x::acc else acc) [] new_res

let to_string t =
  
  let rec tree_to_str t =
    match t with
  | Leaf(x) -> (Fs.get_e x) ^ " "
  | Branch(x,lst) ->
    List.fold_left (fun acc x -> acc^(tree_to_str x)) "" lst in

  let str = tree_to_str t in
  String.sub str 0 ((String.length str)-1)