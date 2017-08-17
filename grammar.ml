(* .ml file for the module Grammar of the MGT which defines the rule-based
 * grammar used by the MGT.
 * This file contains the code which defines the modules for feature values
* and features as well the types for rules and grammars. *)

module FeatVal = struct
  type t =
    First | Second | Third |
    Sing | Pl |
    Masc | Fem |
    Nom | Acc |
    Yes | No |
    Empty | ClashError

  let equals v1 v2 =
    match (v1, v2) with
    | (First, First)
    | (Second, Second)
    | (Third, Third)
    | (Sing, Sing)
    | (Pl, Pl)
    | (Masc, Masc)
    | (Fem, Fem)
    | (Nom, Nom)
    | (Acc, Acc)
    | (Yes, Yes) 
    | (No, No) -> true
    | _ -> false
  end

module Feature = struct
  type t = Any | Only of FeatVal.t | Clash | Unifier of FeatVal.t
    | UnifyBlock of t

  let replace_ftval ftv replacev =
    let replacestr =
      (match replacev with
        | Only(s) -> s
        | Unifier(s) -> s
        | UnifyBlock(Only(s)) -> s
        | UnifyBlock(Unifier(s)) -> s
        | Clash -> FeatVal.ClashError
        | _ -> FeatVal.Empty) in
    match ftv with
      | Only(s) ->
          if (replacestr = FeatVal.Empty) then Only(s)
          else if (not (FeatVal.equals s replacestr)) then Clash
          else Only(replacestr)
      | Unifier(s) ->
          if (replacestr = FeatVal.Empty) then Unifier(s)
          else if (not (FeatVal.equals s replacestr)) then Clash
          else Unifier(replacestr)
      | UnifyBlock(s) ->
          if (replacestr = FeatVal.Empty) then UnifyBlock(s)
          else if
            (s <> Any && s <> Only(replacestr) && s <> Unifier(replacestr) &&
              s <> UnifyBlock(replacev)) then Clash
          else UnifyBlock(replacev)
      | _ ->
          if (replacestr = FeatVal.ClashError) then Clash
          else if (replacestr <> FeatVal.Empty) then replacev
          else ftv
  
  let rec equals_ftval f1 f2 =
   match (f1,f2) with
      | (Any, Any) -> true
      | (Any, Only (s)) -> true
      | (Any, Unifier(s)) -> true
      | (Any, UnifyBlock(f)) -> true
      | (Only(s), Any) -> true
      | (Unifier(s), Any) -> true
      | (UnifyBlock(f), Any) -> true
      | (Only(s1), Only(s2)) -> FeatVal.equals s1 s2
      | (Unifier(s1), Unifier(s2)) -> FeatVal.equals s1 s2
      | (Only(s1), Unifier(s2)) -> FeatVal.equals s1 s2
      | (Unifier(s1), Only(s2)) -> FeatVal.equals s1 s2
      | (Only(s), UnifyBlock(f)) -> equals_ftval (Only s) f
      | (UnifyBlock(f), Only(s)) -> equals_ftval (Only s) f
      | (Unifier(s), UnifyBlock(f)) -> equals_ftval (Unifier s) f
      | (UnifyBlock(f), Unifier(s)) -> equals_ftval (Unifier s) f
      | (UnifyBlock(f1), UnifyBlock(f2)) -> equals_ftval f1 f2
      | _ -> false
end

module Features = struct
  type elt = Feature.t
  type t =
    {person : elt;
    number : elt;
    gender : elt;
    case : elt;
    subordinate : elt;
    lex : string;
    entry : string}
  
  let no_ft =
    {person=Feature.Any;
      number=Feature.Any;
      gender=Feature.Any;
      case=Feature.Any;
      subordinate=Feature.Any;
      lex="";
      entry=""}
      
  let only_entry str = {no_ft with entry=str}

  let get_p f = f.person
  let get_g f = f.gender
  let get_n f = f.number
  let get_c f = f.case
  let get_s f = f.subordinate
  let get_l f = f.lex
  let get_e f = f.entry

  let equals_ft f1 f2 =
    (Feature.equals_ftval f1.person f2.person) &&
    (Feature.equals_ftval f1.gender f2.gender) &&
    (Feature.equals_ftval f1.number f2.number) &&
    (Feature.equals_ftval f1.case f2.case) &&
    (Feature.equals_ftval f1.subordinate f2.subordinate)

  let has_clash f =
    (f.person = Feature.Clash) ||
    (f.gender = Feature.Clash) ||
    (f.number = Feature.Clash) ||
    (f.case = Feature.Clash) ||
    (f.subordinate = Feature.Clash)

  let rec is_subordinate ft =
  match ft.subordinate with
  | Feature.Unifier(s) -> s=FeatVal.Yes
  | Feature.Only(s) -> s=FeatVal.Yes
  | _ -> false

  let set_p f v = {f with person=v}
  let set_g f v = {f with gender=v}
  let set_n f v = {f with number=v}
  let set_c f v = {f with case=v}
  let set_s f v = {f with subordinate=v}
  let set_l f v = {f with lex=v}
  let set_e f v = {f with entry=v}

  let replace_p f v = set_p f (Feature.replace_ftval (f.person) v)
  let replace_g f v = set_g f (Feature.replace_ftval (f.gender) v)
  let replace_n f v = set_n f (Feature.replace_ftval (f.number) v)
  let replace_c f v = set_c f (Feature.replace_ftval (f.case) v)
  let replace_s f v = set_s f (Feature.replace_ftval (f.subordinate) v)
end

type rule = Features.t * Features.t list
type grammar = rule list

let rule_categories_entries rule =
  let rightside = snd rule in
  let leftside = fst rule in
  let newright = List.map Features.get_e rightside in
  (leftside, newright)

let rule_categories_lexes rule =
  let rightside = snd rule in
  let leftside = fst rule in
  let newright = List.map Features.get_l rightside in
  (Features.get_e leftside, newright)