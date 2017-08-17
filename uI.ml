(* .ml file for the module UI which fulfills several user-interface purposes.
 * This file contains the code necessary to help the runner of the MGT 
 * function smoothly by formatting output and providing solutions to incorrect
 * inputs. *)

open Batteries
open Grammar
open Database
open Parser
open Translator
open ANSITerminal

exception QuittingEarly
exception Restart
exception WordList of string list
exception NoInput
exception Help
exception History
exception InvalidLang
exception InvalidYN

let disp = ANSITerminal.print_string

module HistoryEntry = struct
  type t = string * string * string * string
  let create_history_entry lf lt sf st = [(lf,lt,sf,st)]
  let print h =
  match h with
  	| (lf,lt,sf,st) ->
  		disp [Bold; Underlined; yellow] ("\n\n" ^ lf ^ " -> " ^ lt ^ "\n\n");
        disp [Bold; yellow] (sf ^ " -> " ^ st ^ "\n")
end

let history = ref ([] : HistoryEntry.t list)

let category_partofspeech c =
  match c with
  | "NP" -> "Name"
  | "N" -> "Noun"
  | "CV"
  | "IV"
  | "TV" -> "Verb"
  | "Det" -> "Article"
  | _ -> "Other"  

let get_words grammar =
  let only_words =
  	List.filter (fun x -> (Features.get_l (List.hd (snd x))) <> "") grammar in
  let wordlist =
  	List.map
  		(fun (x,y) -> (Features.get_e (List.hd y), category_partofspeech
  			(Features.get_e x))) only_words in
  List.sort_uniq compare wordlist

let sort_pos (lst : string list) =
  let lst = List.sort_uniq compare lst in
  if (not (List.mem "Other" lst)) then lst
  else
    let lst = List.filter (fun x -> x <> "Other") lst in
    (lst @ ["Other"])

let detect_keywords s =
  let lst = Str.split (Str.regexp " +") s in
  if (List.length lst) >= 1 && (List.hd lst) = "words"
  	then raise (WordList (List.tl lst))
  else if s = "exit" or s = "quit" or s = "end" then raise QuittingEarly
  else if s = "restart" then raise Restart
  else if s = "help" then raise Help
  else if s = "history" then raise History
  else if s = "" then raise NoInput
  else ()

let extract_lang s =
  let s = String.sub s 0 1 in
  let lst = List.filter (fun x -> fst x = s) shortened_forms_grammars in
  if List.length lst = 0 then raise InvalidLang else s

let extract_yn s =
  let s = String.sub s 0 1 in
  if s = "y" || s = "n" then s else raise InvalidYN

let disp_no_input () =
  disp [Bold; blue]
	"It looks like you didn't type anything. Please try that step again.\n\n"

let disp_invalid_lang () =
  disp [Bold; blue]
  	("\nIt looks like you typed a language that the MGT doesn't know. " ^
  	" Please try that step again.\n\n")

let disp_invalid_yn () =
  disp [Bold; blue]
  	("\nIt looks like the MGT didn't know what to do there. " ^
	" Please type either of "); 
	disp [Bold; green] "Yes ";
	disp [Bold; blue] "or ";
	disp [Bold; green] "No";
	disp [Bold; blue] ".\n\n"

let disp_instructions () =
  disp [Bold; magenta]
  	"\n\nIf you want to exit the MGT at any point, please type any of ";
  disp [Bold; green] "Exit ";
  disp [Bold; magenta] "or ";
  disp [Bold; green] "Quit ";
  disp [Bold; magenta] "or ";
  disp [Bold; green] "End";
  disp [Bold; magenta] ".\n\n";

  disp [Bold; magenta]
  	"If you want to restart the MGT at any point, please type ";
  disp [Bold; green] "Restart";
  disp [Bold; magenta] ".\n\n";

  disp [Bold; magenta]
  	"If you want to know which words the MGT knows at any point, please type ";
  disp [Bold; green] "Words";
  disp [Bold; magenta] ".\n\n";

  disp [Bold; magenta] "For word lists of a specific language, please type ";
  disp [Bold; green] "Words ";
  disp [Bold; green] "<Language Name>";

  disp [Bold; magenta] ".\n\n";

  disp [Bold; magenta]
  	"If you want to view your saved translations at any point, please type ";
  disp [Bold; green] "History";
  disp [Bold; magenta] ".\n\n";

  disp [Bold; magenta] "To view these instructions at any point, please type ";
  disp [Bold; green] "Help";
  disp [Bold; magenta] ".\n\n\n\n"

let run_translator lang_from lang_to string_from =
  match (lang_from, lang_to) with
  	| ("e", "e") -> english_to_english string_from
 	| ("e", "g") -> english_to_german string_from
 	| ("e", "i") -> english_to_italian string_from
 	| ("i", "g") -> italian_to_german string_from
 	| ("i", "e") -> italian_to_english string_from
 	| ("g", "e") -> german_to_english string_from
 	| ("g", "i") -> german_to_italian string_from
 	| ("i", "i") -> italian_to_italian string_from
 	| ("g", "g") -> german_to_german string_from
	| _ -> []

let add_to_history yn lf lt sf st =
 match yn with
   | "y" ->
   	  let new_lf = shortened_form_to_string lf in
      let new_lt = shortened_form_to_string lt in
      let translated_sf = run_translator lf lf sf in
      let sf =
        if List.length translated_sf = 0 then "<Invalid input>" 
        else List.hd translated_sf in
      let len = List.length st in
        let st_string =
          if len = 0 then "<No translation was available>"
          else if len = 1 then (List.hd st)
          else List.fold_left
          	(fun acc x -> acc ^ " / " ^ x)
          	(List.hd st) (List.tl st) in
      history := ((!history) @
      	(HistoryEntry.create_history_entry new_lf new_lt sf st_string))
   | "n" -> ()
   | _ -> raise (Failure "Something went wrong")

let disp_words_formatting lst =
  match lst with
    | [] -> disp [Bold;yellow] "\n"
    | [x] -> disp [Bold;yellow]  (x ^ "\n")
    | h::t ->
    	disp [Bold;yellow]
    	  ((List.fold_left (fun acc x -> acc ^ ", " ^ x) h t ) ^ "\n")


let rec disp_words lst =
  let lst = List.map String.capitalize lst in
  match lst with
    | [] ->
        let available = List.map fst original_forms_grammars in
    	List.iter (fun name -> disp_words [name]) available
    | [name] ->
    	let name = String.lowercase (String.sub name 0 1) in
        let grammar = original_form_to_grammar name in
        let name = shortened_form_to_string name in
        disp [Bold; yellow] ("\nThe available words for " ^ name ^ " are:\n\n");
        let wordlist = get_words grammar in
        let parts_of_speech = List.sort_uniq compare (List.map snd wordlist) in
        let parts_of_speech = sort_pos parts_of_speech in
        List.iter
          (fun x -> disp [Bold; yellow] (x ^ "s: ");
            let pos_words = List.filter (fun y -> x = snd y) wordlist in
            let pos_words = List.map fst pos_words in
            disp_words_formatting pos_words)
          parts_of_speech;
        print_newline ()  
   | _ -> raise (Failure "Something went wrong")

let disp_history () =
  let lst = !history in
  let _ =
  	if List.length lst = 0 then
  	  disp [Bold; yellow] "\n\nYou currently have no saved translations.\n\n"
  	else
   	  (List.iter HistoryEntry.print lst; print_newline ()) in
  print_newline ()

let disp_results lst =
  List.iter (fun x -> disp [Bold; yellow] (x^"\n")) lst