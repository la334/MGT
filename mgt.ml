(* .ml file for the main function which runs the MGT.
 * This file contains the code necessary to run the MGT according to various
 * user inputs. *)

open ANSITerminal
open Database
open UI

(* [keyword_handler k f s] decides the appropriate response to exception [k]
 * raised during execution of function [f] with argument [s]. [keyword_handler]
 * is used by functions [prompt_user_lang], [prompt_user_line], and
 * [prompt_user_yn] in order to recover from deliberate exceptions (i.e., the
 * user-defined exceptions). *)
let rec keyword_handler keyword fn s = 
  match keyword with
  | WordList lst -> disp_words lst; print_newline (); fn s
  | NoInput -> disp_no_input (); fn s
  | Help -> disp_instructions (); fn s
  | History -> disp_history (); fn s
  | InvalidLang -> disp_invalid_lang (); fn s
  | InvalidYN -> disp_invalid_yn (); fn s
  | QuittingEarly -> exit 0
  | x -> raise x

(* [prompt_user_lang s] displays message [s] along with the list of available
 * languages and then returns the shortened form of the language which the
 * user enters. *)
let rec prompt_user_lang message = try (
  disp [Bold; blue] message;
  List.iter (fun x -> disp [Bold; green] (x ^ "\n")) available_grammars;
  print_newline ();
  let lang = String.lowercase (String.trim (read_line ())) in
  detect_keywords lang;
  extract_lang lang)
with
  | x -> keyword_handler x prompt_user_lang message

(* [prompt_user_line s] displays message [s] and then returns the line which
 * the user enters. *)
let rec prompt_user_line message = try (
  disp [Bold; blue] message;
  let line = String.trim (read_line ()) in
  detect_keywords line;
  line)
with
  | x -> keyword_handler x prompt_user_line message

(* [prompt_user_yn s] displays message [s] and then returns either "y" or "n"
 * for [Yes] or [No]. *)
let rec prompt_user_yn message = try (
  disp [Bold; blue] message;
  let yn = String.lowercase (String.trim (read_line ())) in
  detect_keywords yn;
  extract_yn yn)
with
  | x -> keyword_handler x prompt_user_yn message

(* [run_again x] executes [runner ()] again if [x] is "y" and exits if [x] is
 * "n". If [x] is neither, [run_again] executes [handle_errors ()]. *)
let rec run_again yn =
  match yn with
    | "y" -> runner ()
    | "n" -> exit 0
    | x -> handle_errors ()

(* [exception_run_again k] manipulates exception [k] into a string [s] in order
 * to execute [run_again s]. *)
and exception_run_again k =
  let s =
    match k with
      | QuittingEarly -> "n"
      | Restart -> "y"
      | _ -> raise k in
  run_again s

(* [runner ()] is the main function that executes the MGT program.
 * To run this function, type [make] into your terminal and type [make clean]
 * once finished using it. The output of running [make] will direct the user as
 * to how to use the MGT. The flow of the function is the following:
 * (1) Display an introductory message and a set of instructions for keywords
 * users might want to take note of.
 * (2) Prompt the user to select languages to translate from and to.
 * (3) Prompt the user to enter a string to translate.
 * (4) Display the possible translation(s) for the user's input.
 * (5) Ask the user whether they would like to save the translation and save
 * if their answer is [Yes].
 * (6) Ask the user whether they would like to step through the function again
 * and exit if their answer is [No], or jump back to step (1) if their answer
 * is [Yes]. 
 * The current list of keywords and their functionality include:
 * -- "Exit" or "Quit" or "End" to end the program without having to wait for
 * step (6).
 * -- "Restart" to jump back from whichever step the user is at to step (1).
 * -- "Words" to display a list of all the words that the MGT accepts.
 * -- "Words <Language>" to display a list of all the words that the MGT
 * accepts in <Language>.
 * -- "History" to view all saved translations for the current session of the
 * MGT.
 * -- "Help" to view the list of keywords and their functionality again. *)

and runner () = try (
  (* update_database (); *)
  disp [Bold;magenta] "\n\nWelcome to the Multilingual Grammatical Translator!";
  disp_instructions ();
  let lang_from = prompt_user_lang
    "Please type the name of one of the following languages to translate from:\n\n"
  in
  let lang_to = prompt_user_lang
    "\n\nPlease type the name of one of the following languages to translate to:\n\n"
  in
  let string_from =
    prompt_user_line "\n\nPlease enter a string to be translated:\n\n" in
  let string_to = run_translator lang_from lang_to string_from in
  let has_no_result = (List.length string_to = 0) in
  if has_no_result then
    disp [Bold; blue]
      "\n\nThere are no possible translations for your choices.\n\n"
  else
    disp [Bold; blue]
      "\n\nThese are the possible translations for your choices:\n\n";
  disp_results string_to;
  print_newline ();
  let _ = if (not has_no_result) then
            let yn = prompt_user_yn
              "\nWould you like to save this translation?\n\n" in
              add_to_history yn lang_from lang_to string_from string_to
          else () in
  let yn = prompt_user_yn "\nWould you like to use the MGT again?\n\n" in
  run_again yn
) with | k -> exception_run_again k

and handle_errors () = try (
  disp [Bold; blue]
    "\n\nSomething went wrong this time. Would you like to start over? Please type ";
  disp [Bold; green] "(Y)es ";
  disp [Bold; blue] "or ";
  disp [Bold; green] "(N)o";
  disp [Bold; blue] ".\n\n";
  let yn = prompt_user_yn "" in
  run_again yn) with | k -> exception_run_again k

let () = try (runner ()) with
  | k -> exception_run_again k