(* .mli file for the module UI which fulfills several user-interface purposes.
 * This file contains the code necessary to help the runner of the MGT 
 * function smoothly by formatting output and providing solutions to incorrect
 * inputs. *)

open Database
open Parser
open Translator
open ANSITerminal

(* [QuittingEarly] is the exception thrown when the user indicates through their
 * input that they wish to exit the MGT early. *)
exception QuittingEarly

(* [Restart] is the exception thrown when the user indicates through their input
 * that they wish to restart the MGT. *)
exception Restart

(* [WordList lst] is the exception thrown when the user indicates through their
 * input that they wish to display a certain amount of word lists. If [lst] is
 * the empty list ([]), then all available word lists are to be displayed.
 * If [lst] contains one element [x], then the word list for [x] is to be
 * displayed. *)
exception WordList of string list

(* [NoInput] is the exception thrown when the user hits the enter key without
 * entering any input. *)
exception NoInput

(* [Help] is the exception thrown when the user indicates through their input
 * that they wish to view the instructions for using the MGT. *)
exception Help

(* [History] is the exception thrown when the user indicates through their input
 * that they wish to view their translation history. *)
exception History

(* [InvalidLang] is the exception thrown when the user is meant to select from
 * a list of languages but selects an option not in the list. *)
exception InvalidLang

(* [InvalidYN] is the exception thrown when the user is meant to select from
 * [Yes] or [No] but selects something else. *)
exception InvalidYN

(* [disp args s] is [ANSITerminal.print_string args s]. *)
val disp : style list -> string -> unit

(* A [HistoryEntry] is the type that the user creates when they store a
 * translation. A [HistoryEntry] keeps track of the language that was
 * translated from, the language that was translated to, the string that was
 * translated from, and the string(s) that were translated to. *)
module HistoryEntry : sig
  type t

  (* [create_history_entry lf lt sf st] creates a HistoryEntry list [[h]] where
   * [h] is the HistoryEntry, [lf] is the language that was translated from, 
   * [lt] is the language that was translated to, [sf] is the string that was
   * translated from, and [st] is the string(s) that were translated to. *)
  val create_history_entry : string -> string -> string -> string -> t list

  (* [print h] formats and prints a history entry. *)
  val print : t -> unit
end

(* [history] is a list of HistoryEntries that is updated whenever the user 
 * saves a translation. *)
val history : HistoryEntry.t list ref


(* [detect_keywords s] parses through [s] to match it with keywords that
 * map to an exception and either raises the appropriate exception or does
 * nothing. *)
val detect_keywords : string -> unit

(* [extract_lang s] parses through [s] to decide whether it is a valid language
 * and if it is invalid, it raises [InvalidLang], but if it is valid, it returns
 * the string representing the shortened form of the valid language. *)
val extract_lang : string -> string

(* [extract_lang s] parses through [s] to decide whether it is a [Yes] or [No]
 * and if it is invalid, it raises [InvalidYN], but if it is valid, it returns
 * either the string [y] or [n]. *)
val extract_yn : string -> string

(* [disp_no_input ()] displays the message appropriate for when a user has
 * failed to provide an input. *)
val disp_no_input : unit -> unit

(* [disp_invalid_lang ()] displays the message appropriate for when a user has
 * failed to provide a valid language. *)
val disp_invalid_lang : unit -> unit

(* [disp_invalid_yn ()] displays the message appropriate for when a user has
 * failed to provide a [Yes] or [No]. *)
val disp_invalid_yn : unit -> unit

(* [disp_instructions ()] displays the instructions about keywords while using
 * the MGT. *)
val disp_instructions : unit -> unit

(* [run_translator lf lt sf] picks the appropriate translation function to
 * use on the string [sf] based on the language from ([lf]) and language to
 * ([lt]) and then runs that function. *)
val run_translator : string -> string -> string -> string list

(* [add_to_history lf lt sf st] adds the translation from language [lf] to
 * language [lt] of string [sf] which results in [st] to the history. *)
val add_to_history : string -> string -> string -> string -> string list -> unit

(* [disp_words lst] displays a certain amount of word lists. If [lst] is the
 * empty list ([]), then all of the available word lists are displayed. If
 * [lst] contains one element [x], then the word list for [x] is displayed. *)
val disp_words : string list -> unit

(* [disp_history ()] displays the current history and displays a message if the
 * history is empty. *)
val disp_history : unit -> unit

(* [disp_results lst] formats and displays the result of a translation
 * function [lst]. *)
val disp_results : string list -> unit