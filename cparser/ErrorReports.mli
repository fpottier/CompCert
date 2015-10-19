(* This module is in charge of reporting a syntax error after the pre_parser
   has failed. *)

open Pre_parser.MenhirInterpreter

(* The parser keeps track of the last two tokens in a two-place buffer. *)

type 'a buffer =
| Zero
| One of 'a
| Two of 'a * (* most recent: *) 'a

(* [push buffer x] pushes [x] into [buffer], causing the buffer to slide. *)

val update: 'a buffer -> 'a -> 'a buffer

(* [report text buffer checkpoint] constructs an error message. The C source
   code must be stored in the string [text]. The start and end positions of the
   last two tokens that were read must be stored in [buffer]. The parser state
   (i.e., the automaton's state and stack) must be recorded in the checkpoints
   [inputneeded] and [checkpoint], as produced by [loop_handle_undo]. *)

val report:
  string ->
  (Lexing.position * Lexing.position) buffer ->
  _ checkpoint ->
  _ checkpoint ->
  string

