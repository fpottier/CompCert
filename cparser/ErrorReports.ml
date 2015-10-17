open Lexing
open Pre_parser.MenhirInterpreter

(* -------------------------------------------------------------------------- *)

(* The parser keeps track of the last two tokens in a two-place buffer. *)

type 'a buffer =
| Zero
| One of 'a
| Two of 'a * (* most recent: *) 'a

(* [push buffer x] pushes [x] into [buffer], causing the buffer to slide. *)

let update buffer x : _ buffer =
  match buffer, x with
  | Zero, _ ->
      One x
  | One x1, x2
  | Two (_, x1), x2 ->
      Two (x1, x2)

(* [show f buffer] prints the contents of the buffer. The function [f] is
   used to print an element. *)

let show f buffer : string =
  match buffer with
  | Zero ->
      (* The buffer cannot be empty. If we have read no tokens, we
         cannot have detected a syntax error. *)
      assert false
  | One invalid ->
      (* It is unlikely, but possible, that we have read just one token. *)
      Printf.sprintf "before '%s'" (f invalid)
  | Two (valid, invalid) ->
      (* In the most likely case, we have read two tokens. *)
      Printf.sprintf "after '%s' and before '%s'" (f valid) (f invalid)

(* [last buffer] returns the last element of the buffer (that is, the
   invalid token). *)

let last buffer =
  match buffer with
  | Zero ->
      (* The buffer cannot be empty. If we have read no tokens, we
         cannot have detected a syntax error. *)
      assert false
  | One invalid
  | Two (_, invalid) ->
      invalid

(* -------------------------------------------------------------------------- *)

(* [extract text (pos1, pos2)] extracts the sub-string of [text] delimited
   by the positions [pos1] and [pos2]. *)

let extract text (pos1, pos2) : string =
  let ofs1 = pos1.pos_cnum
  and ofs2 = pos2.pos_cnum in
  let len = ofs2 - ofs1 in
  String.sub text ofs1 len

(* -------------------------------------------------------------------------- *)

(* [state checkpoint] extracts the number of the current state out of a
   pre_parser checkpoint. *)

let state checkpoint : int =
  match checkpoint with
  | HandlingError env ->
      let module G = MenhirLib.General in
      begin match Lazy.force (stack env) with
      | G.Nil ->
          (* Hmm... The parser is in its initial state. Its number is
             usually 0. This is a BIG HACK. TEMPORARY *)
          0
      | G.Cons (Element (s, _, _, _), _) ->
          number s
      end
  | _ ->
      assert false (* this cannot happen, I promise *)

(* -------------------------------------------------------------------------- *)

(* [report text buffer checkpoint] constructs an error message. The C source
   code must be stored in the string [text]. The start and end positions of the
   last two tokens that were read must be stored in [buffer]. The parser state
   (i.e., the automaton's state and stack) must be recorded in [checkpoint]. *)

(* The start and end positions of the invalid token are [lexbuf.lex_start_p]
   and [lexbuf.lex_curr_p], since this is the last token that was read. But
   we need not care about that here. *)

let report text buffer checkpoint : string =
  (* Extract the position where the error occurred, that is, the start
     position of the invalid token. We display it as a filename, a  (1-based)
     line number, and a (0-based) column number. *)
  let (pos, _) = last buffer in
  (* Construct a readable description of where the error occurred, that is,
     after which token and before which token. *)
  let where = show (extract text) buffer in
  (* Find out in which state the parser failed. *)
  let s : int = state checkpoint in
  (* Choose an error message, based on the state number [s]. *)
  let msg = try
    Pre_parser_messages.message s
  with Not_found ->
    (* If the state number cannot be found -- which, in principle,
       should not happen, since our list of erroneous states is
       supposed to be complete! -- produce a generic message. *)
    Printf.sprintf "This is an unknown syntax error (%d).\n\
                    Please report this problem to the compiler vendor.\n" s
  in
  (* Construct the full error message. *)
  Printf.sprintf "%s:%d:%d: syntax error %s.\n%s"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    where
    msg

