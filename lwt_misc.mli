(** Miscellaneous Lwt functions *)

(* Copyright (c) 2011, barko 00336ea19fcb53de187740c490f764f4 All
   rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:
   
   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

   3. Neither the name of barko nor the names of contributors may be used
   to endorse or promote products derived from this software without
   specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

val retry : (unit -> 'a Lwt.t) -> int -> float -> 
  [> `Ok of 'a | `Exn of exn | `Timeout ] list Lwt.t
(* [retry f num_attempts tmo] calls [f] at most [num_attempts]
   times, waiting for no more than [tmo] between each attempt *)


val max_parallel : (unit -> 'a Lwt.t) -> (unit -> int) -> 'b Lwt.t
(* [max_parallel f max] returns a thread which concurrently runs at
   most [max ()] threads generated by [f ()] in parallel *)

val qmap : 
  in_q:('a Lwt_queue.t) -> 
  out_q:('b Lwt_queue.t) -> 
  ('a -> 'b Lwt.t) -> 
  unit Lwt.t
(* [qmap ~in_q ~out_q f] returns a thread which applies mapping
   function [f] to the elements of the queue [in_q] and puts then in
   the queue [out_q] *)

val flow_qmap :
  in_q:('a Lwt_queue.t) -> 
  out_q:('b Lwt_queue.t) -> 
  ('a -> 'b Lwt.t) -> 
  (unit -> int) ->
  unit Lwt.t
(* [flow_qmap ~in_q ~out_q f max] returns a thread which concurrently
   runs at most [max ()] threads; each of these threads applies
   mapping [f ()] to elements of [in_q], and stores the results in the queue
   [out_q]. *)

val array_iter_i_p : (int -> 'a -> unit Lwt.t) -> 'a array -> unit Lwt.t
(* like Array.iteri, but for a function returning [unit Lwt.t] *)

val batchify : 
  inq:'a option Lwt_queue.t -> 
  outq:'a list option Lwt_queue.t -> 
  int -> 
  int Lwt.t
(* [batchify ~inq ~outq size] takes elements from [inq] until [size]
   have been taken, then puts a list of that size in [outq].  The
   elements appear in [outq] in the same order as they did in [inq].
   [batchify] returns when it sees a [None] element in [inq], after
   first placing whatever elements have been collected from it in
   [outq].  [batchify] returns the number of elements batched. *)
