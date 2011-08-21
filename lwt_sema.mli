(** semaphore, based on Lwt *)

type t 

val num_waiters : t -> int
  (* return the number of threads waiting  *)

val value : t -> int
  (* the current value of *)

val set_max_value : t -> int -> unit
  (* the maximum value of the semaphore.  Once [value] has reached
     [max_value] calls to [add] will have no effect *)

val get_max_value : t -> int
  (* inspect the current maximum value *)

val create : ?max_value:int -> unit -> t
  (* create a new semaphore, optionally specifying its maximum value.
     The initial value is the maximum value. *)

val add : t -> unit
  (* should [value < max_value], increase the value of the semaphore
     by one *)

val sub : t -> unit Lwt.t
  (* decrease the value by one *)


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

