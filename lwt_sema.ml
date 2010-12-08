(** semaphore, based on Lwt *)

(* Copyright (c) 2010, barko 00336ea19fcb53de187740c490f764f4 All
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

open Lwt

type t = {
  mutable value : int;
  (* underlying queue *)

  mutable max_value : int ;
  (* upper bound on the value *)

  mutable waiters : unit Lwt.u Lwt_sequence.t;
  (* threads waiting for [value > 0]  *)

}

let num_waiters t =
  Lwt_sequence.fold_l (fun _ c -> c + 1) t.waiters 0 

let value t = 
  t.value

let get_max_value t =
  t.max_value

let create ?(max_value=max_int) () = { 
  (* by default, create a semaphore that is effectively unbounded, and
     whose initial value is [max_value] *)
  value = max_value;
  max_value = max_value;
  waiters = Lwt_sequence.create ();
}

let add t =
  if t.value < t.max_value then
    match Lwt_sequence.take_opt_l t.waiters with
      | None -> t.value <- t.value + 1
      | Some w -> Lwt.wakeup w ()

        
let sub t =
  if t.value = 0 then
    let (res, w) = Lwt.task () in
    let node = Lwt_sequence.add_r w t.waiters in
    Lwt.on_cancel res (fun _ -> Lwt_sequence.remove node);
    res
  else (
    t.value <- t.value - 1;
    return ()
  )


let set_max_value t new_max_value =
  if new_max_value < 1 then
    raise (Invalid_argument "Lwt_sema.set_max_value")
  else
    t.max_value <- new_max_value





