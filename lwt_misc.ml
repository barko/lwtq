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

exception Timeout 

let rec retry f max_attempts timeout attempts_remaining last_exn_opt =
  Lwt.choose [

    Lwt_unix.sleep timeout >> return `Timeout; 

    try_lwt
      let attempt_count = 1 + max_attempts - attempts_remaining in
      f attempt_count last_exn_opt >>= fun result -> return (`Result result)
    with exn ->
      return (`Exn exn)

  ] >>= function

    | `Timeout ->

      if attempts_remaining = 0 then
        fail Timeout
      else
        retry f max_attempts timeout (attempts_remaining-1) last_exn_opt
          
    | `Result result -> return result 

    | `Exn exn -> 

      (* got an exception, so after sleeping a bit, retry; ideally, we
         only sleep [timeout] minus the time it took [f] to fail, but
         this would complicate the implementation *)

      Lwt_unix.sleep timeout >>
        retry f max_attempts timeout (attempts_remaining-1) (Some exn)

let retry f attempts_remaining timeout =
  retry f attempts_remaining timeout attempts_remaining None


let sleeping threads = 
  List.filter (
    fun thr -> 
      (Lwt.state thr) = Sleep
  ) threads

let max_parallel next_thread max =
  let rec loop n threads = 
    if n < max () then
      let threads' = (next_thread ()) :: threads in
      loop (n+1) threads'
    else (
      lwt chosen = Lwt.choose threads in
      let threads' = (next_thread ()) :: (sleeping threads) in
      loop (n+1) threads'
    )
  in
  loop 0 []

let qmap ~in_q ~out_q f =
  lwt in_el = Lwt_queue.take in_q in
  lwt out_el = f in_el in
  Lwt_queue.put out_q out_el 

let flow_qmap ~in_q ~out_q f max =
  max_parallel (fun () -> qmap ~in_q ~out_q f) max

let rec array_iter_i_p f a i =
  if i = Array.length a then
    return ()
  else
    let t = f i a.(i) and lt = array_iter_i_p f a (i+1) in
    lwt () = t in
    lt

let array_iter_i_p f a =
  array_iter_i_p f a 0
