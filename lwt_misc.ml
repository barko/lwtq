open Lwt

let rec retry f timeout attempts_remaining outcome_accu =
  let tick = Unix.gettimeofday () in
  Lwt.choose [

    Lwt_unix.sleep timeout >> return `Timeout; 

    try_lwt
      f () >>= fun result -> return (`Ok result)
    with exn ->
      return (`Exn exn)

  ] >>= function

    | `Timeout ->

        let outcome_accu' = `Timeout :: outcome_accu in
        if attempts_remaining <= 0 then
          return outcome_accu'
        else
          retry f timeout (attempts_remaining-1) outcome_accu'
            
    | `Ok result -> 
        let outcome_accu' = (`Ok result) :: outcome_accu in
        return outcome_accu'

    | `Exn exn -> 

        let outcome_accu' = (`Exn exn) :: outcome_accu in

        if attempts_remaining <= 0 then
          return outcome_accu'
        else (
          let tock = Unix.gettimeofday () in
          let time_to_fail = tock -. tick in
          let time_remaining = max 0. (timeout -. time_to_fail) in
          
          Lwt_unix.sleep time_remaining >>
            retry f timeout (attempts_remaining-1) outcome_accu'
        )


let retry f max_attempts timeout =
  retry f timeout (max_attempts-1) []


let sleeping threads = 
  List.filter (
    fun thr -> 
      (Lwt.state thr) = Sleep
  ) threads

let rec repeat n f accu =
  if n = 0 then
    accu
  else
    repeat (n-1) f ((f ()) :: accu)

let max_parallel next_thread max =
  let rec loop finished = function
    | [] when finished -> return ()
    | threads -> 
        if finished then 
          lwt _ = Lwt.nchoose threads in
          let sleeping_threads = sleeping threads in
          loop finished sleeping_threads 
        else
          if (List.length threads) < max () then
            (* add another thread *)
            let threads' = (next_thread ()) :: threads in
            loop finished threads'
          else (
            (* wait for one or more threads to finish *)
            lwt should_continue_list = Lwt.nchoose threads in
            let n_chosen, should_continue = List.fold_left (
              fun (n_chosen, should_continue) sc -> 
                n_chosen + 1, should_continue && sc
            ) (0, true) should_continue_list in
            let finished = not should_continue in

            let threads' = repeat n_chosen next_thread (sleeping threads) in
            loop finished threads'
          )
  in
  loop false []

let qmap ~in_q ~out_q f =
  lwt in_el = Lwt_queue.take in_q in
  lwt out_el = f in_el in
  Lwt_queue.put out_q out_el

let flow_qmap ~in_q ~out_q f max =
  max_parallel (fun () -> qmap ~in_q ~out_q f >> return true) max

let rec array_iter_i_p f a i =
  if i = Array.length a then
    return ()
  else
    let t = f i a.(i) and lt = array_iter_i_p f a (i+1) in
    lwt () = t in
    lt

let array_iter_i_p f a =
  array_iter_i_p f a 0

let rec batchify ~inq ~outq batch_size num_elements (accu_len, accu) =
  if accu_len = batch_size then
    lwt () = Lwt_queue.put outq (Some (List.rev accu)) in
    batchify ~inq ~outq batch_size num_elements (0, [])
  else (
    assert (0 <= accu_len && accu_len < batch_size);
    lwt e_opt = Lwt_queue.take inq in
    match e_opt with 
      | None -> 
          (* terminate the iteration with a [None] *)
          lwt () = Lwt_queue.put outq (Some (List.rev accu)) in
          lwt () = Lwt_queue.put outq None in
          return num_elements 
      | Some e ->
          batchify ~inq ~outq batch_size (num_elements+1) (accu_len+1, e :: accu)
  )

let batchify ~inq ~outq batch_size =
  if batch_size < 1 then
    fail (Invalid_argument "batchify batch size is < 1")
  else
    batchify ~inq ~outq batch_size 0 (0,[])


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

