open Lwt
module LS = Lwt_sema

let num_subbers = 1 
let num_adders = 2
let initial_max_value = 3
let max_max_value = 5
let max_delay = 1. (* applies to both takers and adders *)
let modify_max_value_delay = 2.

let fixed_delay () = Lwt_unix.sleep max_delay
let random_delay () = Lwt_unix.sleep (Random.float max_delay)

let delay =
  match Sys.argv with
    | [| _ |] | [| _ ; "r" |] -> 
      Printf.printf "using random delay with maximum %f seconds\n%!" max_delay;
      random_delay
    | _ -> 
      Printf.printf "using fixed delay of %f seconds\n%!" max_delay;      
      fixed_delay 

let s = LS.create ~max_value:initial_max_value ()

let pr action id =
  let v = LS.value s in
  let n_waiters = LS.num_waiters s in
  Printf.printf "%s id=%s v=%d nw=%d\n%!" 
    action
    id v 
    n_waiters

let rec add id =
  LS.add s;
  pr "add" id;
  lwt () = delay () in
  add id


let rec sub id =
  lwt () = LS.sub s in
  pr "sub" id;
  lwt () = delay () in
  sub id
  
let rec modify_max_value () =
  lwt () = Lwt_unix.sleep modify_max_value_delay in
  let new_max_value = 1 + (Random.int (max_max_value-1)) in
  Printf.printf "changing max value to %d\n%!" new_max_value;
  LS.set_max_value s new_max_value;
  modify_max_value ()

let _ =
  let subbers = Array.init num_subbers
    (fun i -> sub ("S" ^ (string_of_int i))) in
  let adders = Array.init num_adders 
    (fun i -> add ("A" ^ (string_of_int i))) in

  let threads = (modify_max_value ()) :: 
    ((Array.to_list subbers) @ (Array.to_list adders))
  in
  Lwt_unix.run (Lwt.join threads)

