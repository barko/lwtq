let num_takers = 2 
let num_adders = 10
let initial_max_queue_length = 5
let max_queue_length_increment = 3
let max_delay = 1. (* applies to both takers and adders *)
let incr_max_queue_length_delay = 2.

open Lwt
module LQ = Lwt_queue

let counter () =
  let c = ref 0 in
  fun () ->
    let i = !c in
    incr c;
    i

let fixed_delay () = Lwt_unix.sleep max_delay
let random_delay () = Lwt_unix.sleep (Random.float max_delay)

let delay =
  match Sys.argv with
    | [| _ |] | [| _ ; "r" |] -> random_delay
    | _ -> fixed_delay 

let c = counter () 
let q = LQ.create ~max_length:initial_max_queue_length ()

let elements () =
  List.rev (
    LQ.fold (
      fun accu el ->
        el :: accu
    ) [] q
  )

let string_of_elements () =
  String.concat "," (List.map string_of_int (elements ()))

let pr action id v =
  let n = LQ.length q in
  let n_read = LQ.num_readers q in
  let n_write = LQ.num_writers q in
  Printf.printf "%s id=%s v=%d len=%d nr=%d nw=%d elements=%s\n%!" 
    action
    id v 
    n n_read n_write
    (string_of_elements ())

let rec add id =
  let v = c () in
  lwt () = LQ.add q v in
  pr "add" id v;
  lwt () = delay () in
  add id


let rec take id =
  lwt v = LQ.take q in
  pr "take" id v;
  lwt () = delay () in
  take id
  
let rec incr_max_queue_length () =
  lwt () = Lwt_unix.sleep incr_max_queue_length_delay in
  let new_max_length = max_queue_length_increment + (LQ.get_max_length q) in
  Printf.printf "changing max queue size to %d\n%!" new_max_length;
  LQ.set_max_length q new_max_length;
  incr_max_queue_length ()

let _ =
  let takers = Array.init num_takers 
    (fun i -> take ("T" ^ (string_of_int i))) in
  let adders = Array.init num_adders 
    (fun i -> add ("A" ^ (string_of_int i))) in

  let threads = (incr_max_queue_length ()) :: 
    ((Array.to_list takers) @ (Array.to_list adders))
  in
  Lwt_unix.run (Lwt.join threads)
