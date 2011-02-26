open Lwt
open Lwt_misc

let sleep = Lwt_unix.sleep 

let results_rev = ref []
let add v = results_rev := v :: !results_rev

let test () =
  array_iter_i_p (
    fun i s ->
      lwt () = Lwt_unix.sleep s in
      Printf.printf "%d slept %f\n%!" i s;
      add i;
      return ()
  ) [| 3.; 1.; 5.; 2.; 7. |]


let _ =
  Lwt_unix.run (test ());
  assert (List.rev !results_rev = [1;3;0;2;4]);
  print_endline "test passed"
  
