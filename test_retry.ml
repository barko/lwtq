open Lwt

let string_of_out = function 
  | `Ok _ -> "ok"
  | `Timeout -> "timeout"
  | `Exn _ -> "exn"

let t () = 
  lwt attempts = Lwt_misc.retry (
    fun () ->
      lwt () = Lwt_unix.sleep (Random.float 10.) in
      if Random.bool () then
        fail (Failure "boo")
      else
        return ()
  ) max_int 1.0 in
  List.iter (fun outcome -> print_endline (string_of_out outcome)) attempts;
  return ()

let _ =
  Lwt_unix.run (t ())

               
      
  
