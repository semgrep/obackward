(* [@@inline never] keeps the bad load from being constant-folded and
   ensures it shows up as its own frame in the captured stack trace. *)
let[@inline never] bad_obj_magic () =
  let arr : int array = Obj.magic 0 in
  print_int arr.(0)

external ffi_null_deref : int -> int = "test_crash_ffi_null_deref"

let[@inline never] bad_ffi () = ignore (ffi_null_deref 0)

let () =
  match Backward.register () with
  | Error msg ->
      prerr_endline msg;
      exit 2
  | Ok () ->
      let mode = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sigusr1" in
      (match mode with
      | "sigusr1" -> Unix.kill (Unix.getpid ()) Sys.sigusr1
      | "magic" -> bad_obj_magic ()
      | "ffi" -> bad_ffi ()
      | other ->
          prerr_endline ("unknown mode: " ^ other);
          exit 2);
      (* The signal handler in backward-cpp re-raises the signal after
         printing the trace, so we should never actually reach this point. *)
      exit 0
