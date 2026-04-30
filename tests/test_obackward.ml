let read_all_from_fd fd =
  let buf = Buffer.create 4096 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buf chunk 0 n;
        loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ();
  Buffer.contents buf

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec loop i =
    if i + m > n then false
    else if String.sub s i m = sub then true
    else loop (i + 1)
  in
  loop 0

(* Spawn the crash_helper executable in [mode], capture its stderr, and return
   the wait status, captured output, and whether a watchdog had to SIGKILL the
   helper. The watchdog matters because backward-cpp's signal handler is
   supposed to re-raise the original signal after printing, but on some
   environments (observed on GitHub macOS runners) the helper prints the trace
   and then hangs, which would otherwise deadlock [Unix.read] / [Unix.waitpid]
   forever. *)
let run_helper ?(timeout_seconds = 15) mode =
  let helper =
    Filename.concat (Filename.dirname Sys.executable_name) "crash_helper.exe"
  in
  if not (Sys.file_exists helper) then
    Alcotest.failf "crash_helper.exe not found at %s" helper;
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let pid =
    Unix.create_process helper [| helper; mode |] devnull devnull stderr_w
  in
  Unix.close stderr_w;
  Unix.close devnull;
  let killed_by_watchdog = ref false in
  let prev_handler =
    Sys.signal Sys.sigalrm
      (Sys.Signal_handle
         (fun _ ->
           killed_by_watchdog := true;
           try Unix.kill pid Sys.sigkill with _ -> ()))
  in
  ignore (Unix.alarm timeout_seconds);
  let captured = read_all_from_fd stderr_r in
  Unix.close stderr_r;
  ignore (Unix.alarm 0);
  Sys.set_signal Sys.sigalrm prev_handler;
  let _, status = Unix.waitpid [] pid in
  (status, captured, !killed_by_watchdog)

let assert_died_with_trace ~mode =
  let status, captured, killed_by_watchdog = run_helper mode in
  let needle = "Stack trace (most recent call last)" in
  if not (contains captured needle) then
    Alcotest.failf
      "expected stderr (mode=%s) to contain %S;\n\
       status=%s, watchdog_killed=%b;\n\
       got:\n\
       %s"
      mode needle
      (match status with
      | Unix.WSIGNALED s -> Printf.sprintf "WSIGNALED %d" s
      | Unix.WEXITED c -> Printf.sprintf "WEXITED %d" c
      | Unix.WSTOPPED s -> Printf.sprintf "WSTOPPED %d" s)
      killed_by_watchdog captured

let test_register_returns_ok () =
  match Backward.register () with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "expected Ok (), got Error %S" msg

let test_signal_triggers_backtrace () = assert_died_with_trace ~mode:"sigusr1"

let test_obj_magic_triggers_backtrace () = assert_died_with_trace ~mode:"magic"

let test_ffi_null_deref_triggers_backtrace () =
  assert_died_with_trace ~mode:"ffi"

let () =
  Alcotest.run "obackward"
    [
      ( "register",
        [
          Alcotest.test_case "returns Ok" `Quick test_register_returns_ok;
        ] );
      ( "signal_handler",
        [
          Alcotest.test_case "SIGUSR1 prints stack trace" `Quick
            test_signal_triggers_backtrace;
          Alcotest.test_case "Obj.magic deref prints stack trace" `Quick
            test_obj_magic_triggers_backtrace;
          Alcotest.test_case "C FFI null deref prints stack trace" `Quick
            test_ffi_null_deref_triggers_backtrace;
        ] );
    ]
