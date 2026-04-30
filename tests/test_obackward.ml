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
   the wait status and captured output. *)
let run_helper mode =
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
  let captured = read_all_from_fd stderr_r in
  Unix.close stderr_r;
  let _, status = Unix.waitpid [] pid in
  (status, captured)

let assert_died_with_trace ~mode =
  let status, captured = run_helper mode in
  (match status with
  | Unix.WSIGNALED _ -> ()
  | Unix.WEXITED code ->
      Alcotest.failf
        "helper (%s) exited normally with code %d (signal handler did not \
         re-raise);\n\
         captured stderr was:\n\
         %s"
        mode code captured
  | Unix.WSTOPPED s ->
      Alcotest.failf
        "helper (%s) stopped with signal %d; captured stderr was:\n%s" mode s
        captured);
  let needle = "Stack trace (most recent call last)" in
  if not (contains captured needle) then
    Alcotest.failf
      "expected stderr (mode=%s) to contain %S; got:\n%s" mode needle captured

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
