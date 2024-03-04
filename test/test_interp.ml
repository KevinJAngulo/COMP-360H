(* Testing framework for Imp.
 *
 * N. Danner
 *
 * Tests are defined by a combination of specifications and Imp source code
 * files.
 *
 * A test specification is a JSON object with the following attributes:
 *
 *   - "file":   name of an Imp source code file (string)
 *   - "input":  a string list of values to provide as input.
 *   - "output": a string list of expected output values.
 *
 * The test is conducted as follows.  The source code file is executed by the
 * interpreter.  Each `get_*` function is given the next item in the input
 * list.  Every `print_*` output is recorded into an output file.  The lines of
 * the output file are compared to the output list.  If they are the same, the
 * test passes; otherwise the test fails.  So, for example, consider
 *
 *   {
 *       "file":     "gcd.imp",
 *       "input":    [ "84", "60" ],
 *       "output":   [ "12" ]
 *   }
 *
 * This test will execute `gcd.imp`.  The first call to `get_*` will get "84"
 * and the second call will get "60".  The test passes if the program outputs a
 * single line that consists of "12", fails otherwise.
 *
 * A specification file is a sequence of specifications.  One test suite is
 * defined for each specification file, and is named for that file.  Individual
 * tests within a suite are named 0, 1, 2,... according to their index in the
 * specification file.
 *
 * Tests are grouped by teams.  Each team has a directory under
 * `interp_tests_dir`.  Each team directory has subdirectories `specs_dir`
 * (for test specifications) and `files_dir` (for the program files).
 *
 *)

open OUnit2

module YJ = Yojson.Basic
module YJU = YJ.Util

(* Directory in which to find test specifications.
 *)
let specs_dir = "specs"

(* Directory in which to find test code.
 *)
let files_dir = "files"

(* Directory in which to find team test directories.
 *)
let interp_tests_dir = "teams"

(* make_test_from_json_spec fd sp = tf, where tf is a a test defined by sp as
 * described above.  `fd` is the directory in which to find program files.
 *
 * Note that what I call a "test" has type `OUnit2.test_fun`.
 *)
let make_test_from_json_spec 
    (files_dir : string) (spec : YJ.t) : test_fun = fun tc ->
  (* Get the code to be tested.
   *)
  let test_file = 
    Filename.concat files_dir (spec |> YJU.member "file" |> YJU.to_string) in
  let test_code = In_channel.with_open_text test_file (
    fun ic ->
      let lexbuf = Lexing.from_channel ic in
      Imp.Parser.terminated_pgm Imp.Lexer.read_token lexbuf
  ) in

  (* Set the API input channel to read from the input in the test
   * specification.
   *)
  let input : string list = 
    spec |> YJU.member "input" |> YJU.to_list |> YJU.filter_string in
  let () = 
    Imp.Interp.Api.in_channel := 
      Scanf.Scanning.from_string (String.concat " " input) in

  (* Get the expected output.
   *)
  let expected : string list =
    spec |> YJU.member "output" |> YJU.to_list |> YJU.filter_string in

  (* Execute the test code, sending output to a file, then reading that output
   * back into `actual`.  This would be so much nicer if I could just set the
   * API output channel to a buffer!
   *)
  let (tmpfile, oc) = bracket_tmpfile tc in
  let () =
    Imp.Interp.Api.out_channel := oc in
  let () = Imp.Interp.Api.show_prompts := false in
  let () = Imp.Interp.exec test_code in
  let () = Out_channel.close oc in
  let actual : string list =
    In_channel.with_open_text tmpfile In_channel.input_lines in

  assert_equal ~printer:(String.concat " ") expected actual

(* make_suite_from_specs sd fd f = ts, where ts is a test suite with name
 * `f`, where the tests in ts are specified by the specs in `sd`/`f` and
 * program files are found in `fd`.
 *
 * Note that what I call a "test suite" has type OUnit2.test.
 *)
let make_suite_from_specs 
    (specs_dir : string) (files_dir : string) (f : string) : test =
  let specs = Filename.concat specs_dir f |> YJ.seq_from_file |> List.of_seq in

  f >::: 
    List.mapi 
      (fun i s -> Int.to_string i >:: make_test_from_json_spec files_dir s) 
      specs

let () =
  let show_and_run s =
    match s with
    | OUnitTest.TestLabel (name, _) ->
      print_endline @@ "=====" ^ name  ^ "=====" ;
      run_test_tt_main s ;
      print_endline ""
    | _ ->
      run_test_tt_main s
  in

  let team_tests d =
    print_endline "========================================" ;
    print_endline d ;
    print_endline "========================================" ;
    print_endline "" ;
    let team_specs_dir = 
      Filename.concat (Filename.concat interp_tests_dir d) specs_dir in
    let team_files_dir =
      Filename.concat (Filename.concat interp_tests_dir d) files_dir in
    let spec_files = 
      Sys.readdir team_specs_dir |> Array.to_list in
    let suites = List.map (make_suite_from_specs team_specs_dir team_files_dir) spec_files in
    List.iter show_and_run suites
  in

  let team_dirs : string list =
    Sys.readdir interp_tests_dir |> Array.to_list in

  List.iter team_tests team_dirs

