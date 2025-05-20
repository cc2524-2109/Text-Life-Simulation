(* test_Final_Project.ml *)

open OUnit2
open Activities_test
open Character_test
open Event_test
open Option_test
open Game_test

let suite =
  "Final Project Test Suite"
  >::: [
         "Activities Tests" >::: Activities_test.suite;
         "Character Tests" >::: Character_test.suite;
         "Option Tests" >::: Option_test.suite;
         "Event Tests" >::: Event_test.suite;
         "Game Tests" >::: Game_test.suite;
       ]

let () = run_test_tt_main suite
