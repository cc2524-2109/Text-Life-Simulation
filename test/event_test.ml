open OUnit2
open Final_Project

let test_load_event _ =
  let test_events = Event.load_events "data/adult_events.json" in
  assert_bool "list is non-empty" (not (List.is_empty test_events))

let test_create_event _ =
  let event =
    Event.create_event "event_1" "Test Description" [ "npc_1" ]
      [
        Option.create ~id:"opt_1" ~description:"Test Option 1"
          ~character_effects:[ ("npc_1", [ ("health", 10) ]) ]
          ~npc_added:[ "npc_2" ];
      ]
  in
  assert_equal ~printer:(fun x -> x) "event_1" (Event.get_id event);
  assert_equal
    ~printer:(fun x -> x)
    "Test Description"
    (Event.get_description event []);
  assert_equal ~printer:(String.concat ", ") [ "npc_1" ]
    (Event.get_npc_needed event);
  assert_equal ~printer:string_of_int 1 (List.length (Event.get_options event))

let test_get_id _ =
  let event = Event.create_event "event_2" "Another Test" [] [] in
  assert_equal ~printer:(fun x -> x) "event_2" (Event.get_id event)

let test_get_description _ =
  let npc =
    Character.create_character "John" Character.player Character.male 25 None
  in
  let event = Event.create_event "event_3" "Hello [npc_1]!" [ "npc_1" ] [] in
  let result = Event.get_description event [ ("npc_1", npc) ] in
  assert_equal ~printer:(fun x -> x) "Hello John!" result

let test_get_options _ =
  let option1 =
    Option.create ~id:"opt_1" ~description:"Test Option 1"
      ~character_effects:[ ("npc_1", [ ("health", 10) ]) ]
      ~npc_added:[]
  in
  let option2 =
    Option.create ~id:"opt_2" ~description:"Test Option 2"
      ~character_effects:[ ("npc_1", [ ("smarts", 5) ]) ]
      ~npc_added:[]
  in
  let event =
    Event.create_event "event_4" "Options Test" [] [ option1; option2 ]
  in
  let options = Event.get_options event in
  assert_equal ~printer:string_of_int 2 (List.length options);
  assert_equal ~printer:(fun x -> x) "opt_1" (Option.get_id (List.hd options))

let test_get_npc_needed _ =
  let event = Event.create_event "event_5" "NPC Test" [ "npc_1"; "npc_2" ] [] in
  let npcs = Event.get_npc_needed event in
  assert_equal ~printer:(String.concat ", ") [ "npc_1"; "npc_2" ] npcs

let suite =
  [
    "test_load_event" >:: test_load_event;
    "test_create_event" >:: test_create_event;
    "test_get_id" >:: test_get_id;
    "test_get_description" >:: test_get_description;
    "test_get_options" >:: test_get_options;
    "test_get_npc_needed" >:: test_get_npc_needed;
  ]
