open OUnit2
open Yojson.Basic
open Yojson.Basic.Util
open Final_Project

(* Import the module to test *)

let string_list_printer lst = "[" ^ String.concat ", " lst ^ "]"

let character_effects_printer effects =
  let effect_to_string (char, stats) =
    let stats_str =
      List.map (fun (stat, value) -> Printf.sprintf "(%s, %d)" stat value) stats
      |> String.concat ", "
    in
    Printf.sprintf "(%s, [%s])" char stats_str
  in
  "[" ^ String.concat ", " (List.map effect_to_string effects) ^ "]"

let test_create _ =
  let id = "opt1" in
  let description = "This is a test option" in
  let character_effects = [ ("char1", [ ("stat1", 10); ("stat2", -5) ]) ] in
  let npc_added = [ "npc1" ] in
  let option = Option.create ~id ~description ~character_effects ~npc_added in
  assert_equal ~printer:(fun x -> x) id (Option.get_id option);
  assert_equal ~printer:(fun x -> x) description (Option.get_description option);
  assert_equal ~printer:character_effects_printer character_effects
    (Option.get_character_effects option);
  assert_equal ~printer:string_list_printer npc_added
    (Option.get_npc_added option)

let test_getters _ =
  let id = "opt2" in
  let description = "Another test option" in
  let character_effects = [ ("char2", [ ("stat3", 20) ]) ] in
  let npc_added = [ "npc2" ] in
  let option = Option.create ~id ~description ~character_effects ~npc_added in
  assert_equal ~printer:(fun x -> x) "opt2" (Option.get_id option);
  assert_equal
    ~printer:(fun x -> x)
    "Another test option"
    (Option.get_description option);
  assert_equal ~printer:character_effects_printer
    [ ("char2", [ ("stat3", 20) ]) ]
    (Option.get_character_effects option);
  assert_equal ~printer:string_list_printer [ "npc2" ]
    (Option.get_npc_added option)

let test_setters _ =
  let id = "opt3" in
  let description = "Option before update" in
  let character_effects = [] in
  let npc_added = [] in
  let option = Option.create ~id ~description ~character_effects ~npc_added in
  let updated_option = Option.set_id option "new_id" in
  let updated_option =
    Option.set_description updated_option "Updated description"
  in
  let updated_option =
    Option.set_character_effects updated_option [ ("char3", [ ("stat4", 15) ]) ]
  in
  let updated_option = Option.set_npc_added updated_option [ "npc3" ] in
  assert_equal ~printer:(fun x -> x) "new_id" (Option.get_id updated_option);
  assert_equal
    ~printer:(fun x -> x)
    "Updated description"
    (Option.get_description updated_option);
  assert_equal ~printer:character_effects_printer
    [ ("char3", [ ("stat4", 15) ]) ]
    (Option.get_character_effects updated_option);
  assert_equal ~printer:string_list_printer [ "npc3" ]
    (Option.get_npc_added updated_option)

let test_from_json _ =
  let json_string =
    "{\"id\": \"opt4\",\"description\": \"JSON test \
     option\",\"character_effects\": [{\"character\": \"char4\", \"effects\": \
     [{\"stat\": \"stat5\", \"value\": 30}]}],\"npc_added\": [\"npc4\"]}"
  in
  let json = Yojson.Basic.from_string json_string in
  let option = Option.from_json json in
  assert_equal ~printer:(fun x -> x) "opt4" (Option.get_id option);
  assert_equal
    ~printer:(fun x -> x)
    "JSON test option"
    (Option.get_description option);
  assert_equal ~printer:character_effects_printer
    [ ("char4", [ ("stat5", 30) ]) ]
    (Option.get_character_effects option);
  assert_equal ~printer:string_list_printer [ "npc4" ]
    (Option.get_npc_added option)

let suite =
  [
    "test_create" >:: test_create;
    "test_getters" >:: test_getters;
    "test_setters" >:: test_setters;
    "test_from_json" >:: test_from_json;
  ]
