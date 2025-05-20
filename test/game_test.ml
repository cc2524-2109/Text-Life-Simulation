open OUnit2
open Final_Project

(* Test that a new game correctly initializes a player *)
let test_random_create _ =
  let game = Game.random_create () in
  assert_bool "Game should initialize with a valid player"
    (String.length (Game.get_player game |> Character.get_name) > 0)

(* Test the age-up functionality and its impact on player age *)
let test_age_up _ =
  let game = Game.random_create () in
  let initial_age = Character.get_age (Game.get_player game) in
  Game.age_up game (fun _ -> ());
  let new_age = Character.get_age (Game.get_player game) in
  assert_equal ~printer:string_of_int (initial_age + 1) new_age

(* Test adding a new character to the game *)
let test_add_character _ =
  let game = Game.random_create () in
  let new_friend =
    Character.create_random Character.friend (Game.get_player game)
  in
  Game.add_character game new_friend;
  assert_bool "Friend should be added to game"
    (List.exists
       (fun f -> Character.get_name f = Character.get_name new_friend)
       (Game.get_friends game))

(* Test applying an option that affects character stats *)
let test_apply_option _ =
  let game = Game.random_create () in
  let player = Game.get_player game in
  Character.set_level "health" 0 player;
  let option =
    Option.create ~id:"opt_1" ~description:"Gain Health"
      ~character_effects:[ ("Player", [ ("health", 20) ]) ]
      ~npc_added:[]
  in
  Game.apply_option game option [];
  assert_equal ~printer:string_of_int 20 (Character.get_level "health" player)

(* Test the financial activities system *)
let test_finance_activity _ =
  let game = Game.random_create () in
  Game.change_finance game 100;
  let initial_balance = Game.get_bank game in
  let _ = Game.finance_activity game "Stocks" in
  assert_bool "Finance activity should change bank balance"
    (Game.get_bank game <> initial_balance)

(* Test that characters can die as expected *)
let test_is_dead _ =
  let game = Game.random_create () in
  let dead = Character.create_random Character.pet (Game.get_player game) in
  Game.add_character game dead;
  Game.mark_as_dead dead;
  assert_bool "Dead char should be dead" (Game.is_dead_char dead)

(* Test NPC generation from events *)
let test_gen_npc_list _ =
  let game = Game.random_create () in
  Game.add_character game
    (Character.create_random Character.parent (Game.get_player game));
  Game.add_character game
    (Character.create_random Character.friend (Game.get_player game));
  Game.add_character game
    (Character.create_random Character.pet (Game.get_player game));
  let event =
    Event.create_event "test_event" "A Test Event"
      [ "Parent"; "Friend"; "New Pet" ]
      []
  in
  let npc_list = Game.gen_npc_list game event in
  List.iter
    (fun npc_id ->
      assert_bool
        ("Expected NPC " ^ npc_id ^ " not found")
        (List.exists (fun (id, _) -> id = npc_id) npc_list))
    (Event.get_npc_needed event)

(* Test game ending summary *)
let test_end_life _ =
  let game = Game.random_create () in
  Character.set_level "health" 0 (Game.get_player game);
  let summary = Game.end_life game in
  assert_bool "Summary should mention player death"
    (String.contains summary (Character.get_name (Game.get_player game)).[0])

(* Test marriage proposal functionality *)
let test_marriage _ =
  let game = Game.random_create () in
  let lover = Character.create_random Character.lover (Game.get_player game) in
  Character.set_level "relationship" 100 lover;
  let result = Game.marriage game lover 10 in
  assert_equal
    ~printer:(fun x -> x)
    "Great! They agreed to your marriage proposal!" result

(* Test job assignment and employment check *)
let test_employment_status _ =
  let game = Game.random_create () in
  assert_bool "Player should be unemployed initially"
    (not (Game.is_employed game));
  Character.set_occupation (Game.get_player game)
    (Activities.generate_random_job ());
  Game.set_employed game;
  assert_bool "Player should be employed after job assignment"
    (Game.is_employed game)

(* Test that events are generated based on player age *)
let test_gen_event _ =
  let game = Game.random_create () in
  Character.set_age (Game.get_player game) 5;
  let event_opt, npc_list = Game.gen_event game in
  match event_opt with
  | Some _ -> assert_bool "An event should be generated" true
  | None -> assert_failure "No event was generated"

(* Test friend to lover relationship transition *)
let test_friend_to_lover _ =
  let game = Game.random_create () in
  let friend =
    Character.create_random Character.friend (Game.get_player game)
  in
  Game.add_character game friend;
  Character.set_level "relationship" 100 friend;
  let result = Game.friend_to_lover game friend 10 in
  assert_bool "Result should be either acceptance or rejection message"
    (result = "Great! They agreed to go out with you!"
    || result = "Oh no! They rejected your confession!")

let test_relation_selection _ =
  let game = Game.random_create () in

  let parent =
    Character.create_random Character.parent (Game.get_player game)
  in
  let sibling =
    Character.create_random Character.sibling (Game.get_player game)
  in
  let lover = Character.create_random Character.lover (Game.get_player game) in
  let friend =
    Character.create_random Character.friend (Game.get_player game)
  in
  let child = Character.create_random Character.child (Game.get_player game) in
  let pet = Character.create_random Character.pet (Game.get_player game) in

  Game.add_character game parent;
  Game.add_character game sibling;
  Game.add_character game lover;
  Game.add_character game friend;
  Game.add_character game child;
  Game.add_character game pet;

  let names, characters = Game.relation_selection game "Parent" in
  assert_bool "List length not empty"
    ((not (List.is_empty names)) || not (List.is_empty characters));
  let names, characters = Game.relation_selection game "Sibling" in
  assert_bool "List length not empty"
    ((not (List.is_empty names)) || not (List.is_empty characters));
  let names, characters = Game.relation_selection game "Lover" in
  assert_bool "List length not empty"
    ((not (List.is_empty names)) || not (List.is_empty characters));
  let names, characters = Game.relation_selection game "Friend" in
  assert_bool "List length not empty"
    ((not (List.is_empty names)) || not (List.is_empty characters));
  let names, characters = Game.relation_selection game "Child" in
  assert_bool "List length not empty"
    ((not (List.is_empty names)) || not (List.is_empty characters));
  let names, characters = Game.relation_selection game "Pet" in
  assert_bool "List length not empty"
    ((not (List.is_empty names)) || not (List.is_empty characters));

  (* Check invalid relation type *)
  assert_raises (Failure "Invalid Relationship Type") (fun () ->
      let _ = Game.relation_selection game "Unknown" in
      ())

let test_valid_activity _ =
  let game = Game.random_create () in

  (* Helper function to test activity validation *)
  let check_activity age bank category_name expected_result =
    Character.set_age (Game.get_player game) age;
    Game.change_finance game bank;
    let category = Activities.create_category ~name:category_name ~options:[] in
    let result = Game.valid_activity game category in
    assert_equal ~printer:string_of_bool
      ~msg:
        ("Expected " ^ category_name ^ " to be "
        ^ string_of_bool expected_result)
      expected_result result
  in

  (* Check all valid categories with various ages and bank balances *)
  check_activity 5 0 "Health" false;
  check_activity 11 0 "Health" true;

  check_activity 5 0 "Mind & Body" false;
  check_activity 12 0 "Mind & Body" true;

  check_activity 4 0 "School" false;
  check_activity 18 0 "School" true;
  check_activity 25 0 "School" false;

  check_activity 15 0 "Romance" false;
  check_activity 17 0 "Romance" true;

  check_activity 15 0 "Finance" false;
  check_activity 17 0 "Finance" false;
  check_activity 18 100 "Finance" true;

  check_activity 17 0 "Career" false;
  check_activity 18 0 "Career" true;

  check_activity 18 30 "Entertainment" true;
  check_activity 18 51 "Entertainment" true;

  (* Check invalid category *)
  assert_raises (Failure "Invalid Category") (fun () ->
      let invalid_cat =
        Activities.create_category ~name:"Unknown" ~options:[]
      in
      ignore (Game.valid_activity game invalid_cat))

let test_set_and_get_activities _ =
  let game = Game.random_create () in
  (* Assign sample activities to game *)
  Game.set_activities game;
  assert_equal
    ~printer:(fun _ -> "[]")
    ~msg:"Game should initialize with no activities" []
    (Game.get_activities game);

  (* Test with valid age and bank balance *)
  Character.set_age (Game.get_player game) 18;
  Game.change_finance game 100;
  Game.set_activities game;
  let selected_activities =
    List.map Activities.get_category_name (Game.get_activities game)
  in
  let expected_activities =
    [
      "Health";
      "Mind & Body";
      "School";
      "Romance";
      "Finance";
      "Career";
      "Entertainment";
    ]
  in
  assert_equal
    ~printer:(fun x -> String.concat ", " x)
    ~msg:"Expected all activities to be available" expected_activities
    selected_activities

let test_misc _ =
  let game = Game.random_create () in
  assert_bool "not married" (not (Game.is_married game));
  assert_bool "yep" (String.length (Game.to_string game) > 0);
  let cusstom = Game.custom_create "1" "2" Character.female "NY" [] [] [] in
  assert_bool "check proper init" (Game.get_bank cusstom = 0)

let suite =
  [
    "test_random_create" >:: test_random_create;
    "test_age_up" >:: test_age_up;
    "test_add_character" >:: test_add_character;
    "test_apply_option" >:: test_apply_option;
    "test_finance_activity" >:: test_finance_activity;
    "test_is_dead" >:: test_is_dead;
    "test_gen_npc_list" >:: test_gen_npc_list;
    "test_end_life" >:: test_end_life;
    "test_marriage" >:: test_marriage;
    "test_employment_status" >:: test_employment_status;
    "test_gen_event" >:: test_gen_event;
    "test_friend_to_lover" >:: test_friend_to_lover;
    "test_relation_selection" >:: test_relation_selection;
    "test_valid_activity" >:: test_valid_activity;
    "test_set_and_get_activities" >:: test_set_and_get_activities;
    "test_misc" >:: test_misc;
  ]
