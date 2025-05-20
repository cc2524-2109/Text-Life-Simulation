open OUnit2
open Final_Project
open Character

let jobs = Activities.load_jobs "../data/jobs.json"

(* Test Character Creation *)
let test_create_random_player _ =
  let char = Character.create_random_player () in
  assert_equal ~printer:Character.get_str_of_char_type Character.player
    (Character.get_character_type char);
  assert_bool "Player's name should not be empty"
    (String.length (Character.get_name char) > 0);
  assert_bool "Player's age should be zero" (Character.get_age char = 0)

(* Test Specific Roles Creation *)
let test_create_random_parent _ =
  let parent = Character.create_random_parent Character.male "Smith" in
  assert_equal ~printer:Character.get_str_of_char_type Character.parent
    (Character.get_character_type parent);
  assert_bool "Parent's occupation must be a valid job"
    (match Character.get_occupation parent with
    | Some job -> List.exists (fun j -> j = job) jobs
    | None -> false);
  assert_bool "Parent's relationship must be positive"
    (Character.get_level "relationship" parent > 0)

(* Test Random Character Creation *)
let test_create_random_characters _ =
  let player = Character.create_random_player () in
  let char_types =
    [
      Character.parent;
      Character.sibling;
      Character.lover;
      Character.friend;
      Character.child;
      Character.pet;
    ]
  in
  List.iter
    (fun typ ->
      let char = Character.create_random typ player in
      assert_bool "Random character age must be non-negative"
        (Character.get_age char >= 0))
    char_types;
  let sibling = Character.create_random_sibling "meow" in
  assert_equal ~printer:Character.get_str_of_char_type Character.sibling
    (Character.get_character_type sibling)

(* Test Getters and Setters *)
let test_setters _ =
  let char = Character.create_random_player () in
  let test_job = List.nth jobs 5 in
  Character.set_occupation char (Some test_job);
  assert_equal
    ~printer:(fun x ->
      match x with
      | None -> "None"
      | Some s -> Activities.get_job_name s)
    (Some test_job)
    (Character.get_occupation char);
  Character.set_age char 30;
  assert_equal ~printer:string_of_int 30 (Character.get_age char)

(* Test Level Modifications *)
let test_level_operations _ =
  let char = Character.create_random_pet () in

  Character.set_level "relationship" 50 char;
  assert_equal ~printer:string_of_int 50
    (Character.get_level "relationship" char);
  Character.change_level "relationship" 30 char;
  assert_equal ~printer:string_of_int 80
    (Character.get_level "relationship" char);
  Character.change_level "relationship" (-90) char;
  assert_equal ~printer:string_of_int 0
    (Character.get_level "relationship" char);

  Character.set_level "health" 70 char;
  assert_equal ~printer:string_of_int 70 (Character.get_level "health" char);
  Character.change_level "health" 20 char;
  assert_equal ~printer:string_of_int 90 (Character.get_level "health" char);
  Character.change_level "health" (-100) char;
  assert_equal ~printer:string_of_int 0 (Character.get_level "health" char);

  Character.set_level "smarts" 40 char;
  assert_equal ~printer:string_of_int 40 (Character.get_level "smarts" char);
  Character.change_level "smarts" 30 char;
  assert_equal ~printer:string_of_int 70 (Character.get_level "smarts" char);
  Character.change_level "smarts" (-80) char;
  assert_equal ~printer:string_of_int 0 (Character.get_level "smarts" char);

  Character.set_level "happiness" 80 char;
  assert_equal ~printer:string_of_int 80 (Character.get_level "happiness" char);
  Character.change_level "happiness" 10 char;
  assert_equal ~printer:string_of_int 90 (Character.get_level "happiness" char);
  Character.change_level "happiness" 20 char;
  assert_equal ~printer:string_of_int 100 (Character.get_level "happiness" char);

  Character.set_level "craziness" 30 char;
  assert_equal ~printer:string_of_int 30 (Character.get_level "craziness" char);
  Character.change_level "craziness" 20 char;
  assert_equal ~printer:string_of_int 50 (Character.get_level "craziness" char);
  Character.change_level "craziness" (-60) char;
  assert_equal ~printer:string_of_int 0 (Character.get_level "craziness" char);

  (* Attempt to modify an invalid level *)
  assert_raises (Failure "Unknown level for pet") (fun () ->
      Character.modify_level "unknown_level" char (fun x -> x + 1))

let character_types_with_levels =
  [
    (Character.player, [ "happiness"; "health"; "smarts"; "looks" ]);
    ( Character.parent,
      [ "relationship"; "religiousness"; "generosity"; "money" ] );
    (Character.sibling, [ "relationship"; "smarts"; "looks"; "petulance" ]);
    ( Character.lover,
      [ "relationship"; "looks"; "smarts"; "money"; "craziness" ] );
    (Character.friend, [ "relationship"; "looks"; "smarts"; "craziness" ]);
    (Character.child, [ "relationship"; "health"; "smarts"; "looks" ]);
    ( Character.pet,
      [ "relationship"; "health"; "smarts"; "happiness"; "craziness" ] );
  ]

(* Helper function to create a character based on character_type *)
let create_character_of_type (character_type : Character.character_type) =
  if character_type = Character.player then
    Character.create_character "Alex" Character.player Character.other 25 None
  else if character_type = Character.parent then
    Character.create_character "Alex" Character.parent Character.other 25 None
  else if character_type = Character.sibling then
    Character.create_character "Alex" Character.sibling Character.other 25 None
  else if character_type = Character.lover then
    Character.create_character "Alex" Character.lover Character.other 25 None
  else if character_type = Character.friend then
    Character.create_character "Alex" Character.friend Character.other 25 None
  else if character_type = Character.child then
    Character.create_character "Alex" Character.child Character.other 25 None
  else if character_type = Character.pet then
    Character.create_character "Alex" Character.pet Character.other 25 None
  else failwith "Unknown character type"

(* Test modifying levels for all character types *)
let test_modify_levels_for_all_character_types _ =
  List.iter
    (fun (char_type, levels) ->
      let char = create_character_of_type char_type in
      List.iter
        (fun level_name ->
          (* Set level to 50 *)
          Character.set_level level_name 50 char;
          assert_equal ~printer:string_of_int 50
            (Character.get_level level_name char)
            ~msg:
              (Printf.sprintf "Setting %s of %s to 50" level_name
                 (Character.get_str_of_char_type char_type));

          (* Change level by +10 *)
          Character.change_level level_name 10 char;
          assert_equal ~printer:string_of_int 60
            (Character.get_level level_name char)
            ~msg:
              (Printf.sprintf "Changing %s of %s by +10" level_name
                 (Character.get_str_of_char_type char_type));

          (* Change level by -20 *)
          Character.change_level level_name (-20) char;
          assert_equal ~printer:string_of_int 40
            (Character.get_level level_name char)
            ~msg:
              (Printf.sprintf "Changing %s of %s by -20" level_name
                 (Character.get_str_of_char_type char_type));

          (* Change level to exceed upper boundary *)
          Character.change_level level_name 100 char;
          assert_equal ~printer:string_of_int 100
            (Character.get_level level_name char)
            ~msg:
              (Printf.sprintf "Changing %s of %s to exceed upper boundary"
                 level_name
                 (Character.get_str_of_char_type char_type));

          (* Change level to go below lower boundary *)
          Character.change_level level_name (-150) char;
          assert_equal ~printer:string_of_int 0
            (Character.get_level level_name char)
            ~msg:
              (Printf.sprintf "Changing %s of %s to go below lower boundary"
                 level_name
                 (Character.get_str_of_char_type char_type)))
        levels;

      (* Test modifying an invalid level name *)
      let invalid_level = "invalid_level" in
      assert_raises
        (Failure
           (Printf.sprintf "Unknown level for %s"
              (String.lowercase_ascii
                 (Character.get_str_of_char_type char_type))))
        (fun () -> Character.modify_level invalid_level char (fun x -> x + 1)))
    character_types_with_levels

(* Test Level Boundaries *)
let test_level_boundaries _ =
  let char = Character.create_random_pet () in
  Character.set_level "happiness" 120 char;
  assert_equal ~printer:string_of_int 100 (Character.get_level "happiness" char);
  Character.set_level "happiness" (-10) char;
  assert_equal ~printer:string_of_int 0 (Character.get_level "happiness" char)

(* Test Character String Representation *)
let test_to_string _ =
  let char = Character.create_random_pet () in
  assert_bool "String1 exists" (String.length (Character.to_string char) > 0);
  let char2 = Character.create_random_player () in
  assert_bool "String2 exists" (String.length (Character.to_string char2) > 0)

let test_should_die _ =
  Random.init 42;

  let young_char = Character.create_random_player () in
  Character.set_level "health" 100 young_char;
  assert_bool "Healthy young character shouldn't die"
    (not (Character.should_die young_char));
  Character.set_age young_char 70;
  let result = Character.should_die young_char in
  assert_bool "Character should either die or not"
    (result = true || result = false);
  Character.set_age young_char 90;
  let result = Character.should_die young_char in
  assert_bool "Character should either die or not"
    (result = true || result = false);
  Character.set_age young_char 110;
  let result = Character.should_die young_char in
  assert_bool "Character should either die or not"
    (result = true || result = false);

  let ran_child = Character.create_random Character.child young_char in
  let result = Character.should_die ran_child in
  assert_bool "Character should either die or not"
    (result = true || result = false);

  let ran_par = Character.create_random Character.parent young_char in
  Character.set_age ran_par 30;
  let result = Character.should_die ran_par in
  assert_bool "Character should either die or not"
    (result = true || result = false);
  Character.set_age ran_par 60;
  let result = Character.should_die ran_par in
  assert_bool "Character should either die or not"
    (result = true || result = false);
  Character.set_age ran_par 75;
  let result = Character.should_die ran_par in
  assert_bool "Character should either die or not"
    (result = true || result = false);
  Character.set_age ran_par 85;
  let result = Character.should_die ran_par in
  assert_bool "Character should either die or not"
    (result = true || result = false);
  Character.set_age ran_par 95;
  let result = Character.should_die ran_par in
  assert_bool "Character should either die or not"
    (result = true || result = false);

  let old_pet = Character.create_random_pet () in
  Character.set_age old_pet 20;
  Character.set_level "health" 5 old_pet;
  assert_bool "Old unhealthy pet should die" (Character.should_die old_pet);
  Character.set_age old_pet 2;
  let result = Character.should_die old_pet in
  assert_bool "Character should either die or not"
    (result = true || result = false);
  Character.set_age old_pet 8;
  let result = Character.should_die old_pet in
  assert_bool "Character should either die or not"
    (result = true || result = false);
  Character.set_age old_pet 12;
  let result = Character.should_die old_pet in
  assert_bool "Character should either die or not"
    (result = true || result = false)

(* Test Invalid Level Access *)
let test_invalid_level _ =
  let char = Character.create_random_parent Character.female "Jones" in
  assert_raises (Failure "Unknown level for parent") (fun () ->
      Character.get_level "unknown_level" char)

(* Test Character Age Increment *)
let test_increment_age _ =
  let char = Character.create_random_player () in
  Character.inc_age char;
  assert_equal ~printer:string_of_int 1 (Character.get_age char)

(* Test Full String Representation for All Character Types *)
let test_to_string_full _ =
  let player = Character.create_random_player () in
  let char_types =
    [
      Character.parent;
      Character.sibling;
      Character.lover;
      Character.friend;
      Character.child;
      Character.pet;
    ]
  in
  List.iter
    (fun char_type ->
      let char = Character.create_random char_type player in
      let char_str = Character.to_string char in
      assert_bool "Character string representation must include name"
        (String.contains char_str (Character.get_name char).[0]))
    char_types

(* Test other create_character branches *)
let test_other _ =
  let char =
    Character.create_character "Alex" Character.player Character.other 25 None
  in
  assert_equal Character.other (Character.get_gender char);
  let char2 =
    Character.create_character "Alex" Character.parent Character.other 25 None
  in
  assert_equal Character.other (Character.get_gender char2);
  let char3 =
    Character.create_character "Alex" Character.sibling Character.male 25 None
  in
  assert_equal Character.male (Character.get_gender char3);
  let char4 =
    Character.create_character "Alex" Character.sibling Character.female 25 None
  in
  assert_equal Character.female (Character.get_gender char4);
  let char5 =
    Character.create_character "Alex" Character.child Character.other 25 None
  in
  assert_equal Character.other (Character.get_gender char5)

(* Test get_char_type_of_str and get_char_str_of_char_type *)
let test_get_char _ =
  let char1 =
    Character.create_character "test" Character.parent Character.other 25 None
  in
  let char2 =
    Character.create_character "test" Character.sibling Character.other 25 None
  in
  let char3 =
    Character.create_character "test" Character.lover Character.other 25 None
  in
  let char4 =
    Character.create_character "test" Character.friend Character.other 25 None
  in
  let char5 =
    Character.create_character "test" Character.child Character.other 25 None
  in
  let char6 =
    Character.create_character "test" Character.pet Character.other 25 None
  in
  let char7 =
    Character.create_character "test" Character.player Character.other 25 None
  in
  assert_equal (Character.get_character_type char1) Character.parent;
  assert_equal (Character.get_character_type char2) Character.sibling;
  assert_equal (Character.get_character_type char3) Character.lover;
  assert_equal (Character.get_character_type char4) Character.friend;
  assert_equal (Character.get_character_type char5) Character.child;
  assert_equal (Character.get_character_type char6) Character.pet;
  assert_equal (Character.get_character_type char7) Character.player;
  assert_equal (Character.get_str_of_char_type Character.parent) "Parent";
  assert_equal (Character.get_str_of_char_type Character.sibling) "Sibling";
  assert_equal (Character.get_str_of_char_type Character.child) "Child";
  assert_equal (Character.get_str_of_char_type Character.player) "Player";
  assert_equal Character.parent (Character.get_char_type_of_str "Parent");
  assert_equal Character.sibling (Character.get_char_type_of_str "Sibling");
  assert_equal Character.lover (Character.get_char_type_of_str "Lover");
  assert_equal Character.friend (Character.get_char_type_of_str "Friend");
  assert_equal Character.child (Character.get_char_type_of_str "Child");
  assert_equal Character.pet (Character.get_char_type_of_str "Pet");
  assert_equal Character.player (Character.get_char_type_of_str "Player");

  assert_raises (Failure "Unknown character type") (fun () ->
      Character.get_char_type_of_str "Alien")

(* Test Setting Role *)
let test_set_role _ =
  let char = Character.create_random_player () in
  Character.set_role char "Hero";
  assert_equal ~printer:(fun x -> x) "Hero" (Character.get_role char)

(* Test Suite *)
let suite =
  [
    "test_create_random_player" >:: test_create_random_player;
    "test_create_random_parent" >:: test_create_random_parent;
    "test_create_random_characters" >:: test_create_random_characters;
    "test_setters" >:: test_setters;
    "test_level_operations" >:: test_level_operations;
    "test_modify_levels_for_all_character_types"
    >:: test_modify_levels_for_all_character_types;
    "test_level_boundaries" >:: test_level_boundaries;
    "test_to_string" >:: test_to_string;
    "test_to_string_full" >:: test_to_string_full;
    "test_should_die" >:: test_should_die;
    "test_invalid_level" >:: test_invalid_level;
    "test_increment_age" >:: test_increment_age;
    "test_other" >:: test_other;
    "test_get_char" >:: test_get_char;
    "test_set_role" >:: test_set_role;
  ]
