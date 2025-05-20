open Final_Project

(* Utility Functions *)
let quit () =
  Printf.printf "\nThank you for playing Unit(Life)! Goodbye!\n";
  exit 0

let print_title title =
  let border = String.make (String.length title + 6) '#' in
  Printf.printf "\n%s\n#  %s  #\n%s\n" border title border

let print_border header =
  let lines = String.split_on_char '\n' header in
  let max_length =
    List.fold_left (fun acc line -> max acc (String.length line)) 0 lines
  in
  let border = String.make (max_length + 4) '=' in
  Printf.printf "\n%s\n" border;
  List.iter
    (fun line ->
      Printf.printf "| %s%s |\n" line
        (String.make (max_length - String.length line) ' '))
    lines;
  Printf.printf "%s\n" border

let print_list_with_index lst =
  List.iteri
    (fun index item -> Printf.printf "  [%d] %s\n" (index + 1) item)
    lst

let clear_screen () = Sys.command "clear" |> ignore

(* Centralized Input Handling *)
let rec prompt_choice question options =
  print_endline ("\n" ^ question);
  print_list_with_index options;
  Printf.printf "\nYour choice: ";
  let choice = String.lowercase_ascii (read_line ()) in
  match int_of_string_opt choice with
  | Some index when index > 0 && index <= List.length options -> index
  | _ ->
      print_border "Invalid choice. Please try again.";
      prompt_choice question options

let handle_death char =
  Printf.printf "Your %s %s has died at age %d. May they rest in peace.\n"
    (String.lowercase_ascii
       (Character.get_str_of_char_type (Character.get_character_type char)))
    (Character.get_name char) (Character.get_age char)

(* Game Logic *)
let age_up game =
  Activities.reset_accessed (Game.get_activities game);
  clear_screen ();

  (* init temporary list to track newly dead characters *)
  let newly_dead = ref [] in

  (* update ages and check for deaths *)
  Game.age_up game (fun char -> newly_dead := char :: !newly_dead);
  if !newly_dead <> [] then
    List.iter (fun char -> handle_death char) !newly_dead;

  Game.set_activities game;
  let event_opt, npc_list = Game.gen_event game in
  match event_opt with
  | None -> ()
  | Some event ->
      print_title "An Event Occurs!";
      let event_summary =
        Printf.sprintf "%s\n\n%s" (Event.get_id event)
          (Event.get_description event npc_list)
      in
      print_border event_summary;
      let opt_list = Event.get_options event in
      let choice =
        prompt_choice "What would you like to do?"
          (List.map Option.get_id opt_list)
        - 1
      in
      let opt = List.nth opt_list choice in
      Game.apply_option game opt npc_list;
      print_title "Outcome of Your Decision";
      Printf.printf "\n%s\n" (Option.get_description opt)

let character_select game relation_type delta =
  let relation_names, relation_chars =
    Game.relation_selection game relation_type
  in
  let relation_names = relation_names @ [ "Exit Activities" ] in
  match prompt_choice "Who would you like to select?" relation_names with
  | choice3 when choice3 = List.length relation_names -> ()
  | choice3 -> (
      let selected = List.nth relation_chars (choice3 - 1) in
      match relation_type with
      | "Friend" ->
          Printf.printf "%s\n" (Game.friend_to_lover game selected delta)
      | "Lover" -> Printf.printf "%s\n" (Game.marriage game selected delta)
      | _ -> failwith "May Change in the Future")

let rec browse_lovers game =
  let potential_lover =
    Character.create_random Character.lover (Game.get_player game)
  in
  print_border (Character.to_string potential_lover);
  match
    prompt_choice
      ("Would you like to ask out " ^ Character.get_name potential_lover ^ "?")
      [ "Yes"; "No"; "Exit" ]
  with
  | 1 ->
      if Random.int 2 = 0 then (
        Printf.printf "Congrats! You are now dating %s.\n"
          (Character.get_name potential_lover);
        let gender = Character.get_gender potential_lover in
        let role =
          if gender = Character.female then "Girlfriend"
          else if gender = Character.male then "Boyfriend"
          else ""
        in
        Character.set_role potential_lover role;
        Game.add_character game potential_lover)
      else Printf.printf "Oh no! You got rejected! Better luck next time...\n"
  | 2 ->
      clear_screen ();
      browse_lovers game
  | _ -> ()

let job_select game =
  let jobs = Activities.load_jobs "data/jobs.json" in
  let available_jobs = Activities.select_random_elements 5 jobs in
  let jobs_listing =
    Activities.job_list_to_string_list available_jobs @ [ "Exit Activities" ]
  in
  match prompt_choice "Which job would you like to select?" jobs_listing with
  | choice3 when choice3 = List.length jobs_listing -> false
  | choice3 ->
      let selected = List.nth available_jobs (choice3 - 1) in
      let stat, threshold = Activities.get_stat_threshold (Some selected) in
      if Character.get_level stat (Game.get_player game) >= threshold then (
        Character.set_occupation (Game.get_player game) (Some selected);
        Game.set_employed game;
        Printf.printf "YAY! You got the job!\n";
        true)
      else (
        Printf.printf "Oh no! You didn't meet their requirements!\n";
        false)

let apply_action game category act_name activities character =
  let act_data =
    List.find
      (fun acts -> Activities.get_action_name acts = act_name)
      (Activities.get_category_options category)
  in
  let accessed = Activities.get_action_accessed act_data in
  let stat, delta, accessed_msg, fst_msg =
    Activities.get_action_effects act_data
  in
  if not accessed then (
    print_title act_name;
    Printf.printf "\n%s\n" fst_msg;
    Activities.update_accessed activities category act_name true;
    match stat with
    | "Health" -> Character.change_level "health" delta character
    | "Smarts" -> Character.change_level "smarts" delta character
    | "Happiness" -> Character.change_level "happiness" delta character
    | "Friend" ->
        Game.add_character game
          (Character.create_random Character.friend (Game.get_player game));
        Character.change_level "happiness" delta character;
        Activities.update_accessed activities category act_name false
    | "Lover" ->
        if Game.is_married game then
          Printf.printf "Wait! You are already married. Shame on you!\n"
        else browse_lovers game
    | "Friend_Lover" ->
        if Game.is_married game then
          Printf.printf "Wait! You are already married. Shame on you!\n"
        else character_select game "Friend" delta
    | "Marry" ->
        if Game.is_married game then
          Printf.printf "Wait! You are already married. Shame on you!\n"
        else character_select game "Lover" delta
    | "Stocks" -> Printf.printf "%s\n" (Game.finance_activity game "Stocks")
    | "Lottery" -> Printf.printf "%s\n" (Game.finance_activity game "Lottery")
    | "Get/Change Job" ->
        if not (job_select game) then
          Activities.update_accessed activities category act_name false
    | "Promotion" ->
        if Game.is_employed game then
          let current_job = Character.get_occupation character in
          let stat, threshold = Activities.get_stat_threshold current_job in
          if Character.get_level stat character >= threshold then (
            Activities.incr_threshold current_job 10;
            Activities.incr_salary current_job;
            Character.set_occupation character current_job;
            Game.set_employed game;
            Printf.printf
              "YAY! Your boss acknowledged your abilities and hard work and \
               gave you a promotion!\n")
          else Printf.printf "Oh no! Your promotion request got rejected!\n"
        else Printf.printf "You are currently unemployed!\n"
    | "Quit Job" ->
        if Game.is_employed game then (
          Character.set_occupation character None;
          Game.set_employed game;
          Character.change_level "happiness" delta character)
        else
          Printf.printf
            "You are currently unemployed! There is no job for you to quit!\n"
    | "Relationship" -> Character.change_level "relationship" delta character
    | _ -> failwith "Invalid Selection")
  else (
    print_title act_name;
    Printf.printf "\n%s\n" accessed_msg)

let rec show_activity game =
  clear_screen ();
  print_title "Activities";
  let activities = Game.get_activities game in
  let options =
    List.map (fun categ -> Activities.get_category_name categ) activities
    @ [ "Exit Activities" ]
  in
  match prompt_choice "What would you like to do?" options with
  | choice when choice = List.length options -> () (* Exit option *)
  | choice ->
      let category = List.nth activities (choice - 1) in
      show_actions game activities category

and show_actions game activities category =
  clear_screen ();
  let cat_name = Activities.get_category_name category in
  print_title (cat_name ^ " Activities");
  let actions = Activities.get_actions_name_in_category category in
  let actions = actions @ [ "Exit " ^ cat_name ^ " Activities" ] in
  match prompt_choice "What would you like to do?" actions with
  | choice2 when choice2 = List.length actions -> show_activity game
  | choice2 ->
      let act = List.nth actions (choice2 - 1) in
      apply_action game category act activities (Game.get_player game)

let rec show_relationships game =
  clear_screen ();
  print_title "Relationships";
  let relationships = Game.get_relationships game in
  let options =
    List.map
      (fun char ->
        Character.get_name char ^ " (" ^ Character.get_role char
        ^ ") >>> Relationship: "
        ^ string_of_int (Character.get_level "relationship" char)
        ^ "%")
      relationships
    @ [ "Exit Relationships" ]
  in
  match
    prompt_choice "Pick a character to view more detailed stats." options
  with
  | choice when choice = List.length options -> () (* Exit option *)
  | choice ->
      let char = List.nth relationships (choice - 1) in
      show_character_details game char

and show_character_details game char =
  clear_screen ();
  print_title (Character.get_name char);
  print_border (Character.to_string char);
  let interactions =
    if Character.get_character_type char = Character.pet then
      Activities.load_activity "data/pet_interactions.json"
    else Activities.load_activity "data/interactions.json"
  in
  let options =
    Activities.get_actions_name_in_category (List.hd interactions)
  in
  let options = options @ [ "Back to Relationships"; "Back to Main Game" ] in
  match prompt_choice "What would you like to do?" options with
  | choice2 when choice2 = List.length options -> ()
  | choice2 when choice2 = List.length options - 1 -> show_relationships game
  | choice2 ->
      let interact_name = List.nth options (choice2 - 1) in
      apply_action game (List.hd interactions) interact_name interactions char

let new_game () =
  print_title "Start a New Game";
  let options = [ "Randomize Character"; "Customize Character"; "Quit" ] in
  let choice = prompt_choice "What would you like to do?" options in
  match choice with
  | 1 ->
      print_title "Your Random Character!";
      Game.random_create ()
  | 2 ->
      print_title "Customize Your Character!";
      Printf.printf "\nEnter your character's first name: ";
      let first_name = read_line () in
      Printf.printf "\nEnter your character's last name: ";
      let last_name = read_line () in
      let gender_options = [ "Male"; "Female"; "Other" ] in
      let gender_choice =
        prompt_choice "Enter your character's gender:" gender_options
      in
      let gender =
        match gender_choice with
        | 1 -> Character.male
        | 2 -> Character.female
        | 3 -> Character.other
        | _ -> failwith "Invalid Selection"
      in
      let hometown_options =
        [
          "Hidden Leaf Village";
          "Hidden Sand Village";
          "Hidden Stone Village";
          "Hidden Cloud Village";
          "Create Your Own";
        ]
      in
      let hometown_choice =
        prompt_choice "Select a hometown from the list or create your own:"
          hometown_options
      in
      let hometown =
        match hometown_choice with
        | 1 | 2 | 3 | 4 -> List.nth hometown_options (hometown_choice - 1)
        | 5 ->
            Printf.printf "\nEnter your custom hometown: ";
            read_line ()
        | _ -> failwith "Invalid Selection"
      in
      let family_options =
        [
          "Both Parents";
          "Single Parent (Father)";
          "Single Parent (Mother)";
          "Orphan (Foster Care)";
        ]
      in
      let family_choice =
        prompt_choice "Choose your family background:" family_options
      in
      let parents =
        match family_choice with
        | 1 ->
            [
              Character.create_random_parent Character.male last_name;
              Character.create_random_parent Character.female last_name;
            ]
        | 2 -> [ Character.create_random_parent Character.male last_name ]
        | 3 -> [ Character.create_random_parent Character.female last_name ]
        | 4 -> []
        | _ -> failwith "Invalid Selection"
      in
      let sibling_count =
        prompt_choice "How many siblings would you like? (0-2)"
          [ "0"; "1"; "2" ]
      in
      let siblings =
        match sibling_count with
        | 1 -> []
        | 2 -> [ Character.create_random_sibling last_name ]
        | 3 -> [ Character.create_random_sibling last_name ]
        | _ -> failwith "Invalid Selection"
      in
      let pet_count =
        prompt_choice "Would you like to start with a pet?" [ "No"; "Yes" ]
      in
      let pets =
        match pet_count with
        | 1 -> []
        | 2 -> [ Character.create_random_pet () ]
        | _ -> failwith "Invalid Selection"
      in
      (*Printf.printf "\nEnter starting money (positive integer): "; let rec
        get_starting_money () = try let money = int_of_string (read_line ()) in
        if money >= 0 then money else ( Printf.printf "Please enter a positive
        integer: "; get_starting_money ()) with Failure _ -> Printf.printf
        "Invalid input. Please enter a positive integer: "; get_starting_money
        () in let bank = get_starting_money () in *)
      Game.custom_create first_name last_name gender hometown parents siblings
        pets
  | 3 -> quit ()
  | _ -> failwith "Invalid Selection"

let rec start game =
  let rec game_loop () =
    clear_screen ();
    if Game.is_dead game then begin
      print_title "You have died.";
      print_border (Game.end_life game);
      let choice =
        prompt_choice "Would you like to start a new game?"
          [ "Start New Game"; "Quit" ]
      in
      match choice with
      | 1 -> start (new_game ())
      | _ -> quit ()
    end
    else begin
      print_border ("\n" ^ Game.to_string game ^ "\n");
      let options = [ "Age Up"; "Activities"; "Relationships"; "Quit Game" ] in
      let choice = prompt_choice "What would you like to do?" options in
      match choice with
      | 1 ->
          age_up game;
          game_loop ()
      | 2 ->
          show_activity game;
          game_loop ()
      | 3 ->
          show_relationships game;
          game_loop ()
      | 4 -> quit ()
      | _ -> failwith "Invalid Selection"
    end
  in
  game_loop ()

let () =
  clear_screen ();
  print_endline
    "\n\
    \   _   _       _ _    ___     _  __    __\n\
    \  | | | |_ __ (_) |_ / / |   (_)/ _| __\\ \\\n\
    \  | | | | '_ \\| | __| || |   | | |_ / _ \\ |\n\
    \  | |_| | | | | | |_| || |___| |  _|  __/ |\n\
    \   \\___/|_| |_|_|\\__| ||_____|_|_|  \\___| |\n\
    \                     \\_\\               /_/ ";
  start (new_game ())
