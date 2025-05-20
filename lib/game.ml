module StringMap = Map.Make (String)

type t = {
  player : Character.t;
  mutable is_married : bool;
  mutable is_employed : bool;
  mutable parents : Character.t list;
  mutable siblings : Character.t list;
  mutable lovers : Character.t list;
  mutable friends : Character.t list;
  mutable children : Character.t list;
  mutable pets : Character.t list;
  mutable bank : int;
  mutable activities : Activities.category list;
  location : string;
}
(** Abstraction Function (AF)
    - A value of type `condition` represents a set of requirements for an event
      to occur.
    - `npc_needed : string list` represents the list of required NPCs for the
      event, described by their roles or types.

    - A value of type `t` represents a game event.
    - `id : string` is the unique identifier for the event.
    - `description : string` is a narrative explanation of what the event
      entails.
    - `conditions : condition` specifies prerequisites for the event, such as
      required NPCs.
    - `options : Option.t list` is the list of possible choices the player can
      make when encountering this event.

    Representation Invariant (RI)
    - `npc_needed` in `condition` must contain only valid NPC role strings, with
      no duplicates.
    - `id` must be a non-empty string and should be unique across all events.
    - `description` must be a non-empty string.
    - `options` must be a non-empty list containing valid `Option.t` values. *)

(* Load functions *)
let infant_events = Event.load_events "data/infant_events.json"
let childhood_events = Event.load_events "data/childhood_events.json"
let teenage_events = Event.load_events "data/teenage_events.json"
let adult_events = Event.load_events "data/adult_events.json"
let elderly_events = Event.load_events "data/elderly_events.json"
let activities = Activities.load_activity "data/activities.json"

(* list to track dead characters *)
let graveyard : Character.t list ref = ref []
let mark_as_dead char = graveyard := char :: !graveyard
let is_dead game = Character.should_die game.player
let is_dead_char char = List.exists (( = ) char) !graveyard

let custom_create fn ln gend ht parents siblings pets =
  {
    player =
      Character.create_character (fn ^ " " ^ ln) Character.player gend 0 None;
    is_married = false;
    is_employed = false;
    parents;
    siblings;
    lovers = [];
    friends = [];
    children = [];
    pets;
    bank = 0 (*change to just bank if implementing money*);
    activities = [];
    location = ht;
  }

let random_create () =
  Random.self_init ();
  let location = "Ithaca, New York" in
  let player = Character.create_random_player () in
  {
    player;
    is_married = false;
    is_employed = false;
    parents =
      (let chance = Random.float 1.0 in
       if chance < 0.6 then
         [
           Character.create_random Character.parent player;
           Character.create_random Character.parent player;
         ]
       else if chance < 0.8 then
         [ Character.create_random Character.parent player ]
       else []);
    siblings =
      (let chance = Random.float 1.0 in
       if chance < 0.3 then
         [
           Character.create_random Character.sibling player;
           Character.create_random Character.sibling player;
           Character.create_random Character.sibling player;
         ]
       else if chance < 0.5 then
         [
           Character.create_random Character.sibling player;
           Character.create_random Character.sibling player;
         ]
       else if chance < 0.7 then
         [ Character.create_random Character.sibling player ]
       else []);
    lovers = [];
    friends = [];
    children = [];
    pets =
      (let chance = Random.float 1.0 in
       if chance < 0.2 then
         [
           Character.create_random Character.pet player;
           Character.create_random Character.pet player;
         ]
       else if chance < 0.5 then
         [ Character.create_random Character.pet player ]
       else []);
    bank = 0;
    activities = [];
    location;
  }

let get_relationships game =
  List.flatten
    [
      game.lovers;
      game.children;
      game.parents;
      game.siblings;
      game.friends;
      game.pets;
    ]

let check_and_mark_deaths game death_handler =
  let check_and_remove_dead characters =
    List.filter
      (fun char ->
        if Character.should_die char && not (List.mem char !graveyard) then (
          mark_as_dead char;
          death_handler char;
          false (* Remove from active list *))
        else true)
      characters
  in
  game.parents <- check_and_remove_dead game.parents;
  game.siblings <- check_and_remove_dead game.siblings;
  game.lovers <- check_and_remove_dead game.lovers;
  game.friends <- check_and_remove_dead game.friends;
  game.children <- check_and_remove_dead game.children;
  game.pets <- check_and_remove_dead game.pets

(* Increment age for a player and all associated characters in the game *)
let age_up game death_handler =
  Character.inc_age game.player;
  (match Character.get_occupation game.player with
  | Some job -> game.bank <- game.bank + Activities.get_job_salary job
  | None -> ());
  List.iter Character.inc_age game.parents;
  List.iter Character.inc_age game.siblings;
  List.iter Character.inc_age game.lovers;
  List.iter Character.inc_age game.friends;
  List.iter Character.inc_age game.children;
  List.iter Character.inc_age game.pets;
  check_and_mark_deaths game death_handler

(** [gen_npc_list game event] generates a mapping of NPC IDs from [event] to
    specific NPC characters from [game]. Each NPC ID is mapped to a randomly
    selected character of the corresponding type. Ensures that selected NPCs are
    removed from the pool of available characters to avoid reuse. Raises:
    [Failure: "Invalid NPC ID format"] if the event contains a NPC ID that does
    not match the expected format. *)
let gen_npc_list game event =
  let npc_needed = Event.get_npc_needed event in
  let get_character_type npc_id =
    match String.split_on_char ' ' npc_id with
    | [ "New"; char_type; _ ] -> (true, char_type)
    | [ "New"; char_type ] -> (true, char_type)
    | [ char_type; _ ] -> (false, char_type)
    | [ char_type ] -> (false, char_type)
    | _ -> failwith "Invalid NPC ID format"
  in
  let available_characters =
    ref
      [
        ("Parent", game.parents);
        ("Sibling", game.siblings);
        ("Lover", game.lovers);
        ("Friend", game.friends);
        ("Child", game.children);
        ("Pet", game.pets);
      ]
  in
  let rec pick_random_and_remove char_type =
    match List.assoc_opt char_type !available_characters with
    | None | Some [] -> None
    | Some char_list ->
        let idx = Random.int (List.length char_list) in
        let selected_char = List.nth char_list idx in
        (* Remove the selected character from the available pool *)
        available_characters :=
          List.map
            (fun (typ, chars) ->
              if typ = char_type then
                (typ, List.filter (( != ) selected_char) chars)
              else (typ, chars))
            !available_characters;
        Some selected_char
  in
  (* Build the mapping of NPC IDs to characters *)
  let npc_mapping =
    List.fold_left
      (fun acc npc_id ->
        let is_new, char_type = get_character_type npc_id in
        if is_new then
          let new_char =
            Character.create_random
              (Character.get_char_type_of_str char_type)
              game.player
          in
          (npc_id, new_char) :: acc
        else
          match pick_random_and_remove char_type with
          | Some character -> (npc_id, character) :: acc
          | None -> acc)
      [] npc_needed
  in
  (* Check if all required NPCs have been mapped *)
  if List.length npc_mapping = List.length npc_needed then npc_mapping
  else failwith "Bad mapping"

let get_player game = game.player
let is_married game = game.is_married
let is_employed game = game.is_employed

let is_student game =
  let occupation = Character.get_occupation game.player in
  let students = Activities.student_occupations () in
  List.mem occupation students

let gen_event game =
  Random.self_init ();
  let age = Character.get_age game.player in
  let age_events =
    if age > 65 then elderly_events
    else if age > 20 then adult_events
    else if age > 12 then teenage_events
    else if age > 5 then childhood_events
    else infant_events
  in
  let event_pool = age_events in
  let event = List.nth event_pool (Random.int (List.length event_pool)) in
  try (Some event, gen_npc_list game event) with Failure _ -> (None, [])

let add_character game char =
  let char_type = Character.get_character_type char in
  if char_type = Character.parent then game.parents <- char :: game.parents
  else if char_type = Character.sibling then
    game.siblings <- char :: game.siblings
  else if char_type = Character.lover then game.lovers <- char :: game.lovers
  else if char_type = Character.friend then game.friends <- char :: game.friends
  else if char_type = Character.child then
    game.children <- char :: game.children
  else if char_type = Character.pet then game.pets <- char :: game.pets
  else failwith "Cannot add new Player"

let apply_option game opt npc_list =
  let apply_levels char lvls =
    List.iter (fun (lvl, amt) -> Character.change_level lvl amt char) lvls
  in
  List.iter
    (fun (char_id, lvls) ->
      match char_id with
      | "Player" -> apply_levels game.player lvls
      | _ ->
          let _, npc = List.find (fun x -> fst x = char_id) npc_list in
          apply_levels npc lvls)
    (Option.get_character_effects opt);

  let apply_add npc_added =
    let _, npc = List.find (fun x -> fst x = "New " ^ npc_added) npc_list in
    add_character game npc
  in
  List.iter apply_add (Option.get_npc_added opt)

let end_life game =
  let player_name = Character.get_name game.player in
  let player_age = Character.get_age game.player in
  let bank_balance = game.bank in

  let filtered characters =
    characters
    |> List.filter (fun char -> Character.get_level "relationship" char > 50)
    |> List.map Character.get_name
  in

  let construct_string single_label plural_label characters =
    match filtered characters with
    | [] -> None
    | [ name ] -> Some (single_label ^ " " ^ name)
    | names -> Some (plural_label ^ " " ^ String.concat ", " names)
  in

  let funeral_attendees =
    [
      construct_string "their parent" "their parents" game.parents;
      construct_string "their sibling" "their siblings" game.parents;
      (if game.is_married && game.lovers <> [] then
         construct_string
           ("their "
           ^ String.lowercase_ascii
               (Character.get_role (List.nth game.lovers 0)))
           "" game.lovers
       else construct_string "their lover" "their lovers" game.lovers);
      construct_string "their friend" "their friends" game.friends;
      construct_string "their child" "their children" game.children;
      construct_string "their pet" "their pets" game.pets;
    ]
    |> List.filter_map Fun.id
  in

  let format_funeral_attendees attendees =
    match attendees with
    | [] -> "no one"
    | [ last ] -> last
    | _ ->
        let all_but_last = List.rev (List.tl (List.rev attendees)) in
        let last = List.hd (List.rev attendees) in
        String.concat ",\n" all_but_last ^ ",\nand " ^ last
  in

  Printf.sprintf
    "\n\
     In loving memory of %s, who passed away at age %d...\n\n\
     %s lived a life that was %s and %s. \n\
     They amassed a total of $%d in their bank account.\n\
     Their funeral was attended by %s. \n\n\
     %s's legacy will be remembered by all who knew them.\n"
    player_name player_age player_name
    (if Character.get_level "health" game.player > 50 then "healthy"
     else "riddled with sickness")
    (if Character.get_level "happiness" game.player > 50 then "filled with joy"
     else "often challenging")
    bank_balance
    (format_funeral_attendees funeral_attendees)
    player_name

let to_string game =
  Character.to_string game.player
  ^ "\n\nLocation: " ^ game.location ^ "\nBank: " ^ string_of_int game.bank
  ^ "\nTotal Relationships: "
  ^ string_of_int (List.length (get_relationships game))

let relations_names relation_lst =
  List.map (fun char -> Character.get_name char) relation_lst

let relation_selection game relation_type =
  match relation_type with
  | "Parent" -> (relations_names game.parents, game.parents)
  | "Sibling" -> (relations_names game.siblings, game.siblings)
  | "Lover" -> (relations_names game.lovers, game.lovers)
  | "Friend" -> (relations_names game.friends, game.friends)
  | "Child" -> (relations_names game.children, game.children)
  | "Pet" -> (relations_names game.pets, game.pets)
  | _ -> failwith "Invalid Relationship Type"

let friend_to_lover game friend delta =
  if
    Character.get_level "relationship" friend
    > Random.int_in_range ~min:60 ~max:100
  then (
    game.friends <- List.filter (fun char -> char <> friend) game.friends;
    let new_lover =
      Character.create_character
        (Character.get_name friend)
        Character.lover
        (Character.get_gender friend)
        (Character.get_age friend)
        (Character.get_occupation friend)
    in
    Character.set_level "relationship"
      (Character.get_level "relationship" friend)
      new_lover;
    Character.set_level "looks" (Character.get_level "looks" friend) new_lover;
    Character.set_level "smarts" (Character.get_level "smarts" friend) new_lover;
    Character.set_level "craziness"
      (Character.get_level "craziness" friend)
      new_lover;
    Random.self_init ();
    Character.set_level "money" (Random.int_in_range ~min:0 ~max:100) new_lover;
    game.lovers <- new_lover :: game.lovers;
    Character.change_level "happiness" delta (get_player game);
    "Great! They agreed to go out with you!")
  else (
    Character.change_level "happiness" (-delta) (get_player game);
    "Oh no! They rejected your confession!")

let marriage game lover delta =
  if
    Character.get_level "relationship" lover
    > Random.int_in_range ~min:60 ~max:100
  then (
    let gender = Character.get_gender lover in
    let role =
      if gender = Character.female then "Wife"
      else if gender = Character.male then "Husband"
      else "Spouse"
    in
    Character.set_role lover role;
    game.lovers <- [ lover ];
    game.is_married <- true;
    Character.change_level "happiness" delta (get_player game);
    "Great! They agreed to your marriage proposal!")
  else (
    Character.change_level "happiness" (-delta) (get_player game);
    "Oh no! They rejected your marriage proposal!")

let change_finance game delta = game.bank <- game.bank + delta

let finance_activity game action =
  Random.self_init ();
  let rand = Random.int 100 + 1 in
  match action with
  | "Stocks" ->
      if rand > 60 then (
        change_finance game 100;
        "Your stock increased in value! Great!")
      else (
        change_finance game (-100);
        "Oh no! Your stock decreased in value!")
  | "Lottery" ->
      if rand > 95 then (
        change_finance game 10000;
        "WOW! YOU WON THE LOTTERY!")
      else "Sadly, you didn't win the lottery!"
  | _ -> failwith "Invalid Selection"

let valid_activity game categ =
  let name = Activities.get_category_name categ in
  match name with
  | "Health" -> Character.get_age game.player > 10
  | "Mind & Body" -> Character.get_age game.player > 10
  | "School" ->
      Character.get_age game.player <= 24 && Character.get_age game.player > 4
  | "Romance" -> Character.get_age game.player > 16
  | "Finance" -> Character.get_age game.player > 16 && game.bank > 0
  | "Career" -> Character.get_age game.player >= 18
  | "Entertainment" -> game.bank > 50
  | _ -> failwith "Invalid Category"

let set_activities game =
  game.activities <- List.filter (valid_activity game) activities

let get_activities game = game.activities

let set_employed game =
  match Character.get_occupation game.player with
  | None -> game.is_employed <- false
  | Some _ -> game.is_employed <- true

let get_friends game = game.friends
let get_bank game = game.bank
