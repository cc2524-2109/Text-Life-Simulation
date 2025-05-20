open Csv

type player_levels = {
  mutable happiness : int;
  mutable health : int;
  mutable smarts : int;
  mutable looks : int;
}
(** Abstraction Function (AF):
    - player_levels represents the key attributes of the player character:
    - happiness: The player's emotional well-being or overall satisfaction
      (0-100).
    - health: The player's physical well-being and life expectancy (0-100).
    - smarts: The player's intellectual capability or intelligence (0-100).
    - looks: The player's physical attractiveness or appearance (0-100).

    Representation Invariant (RI):
    - All integer fields must be within the inclusive range [0, 100].
    - happiness, health, smarts, and looks must be valid integers.
    - Values outside this range should be corrected during assignment or
      updates. *)

type parent_levels = {
  mutable relationship : int;
  mutable religiousness : int;
  mutable generosity : int;
  mutable money : int;
}
(** Abstraction Function (AF):
    - parent_levels represents key attributes of a parent character in the game:
    - relationship: The closeness or emotional bond between the player and the
      parent (0-100).
    - religiousness: The degree of religious belief held by the parent (0-100).
    - generosity: The willingness of the parent to give or help others (0-100).
    - money: The parent's financial wealth or savings (0-100).

    Representation Invariant (RI):
    - All integer fields must be within the range [0, 100].
    - relationship, religiousness, generosity, and money must be valid integer
      values.
    - If any field falls outside this range, corrective actions must be taken
      (e.g., bounding values during updates). *)

type sibling_levels = {
  mutable relationship : int;
  mutable smarts : int;
  mutable looks : int;
  mutable petulance : int;
}
(** Abstraction Function (AF):
    - sibling_levels represents the key attributes of a sibling:
    - relationship: The closeness or emotional bond between the player and the
      sibling (0-100).
    - smarts: The intellectual capacity or intelligence of the sibling (0-100).
    - looks: The physical appearance or attractiveness of the sibling (0-100).
    - petulance: The degree of stubbornness, defiance, or irritability of the
      sibling (0-100).

    Representation Invariant (RI):
    - All integer fields must be within the range [0, 100].
    - relationship, smarts, looks, and petulance must be valid integer values.
    - If any field falls outside this range, corrective actions must be taken
      (e.g., bounding values upon updates). *)

type lover_levels = {
  mutable relationship : int;
  mutable looks : int;
  mutable smarts : int;
  mutable money : int;
  mutable craziness : int;
}
(** Abstraction Function (AF):
    - lover_levels represents the various traits of a romantic partner:
    - relationship: The emotional bond or closeness with the player (0-100).
    - looks: The physical appearance or attractiveness of the lover (0-100).
    - smarts: The intellectual capacity or intelligence of the lover (0-100).
    - money: The financial wealth or income of the lover (0-100).
    - craziness: The unpredictability or emotional instability of the lover
      (0-100).

    Representation Invariant (RI):
    - All integer fields must be within the range [0, 100].
    - relationship, looks, smarts, money, and craziness must be valid integer
      values.
    - If any field is outside this range, the abstraction breaks, requiring
      corrective measures (e.g., bounding values when they are modified). *)

type friend_levels = {
  mutable relationship : int;
  mutable looks : int;
  mutable smarts : int;
  mutable craziness : int;
}
(** Abstraction Function (AF):
    - friend_levels represents the various traits of a friend character:
    - relationship: The emotional bond or closeness with the player (0-100).
    - looks: The physical appearance or attractiveness of the friend (0-100).
    - smarts: The intellectual capacity or intelligence of the friend (0-100).
    - craziness: The unpredictability or eccentric behavior of the friend
      (0-100).

    Representation Invariant (RI):
    - All integer fields must be within the range [0, 100].
    - relationship, looks, smarts, and craziness must be valid integer values.
    - If any field is outside this range, the abstraction breaks, and corrective
      measures should be taken (e.g., bounding values when they are modified).
*)

type child_levels = {
  mutable relationship : int;
  mutable health : int;
  mutable smarts : int;
  mutable looks : int;
}
(** Abstraction Function (AF):
    - child_levels represents the various attributes of a child character:
    - relationship: The emotional bond between the child and the player (0-100).
    - health: The physical health status of the child (0-100).
    - smarts: The intellectual ability or education level of the child (0-100).
    - looks: The physical appearance or attractiveness of the child (0-100).

    Representation Invariant (RI):
    - All integer fields must be within the range [0, 100].
    - relationship, health, smarts, and looks must be valid integer values.
    - If any field is outside this range, the abstraction breaks and corrective
      measures should be taken (e.g., bounding the values during mutations). *)

type pet_levels = {
  mutable relationship : int;
  mutable health : int;
  mutable smarts : int;
  mutable happiness : int;
  mutable craziness : int;
}
(** Abstraction Function (AF): pet_levels represents the attributes of a pet in
    the game, including:

    - relationship: The pet's bond with the player (0-100).
    - health: The pet's health level (0-100).
    - smarts: The pet's intelligence (0-100).
    - happiness: How happy the pet is (0-100).
    - craziness: The pet's unpredictable behavior level (0-100).

    Representation Invariant (RI):

    - Each field must be between 0 and 100 inclusive.
    - relationship, health, smarts, happiness, and craziness should be valid
      integers within these bounds. *)

(* Generate a value that mostly stays within [bias_min, bias_max], but
   occasionally falls in the full [min, max] range. *)
let biased_random ~min ~max ~bias_min ~bias_max ~bias_weight =
  Random.self_init ();
  if Random.float 1.0 < bias_weight then
    Random.int_in_range ~min:bias_min ~max:bias_max
  else Random.int_in_range ~min ~max

let player_levels () =
  {
    happiness =
      biased_random ~min:10 ~max:100 ~bias_min:40 ~bias_max:80 ~bias_weight:0.8;
    health =
      biased_random ~min:50 ~max:100 ~bias_min:70 ~bias_max:100 ~bias_weight:0.9;
    smarts =
      biased_random ~min:10 ~max:100 ~bias_min:30 ~bias_max:70 ~bias_weight:0.8;
    looks =
      biased_random ~min:10 ~max:100 ~bias_min:40 ~bias_max:80 ~bias_weight:0.8;
  }

let parent_levels () =
  {
    relationship =
      biased_random ~min:0 ~max:100 ~bias_min:60 ~bias_max:90 ~bias_weight:0.85;
    religiousness =
      biased_random ~min:0 ~max:100 ~bias_min:20 ~bias_max:80 ~bias_weight:0.7;
    generosity =
      biased_random ~min:0 ~max:100 ~bias_min:30 ~bias_max:70 ~bias_weight:0.8;
    money =
      biased_random ~min:0 ~max:100 ~bias_min:40 ~bias_max:90 ~bias_weight:0.8;
  }

let sibling_levels () =
  {
    relationship =
      biased_random ~min:0 ~max:100 ~bias_min:40 ~bias_max:80 ~bias_weight:0.85;
    smarts =
      biased_random ~min:0 ~max:100 ~bias_min:30 ~bias_max:70 ~bias_weight:0.8;
    looks =
      biased_random ~min:0 ~max:100 ~bias_min:40 ~bias_max:80 ~bias_weight:0.8;
    petulance =
      biased_random ~min:0 ~max:100 ~bias_min:10 ~bias_max:50 ~bias_weight:0.7;
  }

let lover_levels () =
  {
    relationship =
      biased_random ~min:0 ~max:100 ~bias_min:50 ~bias_max:90 ~bias_weight:0.85;
    looks =
      biased_random ~min:0 ~max:100 ~bias_min:60 ~bias_max:90 ~bias_weight:0.9;
    smarts =
      biased_random ~min:0 ~max:100 ~bias_min:30 ~bias_max:70 ~bias_weight:0.8;
    money =
      biased_random ~min:0 ~max:100 ~bias_min:40 ~bias_max:80 ~bias_weight:0.8;
    craziness =
      biased_random ~min:0 ~max:100 ~bias_min:20 ~bias_max:80 ~bias_weight:0.7;
  }

let friend_levels () =
  {
    relationship =
      biased_random ~min:0 ~max:100 ~bias_min:40 ~bias_max:90 ~bias_weight:0.85;
    looks =
      biased_random ~min:0 ~max:100 ~bias_min:30 ~bias_max:70 ~bias_weight:0.8;
    smarts =
      biased_random ~min:0 ~max:100 ~bias_min:30 ~bias_max:70 ~bias_weight:0.8;
    craziness =
      biased_random ~min:0 ~max:100 ~bias_min:10 ~bias_max:40 ~bias_weight:0.7;
  }

let child_levels () =
  {
    relationship =
      biased_random ~min:0 ~max:100 ~bias_min:60 ~bias_max:90 ~bias_weight:0.85;
    health =
      biased_random ~min:0 ~max:100 ~bias_min:70 ~bias_max:100 ~bias_weight:0.9;
    smarts =
      biased_random ~min:0 ~max:100 ~bias_min:40 ~bias_max:80 ~bias_weight:0.8;
    looks =
      biased_random ~min:0 ~max:100 ~bias_min:30 ~bias_max:70 ~bias_weight:0.8;
  }

let pet_levels () =
  {
    relationship =
      biased_random ~min:0 ~max:100 ~bias_min:50 ~bias_max:90 ~bias_weight:0.85;
    health =
      biased_random ~min:0 ~max:100 ~bias_min:40 ~bias_max:90 ~bias_weight:0.8;
    smarts =
      biased_random ~min:0 ~max:100 ~bias_min:10 ~bias_max:50 ~bias_weight:0.7;
    happiness =
      biased_random ~min:0 ~max:100 ~bias_min:50 ~bias_max:90 ~bias_weight:0.85;
    craziness =
      biased_random ~min:0 ~max:100 ~bias_min:20 ~bias_max:80 ~bias_weight:0.75;
  }

type levels =
  | PlayerLevels of player_levels
  | ParentLevels of parent_levels
  | SiblingLevels of sibling_levels
  | LoverLevels of lover_levels
  | FriendLevels of friend_levels
  | ChildLevels of child_levels
  | PetLevels of pet_levels
      (** Abstraction Function (AF):
          - levels represents the character's attributes based on their specific
            role.
          - Each variant corresponds to a specific character type:
          - PlayerLevels: Tracks the player's core stats.
          - ParentLevels: Tracks stats related to parental figures.
          - SiblingLevels: Tracks stats relevant to siblings.
          - LoverLevels: Tracks stats relevant to romantic partners.
          - FriendLevels: Tracks stats relevant to friends.
          - ChildLevels: Tracks stats for children.
          - PetLevels: Tracks stats for pets.

          Representation Invariant (RI):
          - Each variant must contain only correctly initialized data
            structures.
          - The inner data structures must satisfy their own RIs (e.g.,
            `player_levels`).
          - Only one level type should be associated with a character at a time.
      *)

type character_type =
  | Player
  | Parent
  | Sibling
  | Lover
  | Friend
  | Child
  | Pet
      (** Abstraction Function (AF):
          - character_type defines the role or identity of a character in the
            game:
          - Player: The main character controlled by the player.
          - Parent: A parental figure associated with the player.
          - Sibling: A sibling of the player.
          - Lover: A romantic partner of the player.
          - Friend: A friend of the player.
          - Child: A child of the player.
          - Pet: A pet owned by the player.

          Representation Invariant (RI):
          - The value must be one of the explicitly defined character types.
          - No additional or invalid character types should exist.
          - Each character should be assigned only one character type. *)

let player = Player
let parent = Parent
let sibling = Sibling
let lover = Lover
let friend = Friend
let child = Child
let pet = Pet

type gender =
  | Male
  | Female
  | Other
      (** Abstraction Function (AF):
          - gender represents a character's gender identity, allowing for:
          - Male: The character is identified as male.
          - Female: The character is identified as female.
          - Other: The character's gender is not explicitly male or female,
            allowing flexibility for additional representations.

          Representation Invariant (RI):
          - The value must be one of `Male`, `Female`, or `Other`.
          - No additional or invalid gender types should exist.
          - Every character must have a valid, explicitly assigned gender. *)

let male = Male
let female = Female
let other = Other

type t = {
  name : string;
  character_type : character_type;
  mutable role : string;
  gender : gender;
  mutable age : int;
  mutable occupation : Activities.job option;
  levels : levels;
}
(** Abstraction Function (AF):
    The record `{name; character_type; role; gender; age; occupation; levels}` represents a character in the game with the following fields:

    - `name : string` — The character’s name, used as a unique identifier in the game.
    - `character_type : character_type` — Indicates the type of the character, such as `Player`, `Parent`, `Friend`, etc.
    - `role : string` — The character's specific role, like "Mother," "Best Friend," or "Spouse." This is mutable and may change based on game events.
    - `gender : gender` — Specifies the character’s gender (`Male`, `Female`, or `Other`).
    - `age : int` — The character’s current age. This value must be incremented as the game progresses.
    - `occupation : Activities.job option` — An optional job that the character holds. If `None`, the character is unemployed.
    - `levels : levels` — A record containing various mutable stats related to the character's attributes, such as health, happiness, and smarts.
    
    Representation Invariant (RI):
    The following invariants must always be upheld:
    
    - `name` is a non-empty string.
    - `age >= 0` (no negative ages).
    - If `occupation = Some job`, the job must be a valid job as defined in the game’s job system.
    - All level values in `levels` are integers in the range `[0, 100]`.
    - `role` must be consistent with `character_type` if the character has a specific role (e.g., `Parent` should have roles like "Father" or "Mother").
    - `gender` must be one of `Male`, `Female`, or `Other` as defined by the `gender` type.
*)

let create_character name character_type gender age occupation =
  {
    name;
    character_type;
    role =
      (match character_type with
      | Player -> ""
      | Parent -> (
          match gender with
          | Male -> "Father"
          | Female -> "Mother"
          | _ -> "Parent")
      | Sibling -> (
          match gender with
          | Male -> "Brother"
          | Female -> "Sister"
          | _ -> "Sibling")
      | Lover -> ""
      | Friend -> ""
      | Child -> (
          match gender with
          | Male -> "Son"
          | Female -> "Daughter"
          | _ -> "Child")
      | Pet -> "");
    gender;
    age;
    occupation;
    levels =
      (match character_type with
      | Player -> PlayerLevels (player_levels ())
      | Parent -> ParentLevels (parent_levels ())
      | Sibling -> SiblingLevels (sibling_levels ())
      | Lover -> LoverLevels (lover_levels ())
      | Friend -> FriendLevels (friend_levels ())
      | Child -> ChildLevels (child_levels ())
      | Pet -> PetLevels (pet_levels ()));
  }

let data_dir =
  if Sys.file_exists "./data/names/fem_first_names.csv" then "./data/names"
  else if Sys.file_exists "../data/names/fem_first_names.csv" then
    "../data/names"
  else failwith "data directory not found"

let fem_first_names =
  List.flatten (Csv.load (Filename.concat data_dir "fem_first_names.csv"))

let masc_first_names =
  List.flatten (Csv.load (Filename.concat data_dir "masc_first_names.csv"))

let last_names =
  List.flatten (Csv.load (Filename.concat data_dir "last_names.csv"))

let pet_names =
  List.flatten (Csv.load (Filename.concat data_dir "pet_names.csv"))

let random_element lst =
  Random.self_init ();
  let n = List.length lst in
  if n = 0 then failwith "List is empty, cannot pick a random element."
  else List.nth lst (Random.int n)

(* Function to create a random character *)
let create_random char_type player =
  Random.self_init ();
  let player_last_name =
    let parts = String.split_on_char ' ' player.name in
    List.hd (List.rev parts)
  in
  let player_age = player.age in
  let gender = if Random.int 2 = 0 then female else male in
  let gender_name gender =
    if gender = female then random_element fem_first_names
    else random_element masc_first_names
  in
  let name, age =
    match char_type with
    | Lover ->
        ( gender_name gender ^ " " ^ random_element last_names,
          biased_random ~min:18 ~max:90
            ~bias_min:(Int.max 13 (player_age - 5))
            ~bias_max:(Int.max 13 (player_age + 5))
            ~bias_weight:0.7 )
    | Child -> (gender_name gender ^ " " ^ player_last_name, 0)
    | Parent ->
        ( gender_name gender ^ " " ^ player_last_name,
          Random.int_in_range ~min:18 ~max:50 )
    | Sibling ->
        ( gender_name gender ^ " " ^ player_last_name,
          Random.int_in_range ~min:0 ~max:18 )
    | Friend ->
        ( gender_name gender ^ " " ^ random_element last_names,
          biased_random ~min:18 ~max:90
            ~bias_min:(Int.max 3 (player_age - 5))
            ~bias_max:(Int.max 3 (player_age + 5))
            ~bias_weight:0.9 )
    | Pet -> (random_element pet_names, Random.int_in_range ~min:0 ~max:10)
    | _ ->
        ( gender_name gender ^ " " ^ random_element last_names,
          Random.int_in_range ~min:0 ~max:10 )
  in
  create_character name char_type gender age None

let create_random_player () =
  Random.self_init ();
  let gender = if Random.int 2 = 0 then female else male in
  let first_name =
    if gender = female then random_element fem_first_names
    else random_element masc_first_names
  in
  create_character
    (first_name ^ " " ^ random_element last_names)
    Player gender 0 None

let create_random_parent gender last_name =
  Random.self_init ();
  let first_name =
    if gender = female then random_element fem_first_names
    else random_element masc_first_names
  in
  create_character
    (first_name ^ " " ^ last_name)
    Parent gender
    (Random.int_in_range ~min:18 ~max:50)
    (Activities.generate_random_job ())

let create_random_sibling last_name =
  Random.self_init ();
  let gender = if Random.int 2 = 0 then female else male in
  let first_name =
    if gender = female then random_element fem_first_names
    else random_element masc_first_names
  in
  create_character
    (first_name ^ " " ^ last_name)
    Sibling gender
    (Random.int_in_range ~min:0 ~max:18)
    None

let create_random_pet () =
  Random.self_init ();
  let gender = if Random.int 2 = 0 then female else male in
  let name = random_element pet_names in
  create_character name Pet gender (Random.int_in_range ~min:0 ~max:10) None

(* Getters for character attributes *)
let get_name t = t.name
let get_age t = t.age
let get_gender t = t.gender
let get_character_type t = t.character_type

let get_char_type_of_str str =
  match str with
  | "Parent" -> Parent
  | "Sibling" -> Sibling
  | "Lover" -> Lover
  | "Friend" -> Friend
  | "Child" -> Child
  | "Pet" -> Pet
  | "Player" -> Player
  | _ -> failwith "Unknown character type"

let get_str_of_char_type char_type =
  match char_type with
  | Parent -> "Parent"
  | Sibling -> "Sibling"
  | Lover -> "Lover"
  | Friend -> "Friend"
  | Child -> "Child"
  | Pet -> "Pet"
  | _ -> "Player"

let get_role t =
  match t.role with
  | "" -> get_str_of_char_type t.character_type
  | _ -> t.role

let set_role t role = t.role <- role
let get_occupation t = t.occupation

(* Setters for character attributes *)
let set_occupation t occupation = t.occupation <- occupation
let set_age t age = t.age <- age

(* Helper function to ensure a level is within bounds *)
let bound_level value = max 0 (min 100 value)

(* General function to retrieve and modify a specific level field *)
let modify_level level_name t f =
  match t.levels with
  | PlayerLevels levels -> (
      match level_name with
      | "happiness" -> levels.happiness <- f levels.happiness
      | "health" -> levels.health <- f levels.health
      | "smarts" -> levels.smarts <- f levels.smarts
      | "looks" -> levels.looks <- f levels.looks
      | _ -> failwith "Unknown level for player")
  | ParentLevels levels -> (
      match level_name with
      | "relationship" -> levels.relationship <- f levels.relationship
      | "religiousness" -> levels.religiousness <- f levels.religiousness
      | "generosity" -> levels.generosity <- f levels.generosity
      | "money" -> levels.money <- f levels.money
      | _ -> failwith "Unknown level for parent")
  | SiblingLevels levels -> (
      match level_name with
      | "relationship" -> levels.relationship <- f levels.relationship
      | "smarts" -> levels.smarts <- f levels.smarts
      | "looks" -> levels.looks <- f levels.looks
      | "petulance" -> levels.petulance <- f levels.petulance
      | _ -> failwith "Unknown level for sibling")
  | LoverLevels levels -> (
      match level_name with
      | "relationship" -> levels.relationship <- f levels.relationship
      | "looks" -> levels.looks <- f levels.looks
      | "smarts" -> levels.smarts <- f levels.smarts
      | "money" -> levels.money <- f levels.money
      | "craziness" -> levels.craziness <- f levels.craziness
      | _ -> failwith "Unknown level for lover")
  | FriendLevels levels -> (
      match level_name with
      | "relationship" -> levels.relationship <- f levels.relationship
      | "looks" -> levels.looks <- f levels.looks
      | "smarts" -> levels.smarts <- f levels.smarts
      | "craziness" -> levels.craziness <- f levels.craziness
      | _ -> failwith "Unknown level for friend")
  | ChildLevels levels -> (
      match level_name with
      | "relationship" -> levels.relationship <- f levels.relationship
      | "health" -> levels.health <- f levels.health
      | "smarts" -> levels.smarts <- f levels.smarts
      | "looks" -> levels.looks <- f levels.looks
      | _ -> failwith "Unknown level for child")
  | PetLevels levels -> (
      match level_name with
      | "relationship" -> levels.relationship <- f levels.relationship
      | "health" -> levels.health <- f levels.health
      | "smarts" -> levels.smarts <- f levels.smarts
      | "happiness" -> levels.happiness <- f levels.happiness
      | "craziness" -> levels.craziness <- f levels.craziness
      | _ -> failwith "Unknown level for pet")

let get_level level_name t =
  let value = ref 0 in
  modify_level level_name t (fun x ->
      value := x;
      x);
  !value

let set_level level_name new_value t =
  modify_level level_name t (fun _ -> bound_level new_value)

let change_level level_name delta t =
  modify_level level_name t (fun x -> bound_level (x + delta))

let should_die char =
  Random.self_init ();
  let age = get_age char in
  let char_type = get_character_type char in
  let health =
    match char_type with
    | (Player | Child | Pet) when get_level "health" char < 40 ->
        Some (get_level "health" char)
    | _ -> None
  in

  let age_factor =
    match char_type with
    | Pet ->
        if age < 5 then 0.0
        else if age < 10 then 0.1
        else if age < 15 then 0.7
        else 0.95
    | Child -> if age < 18 then 0.05 else 0.1
    | Parent | Lover | Friend | Sibling ->
        if age < 50 then 0.0
        else if age < 70 then 0.1
        else if age < 80 then 0.2
        else if age < 90 then 0.4
        else 0.95
    | Player ->
        if age < 60 then 0.0
        else if age < 80 then 0.4
        else if age < 100 then 0.75
        else 0.95
  in

  let death_chance =
    match health with
    | Some h -> age_factor +. (float_of_int (100 - h) /. 200.0)
    | None -> age_factor
  in

  Random.float 1.0 < death_chance

let inc_age t = t.age <- t.age + 1

let to_string t =
  let bar level =
    let filled = String.make (level / 10) '+' in
    let empty = String.make (10 - (level / 10)) '-' in
    "[" ^ filled ^ empty ^ "]"
  in

  let level_to_string label bar level =
    Printf.sprintf "%s: %s (%d%%)" label (bar level) level
  in

  let levels_to_string levels =
    match levels with
    | PlayerLevels l ->
        Printf.sprintf "%s\n%s\n%s\n%s"
          (level_to_string "Happiness" bar l.happiness)
          (level_to_string "Health" bar l.health)
          (level_to_string "Smarts" bar l.smarts)
          (level_to_string "Looks" bar l.looks)
    | ParentLevels l ->
        Printf.sprintf "%s\n%s\n%s\n%s"
          (level_to_string "Relationship" bar l.relationship)
          (level_to_string "Religiousness" bar l.religiousness)
          (level_to_string "Generosity" bar l.generosity)
          (level_to_string "Money" bar l.money)
    | SiblingLevels l ->
        Printf.sprintf "%s\n%s\n%s\n%s"
          (level_to_string "Relationship" bar l.relationship)
          (level_to_string "Smarts" bar l.smarts)
          (level_to_string "Looks" bar l.looks)
          (level_to_string "Petulance" bar l.petulance)
    | LoverLevels l ->
        Printf.sprintf "%s\n%s\n%s\n%s\n%s"
          (level_to_string "Relationship" bar l.relationship)
          (level_to_string "Looks" bar l.looks)
          (level_to_string "Smarts" bar l.smarts)
          (level_to_string "Money" bar l.money)
          (level_to_string "Craziness" bar l.craziness)
    | FriendLevels l ->
        Printf.sprintf "%s\n%s\n%s\n%s"
          (level_to_string "Relationship" bar l.relationship)
          (level_to_string "Looks" bar l.looks)
          (level_to_string "Smarts" bar l.smarts)
          (level_to_string "Craziness" bar l.craziness)
    | ChildLevels l ->
        Printf.sprintf "%s\n%s\n%s\n%s"
          (level_to_string "Relationship" bar l.relationship)
          (level_to_string "Health" bar l.health)
          (level_to_string "Smarts" bar l.smarts)
          (level_to_string "Looks" bar l.looks)
    | PetLevels l ->
        Printf.sprintf "%s\n%s\n%s\n%s\n%s"
          (level_to_string "Relationship" bar l.relationship)
          (level_to_string "Health" bar l.health)
          (level_to_string "Smarts" bar l.smarts)
          (level_to_string "Happiness" bar l.happiness)
          (level_to_string "Craziness" bar l.craziness)
  in
  Printf.sprintf "Name: %s (%s)\nGender: %s\nAge: %d%s\n\nLevels:\n%s" t.name
    (get_role t)
    (match t.gender with
    | Male -> "Male"
    | Female -> "Female"
    | Other -> "Other")
    t.age
    (match t.occupation with
    | Some o ->
        "\nOccupation: " ^ List.hd (Activities.job_list_to_string_list [ o ])
    | None -> "")
    (levels_to_string t.levels)
