open Yojson.Basic.Util

type t = {
  id : string;
  description : string;
  character_effects : (string * (string * int) list) list;
  npc_added : string list;
}
(** Abstraction function: A record of type [t] represents a unique option
    identified by [id], which has a description of its consequences, a list of
    character effects (affecting stats of certain characters), and a list of
    NPCs that are added as a result of choosing the option.

    Representation invariant:
    - [id] is a non-empty string and is unique among all instances of [t].
    - [description] is a non-empty string.
    - [character_effects] is a list of tuples where:
    - Each first element (character) is a non-empty string.
    - The second element is a list of stat-effect pairs where:
    - Each stat is a non-empty string.
    - Each value is an integer (can be positive, negative, or zero).
    - [npc_added] is a list of non-empty strings with no duplicates. *)

(* Constructor *)
let create ~id ~description ~character_effects ~npc_added =
  { id; description; character_effects; npc_added }

(* Getters *)
let get_id opt = opt.id
let get_description opt = opt.description
let get_character_effects opt = opt.character_effects
let get_npc_added opt = opt.npc_added

(* Setters *)
let set_id opt new_id = { opt with id = new_id }

let set_description opt new_description =
  { opt with description = new_description }

let set_character_effects opt new_character_effects =
  { opt with character_effects = new_character_effects }

let set_npc_added opt new_npc_added = { opt with npc_added = new_npc_added }

(* JSON deserialization function *)
let from_json json =
  {
    id = json |> member "id" |> to_string;
    description = json |> member "description" |> to_string;
    character_effects =
      json |> member "character_effects" |> to_list
      |> List.map (fun char_eff ->
             ( char_eff |> member "character" |> to_string,
               char_eff |> member "effects" |> to_list
               |> List.map (fun eff ->
                      ( eff |> member "stat" |> to_string,
                        eff |> member "value" |> to_int )) ));
    npc_added = json |> member "npc_added" |> to_list |> List.map to_string;
  }
