open Yojson.Basic.Util
module StringMap = Map.Make (String)

type t = {
  id : string;
  description : string;
  conditions : string list;
  options : Option.t list;
}
(** Abstraction Function (AF)
    - A value of type `t` represents a game event.
    - `id : string` is the unique identifier for the event.
    - `description : string` is a narrative explanation of what the event
      entails.
    - `conditions : condition` specifies prerequisites for the event, such as
      required NPCs.
    - `options : Option.t list` is the list of possible choices the player can
      make when encountering this event.

    Representation Invariant (RI)
    - `id` must be a non-empty string and should be unique across all events.
    - `description` must be a non-empty string.
    - `options` must be a non-empty list containing valid `Option.t` values.*)

let create_event id description conditions options =
  { id; description; conditions; options }

let load_events path =
  let cwd = Sys.getcwd () in
  (* Check if running in _build/default/test or similar *)
  let adjusted_path =
    if String.ends_with ~suffix:"_build/default/test" cwd then
      Filename.concat
        (Filename.dirname (Filename.dirname (Filename.dirname cwd)))
        path
    else Filename.concat cwd path
  in
  let json = Yojson.Basic.from_file adjusted_path in
  json |> to_list
  |> List.map (fun event_json ->
         {
           id = event_json |> member "id" |> to_string;
           description = event_json |> member "description" |> to_string;
           conditions =
             event_json |> member "conditions" |> member "npc_needed" |> to_list
             |> List.map to_string;
           options =
             event_json |> member "options" |> Yojson.Basic.Util.to_list
             |> List.map Option.from_json;
         })

let get_id event = event.id

let get_description event npc_list =
  let description = event.description in
  let replace_id_with_name desc (npc_id, npc) =
    let name = Character.get_name npc in
    let placeholder = "[" ^ npc_id ^ "]" in
    let rec replace_all acc start =
      match String.index_from_opt acc start placeholder.[0] with
      | None -> acc
      | Some idx ->
          if String.sub acc idx (String.length placeholder) = placeholder then
            replace_all
              (String.sub acc 0 idx ^ name
              ^ String.sub acc
                  (idx + String.length placeholder)
                  (String.length acc - idx - String.length placeholder))
              (idx + String.length name)
          else replace_all acc (idx + 1)
    in
    replace_all desc 0
  in
  List.fold_left replace_id_with_name description npc_list

let get_options event = event.options
let get_npc_needed event = event.conditions
