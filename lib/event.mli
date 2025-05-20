type t
(** The type representing an event. *)

val create_event : string -> string -> string list -> Option.t list -> t
(** [create_event id description conditions options] creates a new event with
    the given [id], [description], [conditions], and [options]. Returns: A newly
    created event of type [t]. *)

val load_events : string -> t list
(** [load_events file_path] loads the list of events from the [file_path]
    specified. Requires: [file_path] must be a valid path to a valid
    representation of an event.*)

val get_id : t -> string
(** [get_id event] returns the name of the [event]. *)

val get_description : t -> (string * Character.t) list -> string
(** [get_description event npc_list] returns the description of the [event]
    based on the given list of NPC IDs and their associated characters. *)

val get_options : t -> Option.t list
(** [get_options event] is the list of options available for the given [event]. *)

val get_npc_needed : t -> string list
(** [get_npc_needed event] is the list of NPC IDs required for the given
    [event]. (Formatted like Friend, Friend 1, or New Friend)*)
