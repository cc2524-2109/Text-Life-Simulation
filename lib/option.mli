type t
(** The type representing an option for an event. *)

val create :
  id:string ->
  description:string ->
  character_effects:(string * (string * int) list) list ->
  npc_added:string list ->
  t
(** [create ~id ~description ~character_effects ~npc_added] creates a new option
    with the given parameters. *)

val from_json : Yojson.Basic.t -> t
(** [from_json json] is an [Option.t] from the given JSON representation [json].
    Requires: [json] must be a valid representation of an option. *)

val get_id : t -> string
(** [get_id option] is the name of the [option]. *)

val get_description : t -> string
(** [get_description option] is the textual description of the given [option]. *)

val get_character_effects : t -> (string * (string * int) list) list
(** [get_character_effects option] is the list of character effects associated
    with the given [option]. Each effect specifies:
    - An NPC ID (ie. Friend, New Pet, Parent 1)
    - A list of level modifications, represented as pairs of level names and
      adjustment values. *)

val get_npc_added : t -> string list
(** [get_npc_added option] is the list of NPCs added associated with the given
    [option]. Unlike in NPC ID, the "New" keyword is omitted (ie. Friend, Friend
    1). *)

val set_id : t -> string -> t
(** [set_id option new_id] returns a new [option] with the [id] set to [new_id]. *)

val set_description : t -> string -> t
(** [set_description option new_description] returns a new [option] with the
    [description] set to [new_description]. *)

val set_character_effects : t -> (string * (string * int) list) list -> t
(** [set_character_effects option new_character_effects] returns a new [option]
    with [character_effects] set to [new_character_effects]. *)

val set_npc_added : t -> string list -> t
(** [set_npc_added option new_npc_added] returns a new [option] with [npc_added]
    set to [new_npc_added]. *)
