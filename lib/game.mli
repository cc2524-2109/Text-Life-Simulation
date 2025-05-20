type t
(** The type representing the game state. *)

val graveyard : Character.t list ref
(** A mutable list storing dead characters. *)

val mark_as_dead : Character.t -> unit
(** Moves the given character to the graveyard and prints a death message. *)

val is_dead : t -> bool
(** [is_dead] is [true] if the player character is in the graveyard. *)

val is_dead_char : Character.t -> bool
(** [is_dead_char char] is [true] if the given [char] is in the graveyard. *)

val random_create : unit -> t
(** [random_create ()] is a new game state with a random starting character. *)

val custom_create :
  string ->
  string ->
  Character.gender ->
  string ->
  Character.t list ->
  Character.t list ->
  Character.t list ->
  t
(** [custom_create fn ln gend ht parents siblings pets] is a new game state with
    a custom starting character. *)

val check_and_mark_deaths : t -> (Character.t -> unit) -> unit
(** [check_and_mark_deaths game] checks all characters in the game except the
    player for whether they should die, based on age and health. If a character
    is determined to be dead, they are added to the graveyard and removed from
    the active game state. *)

val age_up : t -> (Character.t -> unit) -> unit
(** [age_up game] increases the age of all characters currently in the game by
    1. It also calls [check_and_mark_deaths game] to check if any characters are
    marked for death*)

val gen_npc_list : t -> Event.t -> (string * Character.t) list
(** [gen_npc_list game event] generates a list of NPCs required for the given
    [event]. It returns an association list where each NPC ID from the event's
    required NPCs is paired with a corresponding character from the game or a
    newly created character if needed. If the required NPCs cannot be fully
    matched, it raises [Failure "Bad mapping"].*)

val gen_event : t -> Event.t option * (string * Character.t) list
(** [gen_event game] generates a new event option based on the current state of
    [game]. Depending on whether there are available events, returns Some event
    or None and the list of NPC IDs mapped to characters that will participate
    in the event. *)

val apply_option : t -> Option.t -> (string * Character.t) list -> unit
(** [apply_option game option npc_list] applies the effects of [option] to the
    [game] and modifies the states of characters in [npc_list] accordingly. *)

val to_string : t -> string
(** [to_string game] converts the current state of [game] into a readable string
    representation. *)

val add_character : t -> Character.t -> unit
(** [add_character game character] adds a new [character] to the game state
    [game]. Ensures that the character is placed into the appropriate group
    based on its type. *)

val get_relationships : t -> Character.t list
(** [get_relationships game] is the list of NPCs currently in the game. *)

val end_life : t -> string
(** [end_life game] ends the player's life in the given [game]. The final
    message detailing the end of the game is returned as a string. *)

val get_activities : t -> Activities.category list
(** [get_activities game] is the list of activity categories available in the
    current state of the given [game]. *)

val set_activities : t -> unit
(** [set_activities game] sets or updates the list of available activities for
    the given [game], based on the current state and conditions. *)

val relation_selection : t -> string -> string list * Character.t list
(** [relation_selection game relation_type] generates a selection of characters
    and their details based on the given [relation_type] (e.g., "friend",
    "lover"). It is a tuple of a list of string descriptors and the
    corresponding [Character.t] objects. *)

val friend_to_lover : t -> Character.t -> int -> string
(** [friend_to_lover game friend affinity] attempts to change the given [friend]
    character's relationship status to "lover" in the given [game], using the
    specified [affinity] level. The resulting message is a string indicating
    success or failure. *)

val marriage : t -> Character.t -> int -> string
(** [marriage game partner affinity] attempts to marry the given [partner]
    character in the given [game], using the specified [affinity] level. The
    resulting message is a string indicating the outcome. *)

val change_finance : t -> int -> unit
(** [change_finance game amount] adjusts the player's bank balance in the given
    [game] by the specified [amount], which can be positive or negative. *)

val finance_activity : t -> string -> string
(** [finance_activity game activity_name] modifies the player's financial state
    in the given [game] based on the specified [activity_name]. It is a message
    describing the effect of the activity on the player's finances. *)

val valid_activity : t -> Activities.category -> bool
(** [valid_activity game categ] returns [true] if the activity category [categ]
    is valid for the current state of the game [game], based on the player's
    age, bank balance, and other relevant conditions. Raises:
    [Failure "Invalid Category"] if the category name is unrecognized. *)

val get_player : t -> Character.t
(** [get_player game] is the player's character in the given [game]. *)

val is_student : t -> bool
(** [is_student game] checks if the player's occupation in the given [game] is
    listed among recognized student occupations. *)

val is_married : t -> bool
(** [is_married game] is [true] if the player is married in the given [game]. *)

val is_employed : t -> bool
(** [is_employed game] is [true] if the player is employed in the given [game]. *)

val set_employed : t -> unit
(** [set_employed game] updates the player's employment status in the given
    [game] to reflect that they are now employed. *)

val get_friends : t -> Character.t list
(** [get_friends game] returns the list of friends in the game. *)

val get_bank : t -> int
(** [get_bank game] returns the bank balance of the player in the game. *)
