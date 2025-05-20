type t
(** The type [t] represents a character. Holds the following character
    attributes:

    - [name]: A string representing the character's name.
    - [character_type]: An enumerated type that defines the type of the
      character (e.g. Player, Parent, Sibling, etc.).
    - [role]: A string representing the character's role if one is applicable.
      (e.g. "Brother" for male siblings).
    - [gender]: An enumerated type indicating the character's gender identity
      (e.g., Male, Female, Other).
    - [age]: An integer representing the character's age, which should be a
      non-negative value.
    - [occupation]: An optional string representing the character's occupation.
      This field can be [Some occupation] if the character has a job, or [None]
      if not specified.
    - [levels]: A record that holds various attributes related to the
      character's performance or status in different areas, such as happiness,
      health, and other measurable traits. *)

type character_type
(** The type representing different types of characters. Each variant
    corresponds to a specific type the character can have in the game or
    simulation:
    - [Player]: Represents the main character controlled by the character .
    - [Parent]: Represents a character who is a parent.
    - [Sibling]: Represents a character who is a sibling.
    - [Lover]: Represents a character who is a romantic partner.
    - [Friend]: Represents a character who is a friend.
    - [Child]: Represents a character who is a child.
    - [Pet]: Represents a character who is a pet. *)

val player : character_type
(** [player] is a character_type of [Player]. *)

val parent : character_type
(** [parent] is a character_type of [Parent]. *)

val sibling : character_type
(** [sibling] is a character_type of [Sibling]. *)

val lover : character_type
(** [lover] is a character_type of [Lover]. *)

val friend : character_type
(** [friend] is a character_type of [Friend]. *)

val child : character_type
(** [child] is a character_type of [Child]. *)

val pet : character_type
(** [pet] is a character_type of [Pet]. *)

type gender
(** The type representing the gender of a character. The possible values are:
    - [Male]: Represents male gender.
    - [Female]: Represents female gender.
    - [Other]: Represents other gender identities. *)

val male : gender
(** [male] is a gender of type [Male] *)

val female : gender
(** [female] is a gender of type [Female] *)

val other : gender
(** [other] is a gender of type [Other] *)

val create_character :
  string -> character_type -> gender -> int -> Activities.job option -> t
(** [create_character name character_type gender age occupation] creates a
    character with the specified attributes. All level values are randomly
    assigned. Requires: [age] to be a non-negative value. *)

val create_random : character_type -> t -> t
(** [create_random char_type player] creates a random character of the given
    [char_type] based on the given [player]. (e.g. if the [char_type] is
    [Sibling], the last name will match that of the [player]). *)

val create_random_player : unit -> t
(** [create_random_player ()] creates a random player character with random
    attributes. *)

val create_random_parent : gender -> string -> t
(** [create_random_parent gender last_name] creates a random parent character
    with the given [gender] and [last_name] appended to a random first name. *)

val create_random_sibling : string -> t
(** [create_random_sibling last_name] creates a random sibling character with
    the given [last_name] appended to a random last name. *)

val create_random_pet : unit -> t
(** [create_random_pet ()] creates a random pet character. *)

val get_name : t -> string
(** [get_name t] is the name of character [t]. *)

val get_age : t -> int
(** [get_age t] is the age of character [t]. *)

val get_gender : t -> gender
(** [get_gender t] is the gender of character [t]. *)

val get_character_type : t -> character_type
(** [get_character_type t] is the character type of character [t]. *)

val get_char_type_of_str : string -> character_type
(** [get_char_type_of_str str] is character_type of [str]. Raises:
    [Failure "Unknown character type"] if [str] does not match one of the define
    character types. *)

val get_str_of_char_type : character_type -> string
(** [get_str_of_char_type char_type] is the string of [char_type]. *)

val get_occupation : t -> Activities.job option
(** [get_occupation t] is the occupation of character [t]. *)

val get_level : string -> t -> int
(** [get_level level_name t] is the current value of the level identified by
    [level_name] from the structure [t]. Raises: [Failure "Unknown level"] if
    [level_name] is not one of the defined levels. *)

val get_role : t -> string
(** [get_role t] is the role of character [t] if one is applicable. Otherwise,
    it is the string of the character type of character [t]. *)

val set_role : t -> string -> unit
(** [set_role t role] sets the role of character [t] to the given [role]. *)

val set_age : t -> int -> unit
(** [set_age t age] sets the age of character [t] to [age]. Requires: [age] must
    be a non-negative value. *)

val set_occupation : t -> Activities.job option -> unit
(** [set_occupation t occupation] updates the occupation of [t] to [occupation].
    Can be [Some new_occupation] or [None]. *)

val set_level : string -> int -> t -> unit
(** [set_level level_name new_value t] updates the level identified by
    [level_name] to [new_value] for character [t]. The value is bounded between
    0 and 100. Raises: [Failure "Unknown level"] if [level_name] is not defined. *)

val change_level : string -> int -> t -> unit
(** [change_level level_name delta t] modifies the level identified by
    [level_name] by [delta] for character [t]. The result is bounded between 0
    and 100. Raises: [Failure "Unknown level"] if [level_name] is not defined. *)

val modify_level : string -> t -> (int -> int) -> unit
(** [modify_level level_name t f] applies the function [f] to the level
    identified by [level_name] for character [t]. Raises:
    [Failure "Unknown level"] if [level_name] is not defined. *)

val should_die : t -> bool
(** [should_die t] is [true] if character [t] should die based on age, health
    level, or character type. *)

val inc_age : t -> unit
(** [inc_age t] increments the age of character [t] by 1. *)

val to_string : t -> string
(** [to_string t] is a string representation of character [t]. *)
