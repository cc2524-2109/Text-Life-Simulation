open Yojson.Basic.Util
module StringMap : Map.S with type key = String.t

type job
(** Abstract type for jobs*)

type action
(** Abstract type for actions*)

type category
(** Abstract type for categories *)

val create_job :
  name:string ->
  field:string ->
  salary:int ->
  stat:string ->
  threshold:int ->
  job
(** [create_job ~name ~field ~salary ~stat ~threshold] creates a new job with
    the given parameters. *)

val create_action :
  name:string ->
  accessed:bool ->
  effects:string * int * string * string ->
  action
(** [create_action ~name ~accessed ~effects] creates a new action with the given
    parameters. *)

val create_category : name:string -> options:action list -> category
(** [create_category ~name ~options] creates a new category with a name and a
    list of actions. *)

(* Getters for accessing job attributes *)

val get_job_name : job -> string
(** [get_job_name job] retrieves the name of the given job. *)

val get_job_field : job -> string
(** [get_job_field job] retrieves the field associated with the given job. *)

val get_job_salary : job -> int
(** [get_job_salary job] retrieves the salary of the given job. *)

val get_job_stat : job -> string
(** [get_job_stat job] retrieves the stat associated with the given job. *)

val get_job_threshold : job -> int
(** [get_job_threshold job] retrieves the threshold value of the given job. *)

(* Setters for updating job attributes *)

val set_job_salary : job -> int -> unit
(** [set_job_salary job new_salary] sets the salary of the given job to
    [new_salary]. *)

val set_job_threshold : job -> int -> unit
(** [set_job_threshold job new_threshold] sets the threshold of the given job to
    [new_threshold]. *)

(* Getters for accessing action attributes *)

val get_action_name : action -> string
(** [get_action_name action] retrieves the name of the given action. *)

val get_action_accessed : action -> bool
(** [get_action_accessed action] checks if the given action has been accessed. *)

val get_action_effects : action -> string * int * string * string
(** [get_action_effects action] retrieves the effects associated with the given
    action. *)

(* Setter for updating action attributes *)

val set_action_accessed : action -> bool -> unit
(** [set_action_accessed action accessed] sets the accessed status of the given
    action to [accessed]. *)

(* Getters for accessing category attributes *)

val get_category_name : category -> string
(** [get_category_name category] retrieves the name of the given category. *)

val get_category_options : category -> action list
(** [get_category_options category] retrieves the list of actions in the given
    category. *)

val get_actions_name_in_category : category -> string list
(** [get_actions_name_in_category]Function to get the names of actions within a
    specific category **)

(* Functions to load and parse data from JSON files *)

val load_activity : string -> category list
(** [load_activity path] parses a list of categories from a JSON file at the
    given path. *)

val load_jobs : string -> job list
(** [load_jobs path] parses a list of jobs from a JSON file at the given path. *)

(* Utility functions for jobs and categories *)

val select_random_elements : int -> 'a list -> 'a list
(** [select_random_elements n list] selects [n] random elements from the given
    list. *)

val job_list_to_string_list : job list -> string list
(** [job_list_to_string_list jobs] converts a list of jobs into a list of
    strings representing job details. *)

val option_to_job : job option -> job
(** [option_to_job job_option] converts a [job option] to a [job], raising an
    exception if the option is None. *)

val get_stat_threshold : job option -> string * int
(** [get_stat_threshold job_option] retrieves the stat and threshold from a
    [job option]. *)

val incr_threshold : job option -> int -> unit
(** [incr_threshold job_option delta] increments the threshold of a job by
    [delta]. *)

val incr_salary : job option -> unit
(** [incr_salary job_option] increases the salary of a job by 10%. *)

val generate_random_job : unit -> job option
(** [generate_random_job ()] generates a random job from a preloaded list of
    jobs. *)

val student_occupations : unit -> job option list
(** [student_occupations ()] retrieves a list of optional student occupations. *)

val reset_accessed : category list -> unit
(** [reset_accessed activities] resets the accessed status of all events in
    [activities] *)

val update_accessed : category list -> category -> string -> bool -> unit
(** [update_accessed activities category act_name new_bool] sets the accessed
    status of the event in [category] with name [act_name] to [new_bool] *)
