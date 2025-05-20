open Yojson.Basic.Util
module StringMap = Map.Make (String)

type job = {
  name : string;
  field : string;
  mutable salary : int;
  stat : string;
  mutable threshold : int;
}
(** Abstraction function: A `job` record {name; field; salary; stat; threshold} 
    represents a role in a specific field, with an associated salary, a stat used to evaluate suitability, 
    and a threshold value indicating the required proficiency in that stat. 

    Representation invariant:
    - [name] and [field] are non-empty strings.
    - [salary] is a non-negative integer.
    - [threshold] is a non-negative integer.
*)

type action = {
  name : string;
  mutable accessed : bool;
  effects : string * int * string * string;
}
(** Abstraction function: An `action` record {name; accessed; effects} represents 
    a task or activity with a unique name, a boolean flag indicating whether it 
    has been accessed, and associated effects defined as a tuple 
    (stat, value, accessed_msg, first_access_msg). 

    Representation invariant:
    - [name] is a non-empty string.
    - [effects] is a tuple where:
      - [stat] is a non-empty string.
      - [value] is an integer (can be negative or positive).
      - [accessed_msg] and [first_access_msg] are non-empty strings.
*)

type category = {
  name : string;
  options : action list;
}
(** Abstraction function: A `category` record {name; options} represents a category 
    of actions, where [name] identifies the category and [options] is a list of 
    actions belonging to that category.

    Representation invariant:
    - [name] is a non-empty string.
    - [options] is a list containing no duplicate actions (based on [name]).
    - All actions in [options] satisfy the representation invariant for `action`.
*)

(* Constructors *)
let create_job ~name ~field ~salary ~stat ~threshold =
  { name; field; salary; stat; threshold }

let create_action ~name ~accessed ~effects = { name; accessed; effects }
let create_category ~name ~options = { name; options }

(* Getters for job *)
let get_job_name (job : job) = job.name
let get_job_field (job : job) = job.field
let get_job_salary (job : job) = job.salary
let get_job_stat (job : job) = job.stat
let get_job_threshold (job : job) = job.threshold

(* Setters for job *)
let set_job_salary (job : job) salary = job.salary <- salary
let set_job_threshold (job : job) threshold = job.threshold <- threshold

(* Getters for action *)
let get_action_name (action : action) = action.name
let get_action_accessed (action : action) = action.accessed
let get_action_effects (action : action) = action.effects

(* Setters for action *)
let set_action_accessed (action : action) accessed = action.accessed <- accessed

(* Getters for category *)
let get_category_name category = category.name
let get_category_options category = category.options

let get_actions_name_in_category category =
  let actions = category.options in
  List.map (fun (act : action) -> act.name) actions

(* Function to load and parse activities from a JSON file *)
let load_activity path =
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
  |> List.map (fun category_json ->
         {
           name = category_json |> member "name" |> to_string;
           options =
             category_json |> member "options" |> to_list
             |> List.map (fun activity_json ->
                    {
                      name = activity_json |> member "name" |> to_string;
                      accessed = activity_json |> member "accessed" |> to_bool;
                      effects =
                        ( activity_json |> member "effects" |> member "stat"
                          |> to_string,
                          activity_json |> member "effects" |> member "value"
                          |> to_int,
                          activity_json |> member "effects"
                          |> member "accessed_msg" |> to_string,
                          activity_json |> member "effects"
                          |> member "first_access_msg" |> to_string );
                    });
         })

(* Function to load and parse jobs from a JSON file *)
let load_jobs path =
  let job_of_json json =
    {
      name = json |> member "name" |> to_string;
      field = json |> member "field" |> to_string;
      salary = json |> member "salary" |> to_int;
      stat = json |> member "stat" |> to_string;
      threshold = json |> member "threshold" |> to_int;
    }
  in
  let json = Yojson.Basic.from_file path in
  json |> to_list |> List.map job_of_json

let rec select_random_elements n job_lst =
  if n <= 0 || job_lst = [] then []
  else
    let index = Random.int (List.length job_lst) in
    let selected = List.nth job_lst index in
    let remaining = List.filter (fun x -> x <> selected) job_lst in
    selected :: select_random_elements (n - 1) remaining

(* Utility functions for jobs and categories *)
let job_list_to_string_list (jobs : job list) =
  List.map
    (fun (job : job) -> Printf.sprintf "%s ($%d)" job.name job.salary)
    jobs

let option_to_job job_opt =
  match job_opt with
  | Some job -> job
  | None -> failwith "Unemployed"

let get_stat_threshold job_opt =
  let job = option_to_job job_opt in
  (job.stat, job.threshold)

let incr_threshold job_opt delta =
  let job = option_to_job job_opt in
  job.threshold <- job.threshold + delta

let incr_salary job_opt =
  let job = option_to_job job_opt in
  job.salary <- job.salary + (job.salary / 10)

let generate_random_job () =
  let job_file_candidates =
    [ "data/jobs.json"; "../data/jobs.json"; "../../data/jobs.json" ]
  in
  let job_file =
    try List.find Sys.file_exists job_file_candidates
    with Not_found -> failwith "jobs.json not found in any known location."
  in
  let jobs = load_jobs job_file in
  let job_count = List.length jobs in
  if job_count = 0 then None
  else
    let index = Random.int job_count in
    Some (List.nth jobs index)

let student_occupations () =
  List.map (fun job -> Some job) (load_jobs "data/student_jobs.json")

let reset_accessed activities =
  List.iter
    (fun category ->
      List.iter
        (fun act -> set_action_accessed act false)
        (get_category_options category))
    activities

let update_accessed activities category act_name new_bool =
  List.iter
    (fun cat ->
      if get_category_name cat = get_category_name category then
        List.iter
          (fun act ->
            if get_action_name act = act_name then
              set_action_accessed act new_bool)
          (get_category_options cat))
    activities
