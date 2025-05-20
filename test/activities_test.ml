open OUnit2
open Yojson.Basic
open Yojson.Basic.Util
open Final_Project

(* Import the module to test *)

let test_create_job _ =
  let job =
    Activities.create_job ~name:"Software Engineer" ~field:"Tech" ~salary:100000
      ~stat:"Coding" ~threshold:50
  in
  assert_equal
    ~printer:(fun x -> x)
    "Software Engineer"
    (Activities.get_job_name job);
  assert_equal ~printer:(fun x -> x) "Tech" (Activities.get_job_field job);
  assert_equal ~printer:string_of_int 100000 (Activities.get_job_salary job);
  assert_equal ~printer:(fun x -> x) "Coding" (Activities.get_job_stat job);
  assert_equal ~printer:string_of_int 50 (Activities.get_job_threshold job)

let test_create_action _ =
  let action =
    Activities.create_action ~name:"Code Review" ~accessed:false
      ~effects:("Coding", 10, "Reviewed", "First Review")
  in
  assert_equal
    ~printer:(fun x -> x)
    "Code Review"
    (Activities.get_action_name action);
  assert_equal ~printer:string_of_bool false
    (Activities.get_action_accessed action);
  assert_equal
    ~printer:(fun (s, i, s1, s2) -> Printf.sprintf "(%s, %d, %s, %s)" s i s1 s2)
    ("Coding", 10, "Reviewed", "First Review")
    (Activities.get_action_effects action)

let test_create_category _ =
  let action =
    Activities.create_action ~name:"Code Review" ~accessed:false
      ~effects:("Coding", 10, "Reviewed", "First Review")
  in
  let category =
    Activities.create_category ~name:"Tech Tasks" ~options:[ action ]
  in
  assert_equal
    ~printer:(fun x -> x)
    "Tech Tasks"
    (Activities.get_category_name category);
  assert_equal ~printer:(String.concat ", ") [ "Code Review" ]
    (Activities.get_actions_name_in_category category)

let test_setters_job _ =
  let job =
    Activities.create_job ~name:"Software Engineer" ~field:"Tech" ~salary:100000
      ~stat:"Coding" ~threshold:50
  in
  Activities.set_job_salary job 120000;
  Activities.set_job_threshold job 60;
  assert_equal ~printer:string_of_int 120000 (Activities.get_job_salary job);
  assert_equal ~printer:string_of_int 60 (Activities.get_job_threshold job)

let test_setters_action _ =
  let action =
    Activities.create_action ~name:"Code Review" ~accessed:false
      ~effects:("Coding", 10, "Reviewed", "First Review")
  in
  Activities.set_action_accessed action true;
  assert_equal ~printer:string_of_bool true
    (Activities.get_action_accessed action)

let test_load_activity _ =
  let activities = Activities.load_activity "data/test_activities.json" in
  assert_equal ~printer:string_of_int 2 (List.length activities);
  let first_category = List.hd activities in
  assert_equal
    ~printer:(fun x -> x)
    "Tech Tasks"
    (Activities.get_category_name first_category);
  let first_action = List.hd (Activities.get_category_options first_category) in
  assert_equal
    ~printer:(fun x -> x)
    "Code Review"
    (Activities.get_action_name first_action)

let test_load_jobs _ =
  let jobs = Activities.load_jobs "../data/test_jobs.json" in
  assert_equal ~printer:string_of_int 2 (List.length jobs);
  let first_job = List.hd jobs in
  assert_equal
    ~printer:(fun x -> x)
    "Software Engineer"
    (Activities.get_job_name first_job);
  assert_equal ~printer:(fun x -> x) "Tech" (Activities.get_job_field first_job);
  assert_equal ~printer:string_of_int 100000
    (Activities.get_job_salary first_job);
  assert_equal
    ~printer:(fun x -> x)
    "Coding"
    (Activities.get_job_stat first_job);
  assert_equal ~printer:string_of_int 50
    (Activities.get_job_threshold first_job)

let test_select_random_elements _ =
  let jobs =
    [
      Activities.create_job ~name:"Job1" ~field:"Field1" ~salary:1000
        ~stat:"Stat1" ~threshold:10;
      Activities.create_job ~name:"Job2" ~field:"Field2" ~salary:2000
        ~stat:"Stat2" ~threshold:20;
      Activities.create_job ~name:"Job3" ~field:"Field3" ~salary:3000
        ~stat:"Stat3" ~threshold:30;
    ]
  in
  let selected = Activities.select_random_elements 2 jobs in
  assert_equal ~printer:string_of_int 2 (List.length selected);
  List.iter (fun job -> assert (List.mem job jobs)) selected

let test_incr_threshold _ =
  let job =
    Activities.create_job ~name:"Job1" ~field:"Field1" ~salary:1000
      ~stat:"Stat1" ~threshold:10
  in
  Activities.incr_threshold (Some job) 5;
  assert_equal ~printer:string_of_int 15 (Activities.get_job_threshold job)

let test_incr_salary _ =
  let job =
    Activities.create_job ~name:"Job1" ~field:"Field1" ~salary:1000
      ~stat:"Stat1" ~threshold:10
  in
  Activities.incr_salary (Some job);
  assert_equal ~printer:string_of_int 1100 (Activities.get_job_salary job)

let test_reset_accessed _ =
  let action1 =
    Activities.create_action ~name:"Action1" ~accessed:true
      ~effects:("Stat1", 10, "Msg1", "Msg2")
  in
  let action2 =
    Activities.create_action ~name:"Action2" ~accessed:true
      ~effects:("Stat2", 20, "Msg3", "Msg4")
  in
  let category =
    Activities.create_category ~name:"Category1" ~options:[ action1; action2 ]
  in
  Activities.reset_accessed [ category ];
  assert_equal ~printer:string_of_bool false
    (Activities.get_action_accessed action1);
  assert_equal ~printer:string_of_bool false
    (Activities.get_action_accessed action2)

let test_update_accessed _ =
  let action1 =
    Activities.create_action ~name:"Action1" ~accessed:false
      ~effects:("Stat1", 10, "Msg1", "Msg2")
  in
  let action2 =
    Activities.create_action ~name:"Action2" ~accessed:false
      ~effects:("Stat2", 20, "Msg3", "Msg4")
  in
  let category =
    Activities.create_category ~name:"Category1" ~options:[ action1; action2 ]
  in
  Activities.update_accessed [ category ] category "Action1" true;
  assert_equal ~printer:string_of_bool true
    (Activities.get_action_accessed action1);
  assert_equal ~printer:string_of_bool false
    (Activities.get_action_accessed action2)

let suite =
  [
    "test_create_job" >:: test_create_job;
    "test_create_action" >:: test_create_action;
    "test_create_category" >:: test_create_category;
    "test_setters_job" >:: test_setters_job;
    "test_setters_action" >:: test_setters_action;
    "test_load_activity" >:: test_load_activity;
    "test_load_jobs" >:: test_load_jobs;
    "test_select_random_elements" >:: test_select_random_elements;
    "test_incr_threshold" >:: test_incr_threshold;
    "test_incr_salary" >:: test_incr_salary;
    "test_reset_accessed" >:: test_reset_accessed;
    "test_update_accessed" >:: test_update_accessed;
  ]
