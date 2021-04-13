open Imandra_goals


let expected_to_string : expected -> string = function
  | True ->
      "True"
  | False ->
      "False"
  | Unknown ->
      "Unknown"


let result_of_verify_result
    ~(expected : expected) (verify_result : Verify.t) =
  match (expected, verify_result) with
  | True, (Verify.V_proved _ | Verify.V_proved_upto _)
  | False, Verify.V_refuted _
  | Unknown, Verify.V_unknown _ ->
      ()
  | _ ->
      Alcotest.failf
        "Expected %a, but got@ %a"
        (CCFormat.of_to_string expected_to_string)
        expected
        Top_result.pp_view
        (Top_result.Verify verify_result)


let result_of_status (g : t) : unit =
  match g.status with
  | Open _ ->
      Alcotest.failf "Goal still open after close_goal"
  | Error s ->
      Alcotest.fail s
  | Closed { result; _ } ->
      result_of_verify_result ~expected:g.expected result


let test_of_goal (_g_id, (g : t)) =
  Alcotest.test_case g.name `Quick (fun () ->
      let g = close_goal g in
      result_of_status g )


let section_tests goals =
  let goals =
    let cmp (_, g) (_, g') = Stdlib.compare g.idx g'.idx in
    List.fast_sort cmp goals
  in
  CCList.head_opt goals
  |> CCOpt.map (fun (_id, (g : t)) ->
         ( g.section |> CCOpt.get_or ~default:"Global"
         , CCList.map test_of_goal goals ) )


let tests_of_goals goals =
  let goal_sections =
    CCList.group_by
      ~hash:(fun (_id, g) -> Hashtbl.hash g.section)
      ~eq:(fun (_id1, g1) (_id2, g2) -> g1.section = g2.section)
      goals
  in
  goal_sections |> CCList.filter_map section_tests


let run_tests ?report_name () =
  let goals = all () in
  let tests = tests_of_goals goals in
  let () =
    match report_name with
    | None ->
        ()
    | Some report_name ->
        let write_report () =
          CCFormat.printf
            "Writing report to %s/%s.html@."
            (Sys.getcwd ())
            report_name ;
          report report_name
        in
        at_exit write_report
  in
  Alcotest.run "Verification Goals" tests
