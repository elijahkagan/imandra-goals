
(** {1 Goal Manager} *)

module D = Imandra_document.Document
open Printf

type t = {
  name     : string;
  section  : string option;
  desc     : string;
  owner    : owner option;
  status   : status;
  expected : expected;
  idx      : int;
  hints    : Imandra_surface.Uid.t Imandra_surface.Hints.t option
}

and status =
  | Open of { assigned_to : owner option }
  | Closed of {
    timestamp : float;
    duration : float;
    result : Verify.t;
  }
  | Error of string

and expected = True | False | Unknown
and owner = string
and id = string * string option (* name, section *)

type goal = t

let id_of_goal (g : t) = (g.name, g.section)

module State = struct
  type t = {
    goals    : (id, goal) Hashtbl.t;
    focus    : goal option;
    section  : string option;
    max_idx  : int;
    t_begin  : float;
  }

  let state : t ref =
    ref {
      goals   = Hashtbl.create 100;
      focus   = None;
      section = None;
      max_idx = 0;
      t_begin = 0.;
    }

  module Set = struct

    let goals   x = state := { !state with goals = x }
    let focus   x = state := { !state with focus = x }
    let section x = state := { !state with section = x }
    let max_idx x = state := { !state with max_idx = x }
    let t_begin x = state := { !state with t_begin = x }

  end

  let install (g : goal) =
    Set.max_idx (!state.max_idx + 1);
    Hashtbl.add !state.goals (id_of_goal g) g

  let update (g : goal) =
    Hashtbl.replace !state.goals (id_of_goal g) g

  let list_of_goals () =
    let cmp (_, g) (_, g') = Stdlib.compare g.idx g'.idx in
    let goals = Util.list_of_hashtbl !state.goals in
    List.fast_sort cmp goals

  let clear () =
    state := {
      goals = Hashtbl.create 100;
      focus = None;
      section = None;
      max_idx = 0;
      t_begin = Unix.gettimeofday ();
    }
end

let clear = State.clear

module Section = struct

  let start x =
    State.Set.section (Some x);
    State.Set.t_begin (Unix.gettimeofday ())

  let stop _x =
    State.Set.section None

  let get () = !State.state.State.section

  let to_string () =
    match get () with
    | None   -> "<global>"
    | Some s -> s

end

let init ?section ?owner ?(expected=Unknown) ?hints ~desc ~name () : unit =
  let g = {
    name;
    section = (match section with None -> !State.state.State.section | x -> x);
    desc;
    owner;
    status = Open { assigned_to = owner };
    expected;
    idx = State.(!state.max_idx);
    hints;
  } in
  State.(install g);
  State.(Set.focus (Some g))

let focus () =
  State.(!state.focus)

let close_goal ?hints g =
  let timestamp = Unix.gettimeofday() in
  let finalise g =
    State.update g;
    State.Set.focus None;
    g
  in
  let hints =
    match hints with
    | None -> g.hints
    | Some _ -> hints
  in
  try
    let r = Verify.top ?hints g.name in
    let duration = Unix.gettimeofday() -. timestamp in
    let status = Closed { timestamp; duration; result=r; } in
    let g = { g with status  } in
    finalise g
  with _ ->
    let s = sprintf "Verification error: %s undefined?" g.name in
    let g = { g with status = Error s } in
    Printf.printf "%s\n%!" s;
    finalise g

let close ?hints ?name () =
  match name with
  | None   ->
    begin match focus () with
      | Some g -> close_goal ?hints g |> ignore
      | _ -> Error.unsupportedf "No goal under focus"
    end
  | Some g -> close_goal ?hints g |> ignore

let verify_ ?hints = close ?hints

let all () =
  State.(!state.goals) |> CCHashtbl.to_list

(* Report *)
exception Write_to_file of string * exn

let write_to_file filename s =
  try
    let cout = open_out filename in
    Printf.fprintf cout "%s\n" s;
    close_out cout;
  with e ->
    raise (Write_to_file (filename, e))

module Report = struct
  type progress = Complete of int | Partial of int * int

  let ok_green = D.p ~a:[D.A.cls "text-success"] "&check;"
  let bad_red = D.p ~a:[D.A.cls "text-danger"] "×"

  let doc_of_progress = function
    | Complete k ->
      D.block [ok_green; D.s_f "Verification complete: %d out of %d goals verified" k k]
    | Partial (k,k') ->
      D.block [bad_red; D.s_f "Verification incomplete: %d out of %d goals verified" k k']

  module Stat = struct
    type t = {
      proved: int;
      total: int;
      time: float;
    }

    let to_doc self : D.t =
      let {proved; total; time} = self in
      D.record [
        "proved", D.int proved;
        "total", D.int total;
        "time", D.s_f "%.2fs" time;
      ]

  end

  module Digest = struct
    type t = {
      progress       : progress;
      elapsed_time   : float;
      section_name   : string;
      (* report_file    : string; *)
      goal_names     : string list;
    }

    (* TODO: use https://getbootstrap.com/docs/4.5/components/progress/ *)
    let percent_of_progress = function
      | Complete _ -> 100
      | Partial (a,b) ->
        let q = (float_of_int a) /. (float_of_int b) *. 100. in
        int_of_float (floor q)

    let doc_of_digest (d : t) : D.t =
      let p = percent_of_progress d.progress in
      D.record [
        "section name", D.s d.section_name;
        "progress", D.s_f "%d %%" p;
        "elapsed time", D.s_f "%.2fs" d.elapsed_time;
        "goal names", D.fold ~summary:"goal names" ~folded_by_default:true @@
        D.list_of D.p d.goal_names;
      ]

    let get_stats (hs:t list) : Stat.t =
      let proved = ref 0 in
      let total  = ref 0 in
      let time   = ref 0. in
      let f = function
        | Complete k    ->
          proved := !proved + k;
          total := !total + k
        | Partial (a,b) ->
          proved := !proved + a;
          total  := !total  + b
      in
      List.iter (fun d -> f d.progress; time := !time +. d.elapsed_time) hs;
      Stat.{ proved= !proved; total= !total; time = !time; }
  end

  let build_id () =
    match Version.build_commit_id with
    | Some s -> D.s_f "- Build commit ID %s" (String.trim s)
    | None -> D.empty

  let in_header ~section_name ~status ~content ~stats : D.t =
    D.block [
      D.section "Imandra Verification Report";
      Stat.to_doc stats;
      D.block [
        D.section_f "Section: %s" section_name;
        doc_of_progress status;
        content;
      ];
      D.block [
        D.s_f "Verified with Imandra v%s" Version.version;
        build_id ();
      ];
    ]

  let status_marker g : D.t =
    let open Verify in
    match g.status, g.expected with
    | Closed { result=V_refuted _;_ }, True
    | Closed { result=V_proved _;_ }, False -> bad_red
    | Closed { result=V_proved _;_ }, True
    | Closed { result=V_refuted _;_ }, False -> ok_green
    | Error _, _ -> D.p ~a:[D.A.red; D.A.cls "text-danger"] "ERROR"
    | _          -> D.p ~a:[D.A.yellow; D.A.cls "text-warning"] "?"

  let item ?(compressed=false) (g:t) : D.t =
    Debug.tracef (fun k->k "Working on %s\n%!" g.name);
    let module V = Verify in
    let sd = match g.status with
      | Open _ -> D.bold @@ D.s "Goal is open"
      | Error s -> D.bold @@ D.s_f "Error: %s" s
      | Closed { result; _ }  ->
        let mkproof p = match p with
          | Some p when not compressed ->
            D.fold ~folded_by_default:true ~summary:"proof" p
          | _ -> D.empty
        and mk_model m =
          if compressed then D.empty
          else D.fold ~folded_by_default:false ~summary:"model" (Term.Model.to_doc m)
        in
        begin match result with
          | V.V_proved {proof=p;_} ->
            D.block [ D.bold @@ D.s "Proved"; mkproof p]
          | V.V_proved_upto {upto;_} ->
            D.bold @@ D.s_f "Proved up to %a" Event.print_upto upto;
          | V.V_refuted {proof=p; model; _} ->
            D.block [ D.bold @@ D.s "Refuted"; mk_model model; mkproof p ]
          | V.V_unknown {proof=p;_} ->
            D.block [D.bold @@ D.s "Unknown"; mkproof p]
        end
    in
    D.record [
      "status", (status_marker g);
      "VG", D.bold (D.p g.name);
      "description", D.p g.desc;
      "result", sd;
    ]

  let progress_of_oc goals =
    let opens =
      List.filter (fun (_, g) -> match g.status with Open _ | Error _ -> true | _ -> false)
        goals
    and closed =
      List.filter (fun (_, g) -> match g.status with Closed _ -> true | _ -> false)
        goals
    in
    let k_o = List.length opens in
    let k_c = List.length closed in
    let proved =
      CCList.count
        (fun (_, g) -> match g.status, g.expected with
           | Closed { result = Verify.V_proved _; _ }, True -> true
           | Closed { result = Verify.V_refuted _; _ }, False -> true
           | _ -> false)
        closed in
    let k_o = k_o + k_c - proved in
    if k_o = 0 then Complete k_c
    else Partial (proved, k_o + proved)

  let by_section ~compressed l : D.t * Stat.t =
    let goal_sections =
      CCList.group_by
        ~hash:(fun (_id, g) -> Hashtbl.hash g.section)
        ~eq:(fun (_id1, g1) (_id2, g2) ->
            CCEqual.option String.equal g1.section g2.section)
        l
    in

    (* document for this section (all docs are in the same section) *)
    let doc_of_sec ~section (goals:(_*goal) list) : D.t * Digest.t =
      let progress = progress_of_oc goals in

      (* compute digest *)
      let elapsed_time =
        List.fold_left
          (fun n (_,g) -> match g.status with
             | Closed {duration; _} -> n +. duration
             | _ -> n) 0. goals
      in
      let digest =
        Digest.{
          progress; elapsed_time; section_name=section;
          goal_names=List.map (fun (_,g) -> g.name) goals;
        } in

      D.block [
        D.section section;
        doc_of_progress progress;
        Digest.doc_of_digest digest;
        D.list_of (fun (_,x) -> item ~compressed x) goals
      ], digest
    in

    let docs, digests =
      List.map
        (fun goals ->
           let sec = match goals with (_,g)::_ -> g.section | _ -> None in
           let sec = CCOpt.get_or ~default:"<no section>" sec in
           let doc, digest = doc_of_sec ~section:sec goals in
           (sec, doc), digest)
        goal_sections
      |> List.split
    in
    (* sort by section *)
    let docs = List.sort (fun (s1,_)(s2,_) -> CCString.compare_natural s1 s2) docs in

    let stats = Digest.get_stats digests in
    D.list (List.map snd docs), stats

  let doc_to_html (doc:D.t) : string =
    let module H = Tyxml.Html in
    let module DH = Imandra_document_tyxml in

    let mapper = {
      DH.Mapper.default
      with

      (* just force size of first column of records *)
      DH.Mapper.attr_row = (fun self ~row ~col d ->
          let d = DH.Mapper.default.DH.Mapper.attr_row self ~row ~col d in
          if col>0 then d else (H.a_class ["col-3"] :> Html_types.td_attrib H.attrib) :: d);
    } in

    DH.Mapper.run_doc ~title:"Imandra report" mapper doc
    |> DH.string_of_html_doc

  let top ?(section_name=Section.to_string ()) ~compressed ~filename =
    try
      let l = State.list_of_goals () in
      let gs, stats = by_section ~compressed l in
      let progress = progress_of_oc l in
      let doc = in_header ~section_name ~status:progress ~content:gs ~stats in
      let html = doc_to_html doc in
      write_to_file (filename ^ ".html") html;
      (* TODO: put on top of file?
      let time = Unix.gettimeofday () -. State.(!state.t_begin) in
      let goal_names = List.map (fun (_, g) -> g.name) all in
         *)
      printf "Report written to %s.\n" filename
    with e ->
      Error.unsupportedf "Error writing report file (%s).\nException: %s"
        filename (Printexc.to_string e)

end

let report ?section_name ?(compressed=false) filename =
  Report.top ?section_name ~compressed ~filename

