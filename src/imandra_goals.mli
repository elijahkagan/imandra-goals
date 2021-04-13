
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
    date   : float;
    result : Verify.t;
  }
  | Error of string

and expected = True | False | Unknown

and owner = string

and id = string * string option (* name, section *)

type goal = t

(** Start a new goal *)
val init :
  ?section:string ->
  ?owner:owner ->
  ?expected:expected ->
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  desc:string -> name:string ->
  unit -> unit

val close_goal :
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  t -> t

val close :
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  ?name:t -> unit -> unit

val verify_ :
  ?hints:Imandra_surface.Uid.t Imandra_surface.Hints.t ->
  ?name:t -> unit -> unit

val all : unit -> (id * goal) list
val opens : unit -> (id * goal) list
val closed : unit -> (id * goal) list

(** {2 Report} *)

val report : ?section_name:string -> ?compressed:bool -> string -> unit
