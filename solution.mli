open Puzzle

type bridge
type cell
type solution
      
val init_solution : Puzzle.puzzle -> solution

val toString : solution -> string

val solve : puzzle -> solution

val display_solution : puzzle -> unit
