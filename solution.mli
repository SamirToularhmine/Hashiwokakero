open Puzzle

type bridge
type cell
type solution

val string_of_cell : cell -> string
      
val init_solution : Puzzle.puzzle -> cell list list

val toString : cell list list -> string

val debugPont : string

val solve : puzzle -> cell list list
