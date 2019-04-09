open Puzzle

type bridge
type cell
type solution

val string_of_cell : cell -> string
      
val initSolution : Puzzle.puzzle -> cell list list

val toString : cell list list -> string
