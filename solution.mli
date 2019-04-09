open Puzzle

type bridge
type cell
type solution
      
val initSolution : Puzzle.puzzle -> cell list list

val toString : cell list list -> string
