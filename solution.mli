open Puzzle;;

type bridge
type cell
type solution
      
val initSolution : (Coordinate.coordinate * Puzzle.importance) list -> Puzzle.puzzle

