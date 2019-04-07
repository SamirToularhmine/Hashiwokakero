open Puzzle;;

type bridge = { isVertical : bool; isDoubled : bool}
type cell = Nothing | Island of Puzzle.importance | Bridge of bridge
type solution = cell list list

let initSolution = fun p -> Puzzle.create p
