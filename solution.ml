open Puzzle

type bridge = { isVertical : bool; isDoubled : bool}
type cell = Nothing | Island of Puzzle.importance | Bridge of bridge
type solution = cell list list

let initSolution = fun p ->
  match Puzzle.list_of_puzzle p with
  | [] -> []
  | h::t -> [];;
