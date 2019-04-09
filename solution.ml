open Puzzle

type bridge = { isVertical : bool; isDoubled : bool}
type cell = Nothing | Island of Puzzle.importance | Bridge of bridge
type solution = cell list list

let initSolution = fun p ->
  match Puzzle.list_of_puzzle p with
  | [] -> []
  | h::t -> []

let test = Island (getImportance 3)
          
let sol1 = [
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Island (getImportance 8);Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Nothing]
  ]
type direction = Gauche | Haut | Droite | Bas
let dessinerPonts sol coord dir =
  0
