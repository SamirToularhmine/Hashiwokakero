(* Types initiaux *)
type coordinate = int * int;;
type importance = int;;
type puzzle = (coordinate * importance) list;;

(* Types Finaux *)
type bridge = { isVertical : bool; isDoubled : bool};;
type cell = Nothing | Island of importance | Bridge of bridge;;
type solution = cell list list;;

(* Puzzles *)
let puzzle = [((0, 0), 4)];;
