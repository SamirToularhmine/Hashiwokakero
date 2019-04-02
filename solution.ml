open Puzzle;;

module type SOLUTION =
  sig
    type bridge
    type cell
    type solution
  end
  
module Solution : SOLUTION =
  struct
    type bridge = { isVertical : bool; isDoubled : bool}
    type cell = Nothing | Island of Puzzle.importance | Bridge of bridge
    type solution = cell list list
  end;;
                      
