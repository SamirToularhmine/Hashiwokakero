open Puzzle;;

module type SOLUTION =
  sig
    type bridge
    type cell
    type solution
      
    val initSolution : Puzzle.puzzle -> solution
  end
  
module Solution : SOLUTION =
  struct
    type bridge = { isVertical : bool; isDoubled : bool}
    type cell = Nothing | Island of Puzzle.importance | Bridge of bridge
    type solution = cell list list

    let initSolution = fun p ->
      match Puzzle.list_of_puzzle p with
      | [] -> []
      | h::t -> []
      
  end;;
                      
