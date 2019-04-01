module type PUZZLE =
  sig
    type coordinate
    type importance
    type puzzle

    val reduction : (coordinate * importance -> 'a -> 'a) -> puzzle -> 'a -> 'a 
  end

module Puzzle : PUZZLE =
  struct type coordinate = Coordinate of (int*int)
         type importance = Importance of int
         type puzzle = Puzzle of (coordinate * importance) list
                               
         let reduction f =
           function | Puzzle plist -> List.fold_right f plist
  end
(* let ok = Puzzle.Puzzle [(Puzzle.Coordinate (0,0),Puzzle.Importance 4)];;*)
                                          
                                           
