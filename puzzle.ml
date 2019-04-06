open Coordinate;;

module type PUZZLE =
  sig
    type importance
    type puzzle

    val create : (Coordinate.coordinate * importance) list -> puzzle
    val reduction : ((Coordinate.coordinate * importance) -> 'a -> 'a) -> puzzle -> 'a -> 'a 
  end

module Puzzle : PUZZLE =
  struct
    type importance = Importance of int
    type puzzle = Puzzle of ((Coordinate.coordinate * importance) list)
                          
    let create = fun l -> Puzzle l
    let reduction f =
      function | Puzzle plist -> List.fold_right f plist
  end;;
(* let ok = Puzzle.Puzzle [(Puzzle.Coordinate (0,0),Puzzle.Importance 4)];;*)
(*open Coordinate*);;                                      
(*let x = Coordinate (1,1)*);;
