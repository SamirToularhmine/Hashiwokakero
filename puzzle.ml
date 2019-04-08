open Coordinate;;

type importance = Importance of int
type puzzle = Puzzle of ((Coordinate.coordinate * importance) list)
                        
let create = fun l -> Puzzle l
    
let reduction f =
  function | Puzzle plist -> List.fold_right f plist
                               
let getImportance = fun n -> Importance n
    
let list_of_puzzle = fun p ->
  []
  
let rec toString = fun p ->
  match p with
  | Puzzle p ->
    match p with
    | [] -> ""
    | h::t -> (Coordinate.toString (fst h)) ^ (toString (Puzzle t))


(* let ok = Puzzle.Puzzle [(Puzzle.Coordinate (0,0),Puzzle.Importance 4)];;*)
(*open Coordinate*)                                      
(*let x = Coordinate (1,1)*)
