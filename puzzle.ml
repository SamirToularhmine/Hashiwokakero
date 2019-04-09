open Coordinate;;

type importance = Importance of int
type puzzle = Puzzle of ((Coordinate.coordinate * importance) list)
                        
let create = fun l -> Puzzle l
    
let reduction f =
  function | Puzzle plist -> List.fold_right f plist
                               
let getImportance = fun n -> Importance n
    
let list_of_puzzle = fun p ->
  match p with
  | Puzzle p -> p

let getMaxRow = fun p ->
  Coordinate.fstcoord (fst (reduction (fun x -> fun y -> if (fst x) >~ (fst y) then x else y) p (List.hd (list_of_puzzle p))))

let getMaxCol = fun p ->
  Coordinate.fstcoord (fst (reduction (fun x -> fun y -> if (fst x) >~~ (fst y) then x else y) p (List.hd (list_of_puzzle p))))


