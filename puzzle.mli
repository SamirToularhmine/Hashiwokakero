open Coordinate;;

type importance
type puzzle

   
val puzzle_of_list : (Coordinate.coordinate * importance) list -> puzzle
val reduction : ((Coordinate.coordinate * importance) -> 'a -> 'a) -> puzzle -> 'a -> 'a
val importance_of_int : int -> importance
val int_of_importance : importance -> int 
val list_of_puzzle : puzzle -> (Coordinate.coordinate * importance) list
val getMaxCol : puzzle -> int
val getMaxRow : puzzle -> int
val sort : puzzle -> puzzle
