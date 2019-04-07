open Coordinate;;

type importance
type puzzle

val create : (Coordinate.coordinate * importance) list -> puzzle
val reduction : ((Coordinate.coordinate * importance) -> 'a -> 'a) -> puzzle -> 'a -> 'a
val getImportance : int -> importance
val toString : puzzle -> string
val list_of_puzzle : puzzle -> (Coordinate.coordinate * importance) list
