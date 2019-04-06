module type COORDINATE =
  sig
    type coordinate

    val create : (int * int) -> coordinate
    val fstcoord : coordinate -> int   
    val sndcoord : coordinate -> int
    val (>~) : coordinate -> coordinate -> bool
    val (>~~) : coordinate -> coordinate -> bool

    val filtercoord : (coordinate -> coordinate -> bool) -> coordinate -> coordinate -> coordinate
    val toString : coordinate -> string
  end

module Coordinate : COORDINATE =
  struct
    type coordinate = Coordinate of (int * int)

    let create = fun (x,y) -> Coordinate (x,y)
    let fstcoord (Coordinate (x,y))=x
    let sndcoord (Coordinate (x,y))=y


    (* on definit des operateurs de comparaison sur les coordinates  *)
    let (>~) c1 c2 = (fstcoord c1) > (fstcoord c2)
    let (>~~) c1 c2 = (sndcoord c1) > (sndcoord c2)
                    
    (* recuperer c1 ou c2 selon un predicat, ici p *)
    let filtercoord p  =
      fun (x:coordinate) (y:coordinate) -> if (p x y) then x else y

    (* max selon colonne et max selon ligne *)
    let maxfstcoord c1 c2 = filtercoord (>~) c1 c2
    let maxsndcoord c1 c2 = filtercoord (>~~) c1 c2
    let toString = fun c ->
      match c with
      | Coordinate c -> "(" ^ (string_of_int (fst c)) ^ "," ^ (string_of_int (snd c)) ^ ")"
  end;;

