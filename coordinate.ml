type coordinate = Coordinate of (int * int)

let coord_from_pair = fun (x,y) -> Coordinate (x,y)
let pair_from_coord = function | Coordinate c -> c 
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

let compare = fun c1 -> fun c2 ->
  let c11 = fstcoord c1 in
  let c12 = sndcoord c1 in
  let c21 = fstcoord c2 in
  let c22 = sndcoord c2 in
  if c11 > c21 then 1
  else if c11 = c21 then
    if c12 > c22 then 1 else
      if c12 = c22 then 0 else -1
  else -1;;
