open Puzzle
open Coordinate

type bridge = { isVertical : bool; isDoubled : bool}
type cell = Nothing | Island of Puzzle.importance | Bridge of bridge
type solution = cell list list


let test = Island (getImportance 3)
          
let sol1 = [
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Island (getImportance 8);Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Nothing]
  ]
type direction = Gauche | Haut | Droite | Bas
let dessinerPonts sol coord dir =
  0

let initSolution = fun p ->
  let liste = Puzzle.list_of_puzzle p in
  let maxCol = Puzzle.getMaxCol p in
  let maxRow = Puzzle.getMaxRow p in
  let rec creerSolution = fun l -> fun i -> fun j ->
    let rec iles = fun i -> fun l -> 
      match l with
      | [] -> []
      | h::t ->
        if Coordinate.fstcoord ((fst)h) = i then h::(iles i t) else (iles i t) in
    if i > maxRow then [] else
      let rec creerLigne =
        fun l -> fun j ->
          match l with
          | [] ->
            if j <= maxCol then (Nothing)::(creerLigne l (j+1)) else []
          | h::t ->
            let coord = fst h in
            let importance = snd h in
            if (Coordinate.sndcoord coord > j)
            then (Nothing)::(creerLigne (h::t) (j+1))  
            else
            if j > maxCol
            then []
            else
            if (Coordinate.sndcoord coord = j)
            then
              (Island importance)::(creerLigne t (j+1))
            else
              (Nothing)::(creerLigne t (j+1))
        in (creerLigne (iles i l) 0)::(creerSolution liste (i+1) 0)
  in (creerSolution liste 0 0)
     
let rec toString = fun s ->
  match s with
  | [] -> ""
  | h::t ->
    let rec toStringLigne = fun l ->
      match l with
      | [] -> ""
      | h1::t1 ->
        match h1 with
        | Nothing -> "    " ^ (toStringLigne t1)
        | Island i -> "⓪" ^ (toStringLigne t1)
        | Bridge b -> "Pont" ^ (toStringLigne t1) 
    in (toStringLigne h) ^ "\n" ^(toString t)




