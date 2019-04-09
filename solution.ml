open Puzzle;;
open Coordinate;;
open List;;
type bridge = { isVertical : bool; isDoubled : bool}
type cell = Nothing | Island of Puzzle.importance | Bridge of bridge
type solution = cell list list

let string_of_cell = function
  | Nothing -> "Nothing"
  | Island x -> ("Island" ^ (string_of_int (int_of_importance x)))
  | Bridge b -> "Bridge : " ^ (string_of_bool b.isVertical) ^ " : " ^ (string_of_bool b.isDoubled)

let test = Island (importance_of_int 3)
          
let sol1 = [
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Island (importance_of_int 8);Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Nothing]
  ]
  
type direction = Gauche | Haut | Droite | Bas
                 
let c = coord_from_pair (1,1)
    
let getIsland sol = function | (x,y) -> nth (nth sol x) y
                                          
let d = string_of_cell (getIsland sol1 (2,2))
    
let _ = print_string d
    
let dessinerPonts sol coord dir = 0

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




