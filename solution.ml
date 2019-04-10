open Puzzle;;
open Coordinate;;
open List;;
type bridge = { isVertical : bool; isDoubled : bool}
type cell = Nothing | Island of Puzzle.importance | Bridge of bridge
type solution = cell list list
let string_of_bridge b =
  match (b.isDoubled),(b.isVertical) with
  | (false,false) -> "--------"
  | (false,true)  -> "|**II**|"
  | (true,false)  -> "========"
  | (true,true)   -> "|*IIII*|"                 
let string_of_cell = function
  | Nothing -> "|******|"
  | Island x -> ("Île - " ^ (string_of_int (int_of_importance x))) ^ " "
  | (Bridge b) -> string_of_bridge b (*^ (string_of_bool b.isVertical) ^ " : " ^ (string_of_bool b.isDoubled)*)

let importance_of_island = function | Island imp -> imp
                                    | Nothing -> failwith"pas d'importance sur Nothing"
                                    |Bridge b -> failwith"pas d'importance sur Bridge"
let int_of_island = function | Island imp -> int_of_importance imp
                             | _ -> failwith" pas de int sur Nothing ou Bridge"

let rec toString = fun s ->
  match s with
  | [] -> ""
  | h::t ->
     let rec toStringLigne = fun l ->
       match l with
       | [] -> ""
       | h1::t1 ->
          match h1 with
          | cell -> (string_of_cell h1) ^ toStringLigne t1
                  
     in (toStringLigne h) ^ "\n" ^(toString t)





let initSolution =
  fun p ->
  let liste = Puzzle.list_of_puzzle p in
  let maxCol = Puzzle.getMaxCol p in
  let maxRow = Puzzle.getMaxRow p in
  let rec creerSolution =
    fun l ->
    fun i ->
    fun j ->
    let rec iles =
      fun i -> fun l -> 
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
   
let test = Island (importance_of_int 3)
let bv = Bridge {isVertical = true;isDoubled = false}
let isl n = Island (importance_of_int n)
let sol1 =
  let bvs = Bridge {isVertical = true;isDoubled = false} in
  let bvd = Bridge {isVertical = true;isDoubled = true} in
  let bhs = Bridge {isVertical = false;isDoubled = false} in
  let bhd = Bridge {isVertical = false;isDoubled = true} in[
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;isl 8;Nothing;bhs;isl 1];
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [Nothing;Nothing;Nothing;Nothing;Island (importance_of_int 4)]
  ]

let puz1 =
  Puzzle.puzzle_of_list ([(coord_from_pair (2,2), importance_of_int 4);(coord_from_pair (4,4),importance_of_int 4)])         
type direction = Gauche | Haut | Droite | Bas
                                        
let c = coord_from_pair (1,1)
      
let getCell sol = function | (x,y) -> nth (nth sol x) y

let msgFinDebug =("\n ---------FIN DEBUG----------- \n")


let replace sol pair cell =
  let getIndex l l' = ((length l) - (length l'))in
  let l = sol in
  let c = pair in
  let v = cell in
  let getInl1 = (getIndex l) in
  let rec aux1 l1 c v res1 =
    match l1 with
    |[]->res1
    |l2::t1 ->
      let i = (getInl1 l1) 
      in
      let getInl2 = (getIndex l2)
      in
      let rec aux2 l2 c v res2 =
        match l2 with
        |[] -> res2
        |(h::t2) ->let j = (getInl2 l2) in aux2 t2 c v (res2@[if (c=(i,j)) then v else h])
      in aux1 t1 c v ((aux2 l2 c v [])::res1)
  in List.rev (aux1 l c v [])

   
let oob puz =
  let mxC = (Puzzle.getMaxCol puz)
  in let mxR = (Puzzle.getMaxRow puz)
     in function | (x,y) ->
                    (x > mxR) || (x < 0) || (y > mxC) || (y < 0);;

let string_of_pair = function | (x,y) -> ((string_of_int(x))^","^(string_of_int(y)))
exception OutOfBounds
exception IslandMet
exception BridgeMet
        
let dessinerPonts sol pair dir =
  let bvs = Bridge {isVertical = true;isDoubled = false} in
  let bvd = Bridge {isVertical = true;isDoubled = true} in
  let bhs = Bridge {isVertical = false;isDoubled = false} in
  let bhd = Bridge {isVertical = false;isDoubled = true} in
  let nextPair =  
    match dir,pair with
    | Gauche,(x,y) -> (x,y-1)
    | Haut,(x,y) -> (x-1,y)
    | Droite,(x,y) -> (x,y+1)
    | Bas,(x,y) -> (x+1,y)
  in
  let rec aux pair dir res =
    let nextPair =  
      match dir,pair with
      | Gauche,(x,y) -> (x,y-1)
      | Haut,(x,y) -> (x-1,y)
      | Droite,(x,y) -> (x,y+1)
      | Bas,(x,y) -> (x+1,y)
    in
    if (oob puz1 pair) then res
    else
      let cell = getCell res pair in
      
      let actual_bridge =
        match dir with
        |Haut|Bas -> bvs
        |Gauche|Droite -> bhs
      in match cell,actual_bridge with
         |Nothing,Bridge {isVertical = y;isDoubled = x} ->
           (aux nextPair dir (replace res pair (Bridge {isVertical = y;isDoubled = false })))
         | Bridge {isVertical = y; isDoubled = x} as b, Bridge {isVertical = y1; isDoubled = x1} ->
            (match b,actual_bridge with
             | Bridge {isVertical = x;isDoubled = y}, Bridge {isVertical = x';isDoubled = y'} when (x=x' && y <> true) ->
                aux nextPair dir (replace res pair (Bridge {isVertical = x;isDoubled = true }))
             | c1,c2 -> (print_string((string_of_pair(pair))^string_of_pair(nextPair)^"\n");res))
         | Island imp,_ -> (print_int(int_of_importance imp);res)
         | _,_ -> failwith"je pensais pas en arriver là :("
                
  in try aux nextPair dir sol with
     | OutOfBounds -> failwith"OOB"
     | IslandMet -> failwith"IslandMet"
     | BridgeMet -> (print_string "Probleme bridge rencontré\n";sol)
                  

let sol2 = dessinerPonts sol1 (2,4) Gauche
         
let msgDebug = toString (sol2) 
             
let debugPont = msgDebug^msgFinDebug


                           



      

let count_total_ponts =
  fun cell -> fun sol ->
           let haut = getCell sol (fstcoord c - 1, sndcoord c) in
           let bas = getCell sol (fstcoord c + 1, sndcoord c) in
           let gauche = getCell sol (fstcoord c, sndcoord c - 1) in
           let droite = getCell sol (fstcoord c, sndcoord c + 1) in
           let countPonts = fun c ->
             match c with
             | Bridge {isVertical = _; isDoubled = b} -> if b then 2 else 1
             | Nothing -> 0
             | _ -> failwith "pas un pont" in
           let nbHaut = countPonts haut in
           let nbBas = countPonts bas in
           let nbGauche = countPonts gauche in
           let nbDroite = countPonts droite in
           nbHaut + nbBas + nbGauche + nbDroite
            
let est_complet =
  fun c ->
  fun sol ->
   let cell = getCell sol (fstcoord c, sndcoord c) in
  let importance =
    match cell with
    | Island a -> int_of_importance a
    | _ -> failwith "Pas une ile !" in
  let totalPont = count_total_ponts c sol in 
  if totalPont < importance then false else if importance = totalPont then true else failwith "Trop de ponts !"

let ponts_restants =
  fun c -> fun sol ->
           let cell = getCell sol (fstcoord c, sndcoord c) in
           let importance =
             match cell with
             | Island a -> int_of_importance a
             | _ -> failwith "Pas une ile !" in
           let total_ponts = count_total_ponts c sol in importance - total_ponts
                    






