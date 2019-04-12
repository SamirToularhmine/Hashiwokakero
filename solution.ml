open Puzzle;;
open Coordinate;;
open List;;
type bridge = { isVertical : bool; isDoubled : bool}
type cell = Nothing | Island of Puzzle.importance | Bridge of bridge
type solution = cell list list
let string_of_bridge b =
  match (b.isDoubled),(b.isVertical) with
  | (false,false) -> "---------"
  | (false,true)  -> "[   |   ]"
  | (true,false)  -> "========="
  | (true,true)   -> "[  | |  ]"
    
let string_of_cell = function
  | Nothing -> "[       ]"
  | Island x -> ("[Île - " ^ (string_of_int (int_of_importance x))) ^ "]"
  | (Bridge b) -> string_of_bridge b (*^ (string_of_bool b.isVertical) ^ " : " ^ (string_of_bool b.isDoubled)*)


let importance_of_island = function
  | Island imp -> imp
  | Nothing -> failwith"pas d'importance sur Nothing"
  |Bridge b -> failwith"pas d'importance sur Bridge"
                 
let int_of_island = function
  | Island imp -> int_of_importance imp
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
                                  
let oob puz =
  let mxC = (Puzzle.getMaxCol puz) in
  let mxR = (Puzzle.getMaxRow puz) in
  function
  | (x,y) -> (x >mxR) || (x < 0) || (y > mxC) || (y < 0);;


let init_solution = fun p ->
  let liste = Puzzle.list_of_puzzle p in
  let maxCol = Puzzle.getMaxCol p in
  let maxRow = Puzzle.getMaxRow p in
  let rec creerSolution = fun l -> fun i -> fun j ->
    let rec iles =
      fun i -> fun l -> 
        match l with
        | [] -> []
        | h::t ->
          if Coordinate.fstcoord ((fst)h) = i then h::(iles i t) else (iles i t) in
    if i > maxRow then [] else
      let rec creerLigne = fun l -> fun j ->
        match l with
        | [] ->
          if j <= maxCol then (Nothing)::(creerLigne l (j+1)) else []
        | h::t ->
          let coord = fst h in
          let importance = snd h in
          if (Coordinate.sndcoord coord > j) then (Nothing)::(creerLigne (h::t) (j+1))  
          else if j > maxCol then []
          else if (Coordinate.sndcoord coord = j) then (Island importance)::(creerLigne t (j+1))
          else (Nothing)::(creerLigne t (j+1))
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
    [isl 4;bhd;isl 5;Nothing;Nothing];
    [bvd;Nothing;bvs;Nothing;Nothing];
    [isl 3;bhs;isl 3;bhs;isl 1];
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [isl 4;Nothing;isl 8;Nothing;Island (importance_of_int 4)]
  ]      

let puz1 =
  Puzzle.puzzle_of_list ([(coord_from_pair (0,0), importance_of_int 2);(coord_from_pair (2,2), importance_of_int 4); (coord_from_pair (0,2), importance_of_int 2); (coord_from_pair (2,4), importance_of_int 2);(coord_from_pair (4,4),importance_of_int 2)])

type direction = Gauche | Haut | Droite | Bas
                                        
let c = coord_from_pair (1,1)
      
let getCell sol = function | (x,y) -> if(oob puz1 (x,y))then failwith"OULAH" else nth (nth sol x) y
                                        
let msgFinDebug = ("\n ---------FIN DEBUG----------- \n")

let next_pair dir pair = 
  match dir,pair with
  | Gauche,(x,y) -> (x,y-1)
  | Haut,(x,y) -> (x-1,y)
  | Droite,(x,y) -> (x,y+1)
  | Bas,(x,y) -> (x+1,y)
                 
let string_of_direction = fun d ->
  match d with
  | Haut -> "Haut"
  | Bas -> "Bas"
  | Gauche -> "Gauche"
  | Droite -> "Droite"
    
let replace sol pair cell =
  let getIndex l l' = ((length l) - (length l'))in
  let l = sol in
  let c = pair in
  let v = cell in
  let getInl1 = (getIndex l) in
  let rec aux1 l1 c v res1 =
    match l1 with
    | [] -> res1
    | l2::t1 ->
      let i = (getInl1 l1) in
      let getInl2 = (getIndex l2) in
      let rec aux2 l2 c v res2 =
        match l2 with
        | [] -> res2
        | h::t2 ->
          let j = (getInl2 l2) in
          aux2 t2 c v (res2@[if (c=(i,j)) then v else h]) in
      aux1 t1 c v ((aux2 l2 c v [])::res1) in
  List.rev (aux1 l c v [])


let string_of_pair = function
  | (x,y) -> ("("^(string_of_int(x)) ^ "," ^ (string_of_int(y))^")")
let print_pair pair= print_string (string_of_pair pair)
    
exception OutOfBounds
exception IslandMet
exception BridgeMet
        
let dessinerPonts sol pair dir =
  let bvs = Bridge {isVertical = true; isDoubled = false} in
  let bvd = Bridge {isVertical = true; isDoubled = true} in
  let bhs = Bridge {isVertical = false; isDoubled = false} in
  let bhd = Bridge {isVertical = false; isDoubled = true} in
  let nextPair = next_pair dir pair in
  let rec aux pair dir res =
    let nextPair = next_pair dir pair in
    if (oob puz1 pair) then res
    else
      let cell = getCell res pair in
      let actual_bridge =
        match dir with
        | Haut | Bas -> bvs
        | Gauche | Droite -> bhs in
      match cell,actual_bridge with
      | Nothing,Bridge {isVertical = y;isDoubled = x} ->
        (aux nextPair dir (replace res pair (Bridge {isVertical = y;isDoubled = false })))     
      | Bridge {isVertical = y; isDoubled = x} as b, Bridge {isVertical = y1; isDoubled = x1} ->
        (
          match b,actual_bridge with
          | Bridge {isVertical = x;isDoubled = y}, Bridge {isVertical = x';isDoubled = y'} when (x=x' && y <> true) ->
            aux nextPair dir (replace res pair (Bridge {isVertical = x;isDoubled = true }))
          | c1,c2 -> (print_string((string_of_pair(pair))^string_of_pair(nextPair)^"HEHO\n");res)
        )
        
      | Island imp,_ -> (res)
      | _,_ -> failwith"je pensais pas en arriver là :("
                 
  in try aux nextPair dir sol with
  | BridgeMet -> failwith"Probleme bridge rencontré"
                  
                  
let sol2 = dessinerPonts sol1 (2,4) Gauche   

let count_total_ponts = fun cell -> fun sol ->
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
  
let est_complet = fun c -> fun sol ->
  let cell = getCell sol (fstcoord c, sndcoord c) in
  let importance =
    match cell with
    | Island a -> int_of_importance a
    | _ -> failwith "Pas une ile !" in
  let totalPont = count_total_ponts c sol in 
  if totalPont < importance then false
  else if importance = totalPont then true
  else failwith "Trop de ponts !"

let ponts_restants = fun c -> fun sol ->
  let cell = getCell sol (fstcoord c, sndcoord c) in
  let importance =
    match cell with
    | Island a -> int_of_importance a
    | _ -> failwith "Pas une ile !" in
  let total_ponts = count_total_ponts c sol in
  importance - total_ponts
  

let get_voisins sol pair =
  let bon_sens_pas_double =
    function
    | Bridge {isVertical = true; isDoubled = false },(Haut|Bas) -> true
    | Bridge {isVertical = false; isDoubled = false },(Gauche|Droite) -> true
    | _,_ -> false in
  
  let rec get_first_island pair dir =
    let nextPair = next_pair dir pair in
    if (oob puz1 pair) then []
    else
      let current_cell = getCell sol pair  in

    match current_cell with
    |Nothing -> get_first_island nextPair dir
    |(Bridge b') as b -> if (bon_sens_pas_double (b,dir)) then (get_first_island nextPair dir) else []
    |(Island imp) -> if (est_complet (coord_from_pair pair) sol) then [] else [pair]
    (*est_complet (coord_from_pair pair) sol*)
  in

  (get_first_island (next_pair Gauche pair) Gauche)@
    (get_first_island (next_pair Haut pair) Haut)@
      (get_first_island (next_pair Droite pair) Droite)@
  (get_first_island (next_pair Bas pair) Bas)

let get_voisins_pont sol pair =
  let bon_sens =
    function
    | Bridge {isVertical = true; isDoubled = _ },(Haut|Bas) -> true
    | Bridge {isVertical = false; isDoubled = _ },(Gauche|Droite) -> true
    | _,_ -> false in
  
  let rec get_first_island pair dir =
    let nextPair = next_pair dir pair in
    if (oob puz1 pair) then []
    else
      let current_cell = getCell sol pair  in

    match current_cell with
    |Nothing ->  []
    |(Bridge b') as b -> if (bon_sens (b,dir)) then (get_first_island nextPair dir) else []
    |(Island imp)-> [pair]
    
  in

  (get_first_island (next_pair Gauche pair) Gauche)@
    (get_first_island (next_pair Haut pair) Haut)@
      (get_first_island (next_pair Droite pair) Droite)@
  (get_first_island (next_pair Bas pair) Bas)
  
let string_of_list string_of liste = (List.fold_right (fun x y-> ("[")^(string_of x)^"]"^y) (liste) "")

exception UnlinkedCoords

let dir_to_coord = fun c1 -> fun c2 ->
  match c1,c2 with
  | (i1,j1),(i2,j2) ->
    if i1 = i2 || j1 = j2 then
      if i1 = i2 then
        if j1 < j2 then Droite else Gauche
      else
        if i1 < i2 then Bas else Haut
    else raise UnlinkedCoords;;

let solve = fun puzzle ->
  let solution_vide = init_solution puzzle in
  let puzzle_l = list_of_puzzle puzzle in
  let rec aux = fun p -> fun res ->
    match p with
    | [] -> res
    | h::t ->
      let cell_pos = pair_from_coord (fst h) in
      let voisins = get_voisins res cell_pos in
      let rec completer_voisins = fun v -> fun res ->
        match v with
        | [] -> res
        | h::t ->
          if (est_complet (coord_from_pair cell_pos) res) || (est_complet (coord_from_pair cell_pos) res) then completer_voisins t res
          else completer_voisins t (dessinerPonts res cell_pos (dir_to_coord cell_pos h)) in
      aux t (completer_voisins voisins res) in
  aux puzzle_l solution_vide;;
  (*let rec iter = fun sol -> fun i ->
    match sol with
    | [] -> []
    | h::t ->
      let rec iter_ligne = fun ligne -> fun j ->
        match ligne with
        | [] -> []
        | h1::t1 ->
          match h1 with
          | Island a ->
            let voisins = get_voisins solution_vide (i,j) in
            h1::iter_ligne t1 (j+1)
          | Bridge _ -> h1::iter_ligne t1 (j+1)
          | Nothing -> h1::iter_ligne t1 (j+1) in
      (iter_ligne h 0)::iter t (i+1) in
  iter solution_vide 0;;*)

print_int (count_total_ponts (0,0) (solve puz1));
print_string (string_of_bool (est_complet (coord_from_pair (2,4)) (solve puz1)));
print_string (toString (solve puz1))

(* parcours largeur qui retourne une liste des sommets par lesquels il est passé *)
let parcours_largeur_pont sol pair =
  let rec aux pair file res =
    let voisins_pont = get_voisins_pont sol pair in
    let voisins_non_atteint = List.filter (fun x -> (not(List.mem x res ))) voisins_pont in
    let file' = (function |[] -> [] |h::t->t) (file@voisins_non_atteint) in
    match file' with
    |[] -> res
    |h::t -> aux h (print_string("H = "^(string_of_pair h)^" : "^(string_of_list string_of_pair (t@voisins_non_atteint))^"\n");(file')) (res@voisins_non_atteint)
  in aux pair [pair] [pair]

let sol3 = replace sol1 (1,2) (isl 6)
let msgDebug = "\n"^(toString sol3)^"\n"^(string_of_list (string_of_pair ) (parcours_largeur_pont sol1 (0,0)))^"\n"^(toString sol1) 
             
(*let debugPont = msgDebug^msgFinDebug*)

let debugPont = ""
