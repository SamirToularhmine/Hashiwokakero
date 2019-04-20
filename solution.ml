open Puzzle
open Coordinate
open List

type bridge = { isVertical : bool; isDoubled : bool}
type cell = Nothing | Island of Puzzle.importance | Bridge of bridge
type solution = cell list list
type direction = Gauche | Haut | Droite | Bas

exception NotAnIsland
exception OutOfBounds
exception IslandMet
exception BridgeMet
exception UnlinkedCoords
                               
let string_of_bridge b =
  match (b.isDoubled),(b.isVertical) with
  | (false,false) -> "---------"
  | (false,true)  -> "[   |   ]"
  | (true,false)  -> "========="
  | (true,true)   -> "[  | |  ]"

let string_of_pair =
  function
  | (x,y) -> ("("^(string_of_int(x)) ^ "," ^ (string_of_int(y))^")")
           
let print_pair = fun pair -> print_string (string_of_pair pair)

let string_of_cell = function
  | Nothing -> "[       ]"
  | Island x -> ("[Ile - " ^ (string_of_int (int_of_importance x))) ^ "]"
  | (Bridge b) -> string_of_bridge b (*^ (string_of_bool b.isVertical) ^ " : " ^ (string_of_bool b.isDoubled)*)
                
let importance_of_island = function
  | Island imp -> imp
  | Nothing -> raise NotAnIsland
  | Bridge b -> raise NotAnIsland
                 
let int_of_island = function
  | Island imp -> int_of_importance imp
  | _ -> raise NotAnIsland

let rec toString = fun s ->
  match s with
  | [] -> ""
  | h::t ->
     let rec toStringLigne = fun l ->
       match l with
       | [] -> ""
       | h1::t1 ->
         match h1 with
         | cell -> (string_of_cell h1) ^ toStringLigne t1 in
     (toStringLigne h) ^ "\n" ^(toString t)
                                  
let oob sol =
  let mxR = (List.length sol)-1 in
  let mxC = if (mxR > 0) then (List.length (nth sol 0) -1)
            else failwith "La solution est vide !" in
  function
  | (x,y) -> (x > mxR) || (x < 0) || (y > mxC) || (y < 0)

let init_solution = fun p ->
  let liste = list_of_puzzle (Puzzle.sort p) in
  let maxCol = Puzzle.getMaxCol p in
  let maxRow = Puzzle.getMaxRow p in
  let rec creerSolution = fun l -> fun i -> fun j ->
    let rec iles =
      fun i -> fun l -> 
        match l with
        | [] -> []
        | h::t ->
           if Coordinate.fstcoord ((fst)h) = i then h::(iles i t)
           else (iles i t) in
    if i > maxRow then []
    else
      let rec creerLigne = fun l -> fun j ->
        match l with
        | [] ->
           if j <= maxCol then (Nothing)::(creerLigne l (j+1))
           else []
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

let sol1' =
  let bvs = Bridge {isVertical = true;isDoubled = false} in
  let bvd = Bridge {isVertical = true;isDoubled = true} in
  let bhs = Bridge {isVertical = false;isDoubled = false} in
  let bhd = Bridge {isVertical = false;isDoubled = true}
  in
  [
    [isl 4;bhd;isl 4;Nothing;Nothing];
    [bvd;Nothing;bvd;Nothing;Nothing];
    [isl 3;bhs;isl 5;bhs;isl 2];
    [Nothing;Nothing;bvs;Nothing;bvs];
    [Nothing;Nothing;isl 1;Nothing;isl 1]
  ]
(* let _ = print_string("SOL 1 = \n"^toString sol1'^"\n") *)
let coimp c n =
  let imp n = importance_of_int n in
  let cfp c = coord_from_pair c in
  (cfp c,imp n)
  
let puz1' =
  Puzzle.puzzle_of_list
    ([
        (coimp(4,2) 1);(coimp(2,4) 2);(coimp(2,0) 3);(coimp(0,2) 4);(coimp(0,0) 4);(coord_from_pair (2,2), importance_of_int 5);(coord_from_pair (4,4),importance_of_int 2); (coord_from_pair (2,0), importance_of_int 2)])         

  (*let _ = print_string("TESTNUL= \n"^toString(init_solution (puzzle_of_list[(coimp(0,0) 4);(coimp (2,2) 8);(coimp(4,2) 1);(coimp(2,4) 2);(coimp(2,0) 3);(coimp(0,2) 4)])))
    let _ =  print_string("SOL PUZ1PRIME = \n"^toString (init_solution puz1')^"\n")*)

  
let sol1 =
  let bvs = Bridge {isVertical = true;isDoubled = false} in
  let bvd = Bridge {isVertical = true;isDoubled = true} in
  let bhs = Bridge {isVertical = false;isDoubled = false} in
  let bhd = Bridge {isVertical = false;isDoubled = true}
  in
  [
    [isl 4;bhd;isl 5;Nothing;Nothing];
    [bvd;Nothing;bvs;Nothing;Nothing];
    [isl 3;bhs;isl 3;bhs;isl 1];
    [Nothing;Nothing;Nothing;Nothing;Nothing];
    [isl 4;Nothing;isl 8;Nothing;Island (importance_of_int 4)]
  ]      

(*let puz1 =
  puzzle_of_list (
    [
      (coord_from_pair (2,2), importance_of_int 4);
      (coord_from_pair (4,4),importance_of_int 2);
      (coord_from_pair (0,0), importance_of_int 2);
      (coord_from_pair (0,2), importance_of_int 2);
      (coord_from_pair (2,4), importance_of_int 2);
      (coord_from_pair (2,0), importance_of_int 3);
      
    ])*)

let puzzleTest2 = puzzle_of_list
    (
      [
        (coord_from_pair (0,0), importance_of_int 4);
        (coord_from_pair (0,3), importance_of_int 4);
        (coord_from_pair (0,6), importance_of_int 3);
        (coord_from_pair (2,1), importance_of_int 1);
        (coord_from_pair (2,3), importance_of_int 4);
        (coord_from_pair (2,5), importance_of_int 2);
        (coord_from_pair (3,0), importance_of_int 4);
        (coord_from_pair (3,6), importance_of_int 5);
        (coord_from_pair (5,0), importance_of_int 2);
        (coord_from_pair (5,5), importance_of_int 1);
        (coord_from_pair (6,2), importance_of_int 1);
        (coord_from_pair (6,4), importance_of_int 3);
        (coord_from_pair (6,6), importance_of_int 4)
      ]
    );;

let puzzleTest1 = puzzle_of_list
                    (
                      [
                        (coord_from_pair (0,2), importance_of_int 2);
                        (coord_from_pair (2,0), importance_of_int 3);
                        (coord_from_pair (2,2), importance_of_int 8);
                        (coord_from_pair (2,4), importance_of_int 4);
                        (coord_from_pair (4,0), importance_of_int 3);
                        (coord_from_pair (4,2), importance_of_int 5);
                        (coord_from_pair (4,4), importance_of_int 3);
                    ]);;

let puzzleTest3 = puzzle_of_list
                    (
                      [
                        (coord_from_pair (0,2), importance_of_int 1);
                        (coord_from_pair (0,4), importance_of_int 3);
                        (coord_from_pair (0,6), importance_of_int 1);
                        (coord_from_pair (1,0), importance_of_int 2);
                        (coord_from_pair (1,5), importance_of_int 1);
                        (coord_from_pair (2,2), importance_of_int 4);
                        (coord_from_pair (2,4), importance_of_int 5);
                        (coord_from_pair (3,0), importance_of_int 4);
                        (coord_from_pair (5,4), importance_of_int 1);
                        (coord_from_pair (6,0), importance_of_int 3);
                        (coord_from_pair (6,2), importance_of_int 3);
                        (coord_from_pair (6,5), importance_of_int 2)
                      ]
                    );;

let puzzleTest4 = puzzle_of_list
    (
      [
        (coord_from_pair (0,0), importance_of_int 2);
        (coord_from_pair (0,2), importance_of_int 3);
        (coord_from_pair (0,4), importance_of_int 1);
        (coord_from_pair (0,6), importance_of_int 1);
        (coord_from_pair (1,1), importance_of_int 2);
        (coord_from_pair (1,3), importance_of_int 1);
        (coord_from_pair (3,2), importance_of_int 1);
        (coord_from_pair (4,1), importance_of_int 3);
        (coord_from_pair (4,3), importance_of_int 5);
        (coord_from_pair (4,6), importance_of_int 2);
        (coord_from_pair (6,0), importance_of_int 2);
        (coord_from_pair (6,3), importance_of_int 4);
        (coord_from_pair (6,5), importance_of_int 1)
      ]
    );;

let puzzleTest5 = puzzle_of_list
    (
      [
        (coord_from_pair (0,1), importance_of_int 2);
        (coord_from_pair (0,3), importance_of_int 6);
        (coord_from_pair (0,6), importance_of_int 3);
        (coord_from_pair (2,0), importance_of_int 1);
        (coord_from_pair (2,3), importance_of_int 6);
        (coord_from_pair (2,5), importance_of_int 2);
        (coord_from_pair (3,4), importance_of_int 1);
        (coord_from_pair (3,6), importance_of_int 3);
        (coord_from_pair (4,0), importance_of_int 1);
        (coord_from_pair (5,4), importance_of_int 1);
        (coord_from_pair (5,6), importance_of_int 2);
        (coord_from_pair (6,0), importance_of_int 3);
        (coord_from_pair (6,3), importance_of_int 5);
        (coord_from_pair (6,5), importance_of_int 2)
      ]
    );;

let puzzleTest6 = puzzle_of_list
    (
      [
        (coord_from_pair (0,0), importance_of_int 4);
        (coord_from_pair (0,2), importance_of_int 4);
        (coord_from_pair (0,5), importance_of_int 2);
        (coord_from_pair (0,8), importance_of_int 3);
        (coord_from_pair (2,0), importance_of_int 6);
        (coord_from_pair (2,2), importance_of_int 8);
        (coord_from_pair (2,4), importance_of_int 4);
        (coord_from_pair (2,7), importance_of_int 1);
        (coord_from_pair (3,6), importance_of_int 1);
        (coord_from_pair (3,8), importance_of_int 3);
        (coord_from_pair (4,2), importance_of_int 2);
        (coord_from_pair (4,4), importance_of_int 2);
        (coord_from_pair (4,7), importance_of_int 1);
        (coord_from_pair (5,0), importance_of_int 4);
        (coord_from_pair (5,3), importance_of_int 3);
        (coord_from_pair (5,5), importance_of_int 2);
        (coord_from_pair (6,6), importance_of_int 2);
        (coord_from_pair (6,8), importance_of_int 3);
        (coord_from_pair (7,1), importance_of_int 1);
        (coord_from_pair (7,3), importance_of_int 5);
        (coord_from_pair (7,5), importance_of_int 4);
        (coord_from_pair (8,0), importance_of_int 3);
        (coord_from_pair (8,2), importance_of_int 3);
        (coord_from_pair (8,4), importance_of_int 2);
        (coord_from_pair (8,6), importance_of_int 3);
        (coord_from_pair (8,8), importance_of_int 2)
      ]
    );;


let getCell sol = function | (x,y) -> if(oob sol (x,y))then failwith"OULAH" else nth (nth sol x) y
                                        
let c = coord_from_pair (1,1)
                                              
let msgFinDebug = ("\n ---------FIN DEBUG----------- \n")

let next_pair dir pair = 
  match (dir,pair) with
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
  let getIndex l l' = ((length l) - (length l')) in
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
          aux2 t2 c v (res2@[if (c = (i,j)) then v else h]) in
      aux1 t1 c v ((aux2 l2 c v [])::res1) in
  List.rev (aux1 l c v [])
        
let dessinerPonts sol pair dir =
  let bvs = Bridge {isVertical = true; isDoubled = false} in
  let bvd = Bridge {isVertical = true; isDoubled = true} in
  let bhs = Bridge {isVertical = false; isDoubled = false} in
  let bhd = Bridge {isVertical = false; isDoubled = true} in
  let nextPair = next_pair dir pair in
  let rec aux pair dir res =
    let nextPair = next_pair dir pair in
    if (oob sol pair) then res
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
  | BridgeMet -> failwith "Problème pont rencontré"
                  
let sol2 = dessinerPonts sol1 (2,4) Gauche
         
(* puz1here *)
let nombre_de_pont sol pair =
  let aux dir =
    let bon_sens =
    function
    | Bridge {isVertical = true; isDoubled = _ },(Haut|Bas) -> true
    | Bridge {isVertical = false; isDoubled = _ },(Gauche|Droite) -> true
    | _,_ -> false in
    
    let pair' = next_pair dir pair in
    if(oob sol pair') then 0
    else
      let cell = getCell sol pair' in
      match cell with
      |(Bridge {isVertical = _; isDoubled = double}) as bridge -> if (bon_sens (bridge,dir)) then (if (double) then 2 else 1) else 0
      | _ -> 0                                             
  in (aux Gauche)+(aux Haut)+(aux Droite)+(aux Bas)

let count_total_ponts = fun cell -> fun sol ->
  let haut = getCell sol (fstcoord c - 1, sndcoord c) in
  let bas = getCell sol (fstcoord c + 1, sndcoord c) in
  let gauche = getCell sol (fstcoord c, sndcoord c - 1) in
  let droite = getCell sol (fstcoord c, sndcoord c + 1) in
  let countPonts = fun c ->
    match c with
    | Bridge {isVertical = _; isDoubled = b} -> if b then 10 else 1
    | Nothing -> 100
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
  let totalPont = nombre_de_pont sol (pair_from_coord c) in 
  if totalPont < importance then false
  else if importance = totalPont then true
  else failwith "Trop de ponts !"

let ponts_restants = fun c -> fun sol ->
  let cell = getCell sol (fstcoord c, sndcoord c) in
  let importance =
    match cell with
    | Island a -> int_of_importance a
    | _ -> failwith "Pas une ile !" in
  let total_ponts = nombre_de_pont sol (pair_from_coord c) in
  importance - total_ponts


  
let get_voisins_pont sol pair =
  let bon_sens =
    function
    | Bridge {isVertical = true; isDoubled = _ },(Haut|Bas) -> true
    | Bridge {isVertical = false; isDoubled = _ },(Gauche|Droite) -> true
    | _,_ -> false in
  
  let rec get_first_island pair dir =
    let nextPair = next_pair dir pair in
    if (oob sol pair) then []
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


  (* parcours largeur qui retourne une liste des sommets par lesquels il est passé *)
let parcours_largeur_pont sol pair =
  let rec aux pair file res =
    let voisins_pont = get_voisins_pont sol pair in
    let voisins_non_atteint = List.filter (fun x -> (not(List.mem x res ))) voisins_pont in
    let file' = (function |[] -> [] |h::t->t) (file@voisins_non_atteint) in
    match file' with
    |[] -> res
    |h::t -> aux h (file') (res@voisins_non_atteint)
  in aux pair [pair] [pair]

  
(* la liste de paire donnée en paramètre doit être issue d'un parcours en largeur*)    
let test_est_composante_connexe liste sol =
  (List.length (List.filter (fun x -> est_complet (coord_from_pair x) sol) liste)) = (List.length liste)

let jeu_est_fini sol puz =
  let puz' = list_of_puzzle puz in 
  let liste_finale = 
    let first_pair = pair_from_coord(fst ((function |[]->failwith"puzzle vide"|h::t->h) puz')) in

    parcours_largeur_pont sol first_pair in
  ((List.length liste_finale) = (List.length puz')) && (test_est_composante_connexe liste_finale sol)

  
let get_voisins sol pair puz =
  
  let bon_sens_pas_double =
    function
    | Bridge {isVertical = true; isDoubled = false },(Haut|Bas) -> true
    | Bridge {isVertical = false; isDoubled = false },(Gauche|Droite) -> true
    | _,_ -> false in
  
  let rec get_first_island pair' dir =
    
    let nextPair = next_pair dir pair' in
    if (oob sol pair') then []
    else
      let current_cell = getCell sol pair' in

    match current_cell with
    |Nothing -> get_first_island nextPair dir
    |(Bridge b') as b -> if (bon_sens_pas_double (b,dir)) then (get_first_island nextPair dir) else []
    |(Island imp) ->if (est_complet (coord_from_pair pair') sol) then [] else
                      if(est_complet (coord_from_pair pair) sol) then [] else
      let compo_connexe =
        let sol_sim = dessinerPonts sol pair dir in
        let liste_cw = parcours_largeur_pont sol_sim pair' in
        
        (test_est_composante_connexe liste_cw sol_sim) && (not(jeu_est_fini sol_sim puz))  

      in if (compo_connexe) then [] else [pair']
    
  in

  (get_first_island (next_pair Gauche pair) Gauche)@
    (get_first_island (next_pair Haut pair) Haut)@
      (get_first_island (next_pair Droite pair) Droite)@
  (get_first_island (next_pair Bas pair) Bas)


  
let string_of_list string_of liste = (List.fold_right (fun x y-> ("[")^(string_of x)^"]"^y) (liste) "")

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
      let voisins = get_voisins res cell_pos puzzle in
      let nb_voisins = List.length voisins in
      let importance = int_of_importance (snd h) in
      let ponts_min = fun (x,y) ->
        if x mod 2 = 0 then
          if x/2 = y then x
          else if x/2 > y  then 1
          else 0
        else
        if x = 1 && y = 1 then 1
          else
        if x <= y then 0 else
          int_of_float (ceil (float_of_int (importance) /. 2.0)) in
      let rec completer_voisins = fun v -> fun res ->
        match v with
        | [] -> res
        | h::t ->
          let pontMIN = ponts_min((ponts_restants (coord_from_pair cell_pos) res), nb_voisins) in
          if (est_complet (coord_from_pair cell_pos) res) || (est_complet (coord_from_pair h) res) then
            completer_voisins t res
          else
            let ile_voisine = getCell res h in
            if importance >= (int_of_island ile_voisine) && List.length (get_voisins res h puzzle) = 1 then
              completer_voisins t (dessinerPonts res cell_pos (dir_to_coord cell_pos h))
            else
            if pontMIN > 0 then
              (
                if pontMIN = importance then
                  (
                    let rec pontsrestants_ok = fun res ->
                      
                      if ponts_restants (coord_from_pair cell_pos) res > 0 then
                        let rec remplir_voisin = fun i -> fun v -> fun res -> 
                          if i = 0 then res
                          else
                            match v with
                            | [] -> res
                            | h::t -> remplir_voisin (i-1) t (dessinerPonts res cell_pos (dir_to_coord cell_pos h))
                        in remplir_voisin pontMIN v res
                      else
                        res
                    in pontsrestants_ok res
                  )
                  
                else 
                  
                  (*completer_voisins t (dessinerPonts res cell_pos (dir_to_coord cell_pos h))*)
                  dessinerPonts res cell_pos (dir_to_coord cell_pos h)
                
              )
            else
              completer_voisins t res in
      aux t (completer_voisins voisins res) in
let rec apply = fun i -> fun res ->
    if i = 0 then res
    else apply (i-1) (aux puzzle_l res) in
  apply 1 solution_vide;;
(*let rec apply = fun stop -> fun res ->
    if stop then res
    else
      apply (jeu_est_fini res puzzle) (aux puzzle_l res) in
  apply (jeu_est_fini solution_vide puzzle) solution_vide;;*)


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
             
(* let debugPont = msgDebug^msgFinDebug *)
let puztest = puzzleTest6
let soltest = init_solution puztest
let _ = print_string ((string_of_bool (jeu_est_fini soltest puztest))^"\n")
let solvetest = solve puztest
      
let msgDebug = "\n"^(string_of_list (string_of_pair ) (parcours_largeur_pont soltest (0,0)))^"\n"^(toString soltest)^"\n"^(toString solvetest)
             
let debugPont = msgDebug^"\n"

(* let _ = print_string ("si c'est True ça veut dire que solve marche, jeu_est_fini ? :"^(string_of_bool (jeu_est_fini (solve puzzleTest3) puzzleTest3))^"\n") *)
let main = fun unit ->
  (*let _ = print_string (toString (solve puzzleTest1)); in*)
  let solution = solve (puzzleTest2) in
  Graphics.open_graph "";
  Graphics.resize_window 500 500;
  Graphics.set_color(Graphics.black);
  Graphics.draw_rect 0 0 500 500;
  Graphics.fill_rect 0 0 500 500;
  Graphics.set_window_title "Hashiwo Kakero";
  Graphics.rmoveto 200 450;
  Graphics.set_color(Graphics.white);
  Graphics.draw_string "Hashiwo Kakero !";
  Graphics.set_text_size 2;
  let rec displaySol = fun sol -> fun i ->
    match sol with
  | [] -> ""
  | h::t ->
    let rec displayLine = fun line -> fun j ->
      match line with
      | [] -> displaySol t (i+1)
      | h1::t1 ->
        match h1 with
        | Nothing ->
          Graphics.moveto (50 + j * 60) (350 - i * 50);
          displayLine t1 (j+1) 
        | Island island ->
          (*
          Graphics.draw_circle (50 +j * 60) (350 - i * 50) 20;
          Graphics.moveto (50 + j * 60 - 2) (350 - i * 50 - 5);
          Graphics.draw_string (string_of_int (int_of_importance island));
          Graphics.moveto (50 + j * 60) (350 - i * 50);
          displayLine t1 (j+1)*)
          if ponts_restants (coord_from_pair (i,j) ) sol = 0 then
            (
              Graphics.draw_circle (50 +j * 60) (350 - i * 50) 20;
              Graphics.moveto (50 + j * 60 - 2) (350 - i * 50 - 5);
              Graphics.draw_string (string_of_int (int_of_importance island));
              Graphics.moveto (50 + j * 60) (350 - i * 50);
              displayLine t1 (j+1)
            )
          else
            (
              Graphics.set_color Graphics.red;
              Graphics.draw_circle (50 +j * 60) (350 - i * 50) 20;
              Graphics.moveto (50 + j * 60 - 2) (350 - i * 50 - 5);
              Graphics.draw_string (string_of_int (int_of_importance island));
              Graphics.moveto (50 + j * 60) (350 - i * 50);
              Graphics.set_color Graphics.white;
              displayLine t1 (j+1)
            )
        | Bridge { isVertical = iv; isDoubled = id } ->
          if iv then
            (
              if id then
                (
                  Graphics.set_color Graphics.green;
                  Graphics.moveto (50 + j * 60 - 5) (350 - i * 50 + 30);
                  Graphics.lineto (50 + j * 60 - 5) (350 - i * 50 - 30);

                  Graphics.moveto (50 + j * 60 + 5) (350 - i * 50 + 30);
                  Graphics.lineto (50 + j * 60 + 5) (350 - i * 50 - 30);
                  Graphics.set_color Graphics.white;displayLine t1 (j+1)
                )
              else
                (
                  Graphics.set_color Graphics.yellow;
                  Graphics.moveto (50 + j * 60) (350 - i * 50 + 30);
                  Graphics.lineto (50 + j * 60) (350 - i * 50 - 30);
                  Graphics.set_color Graphics.white;displayLine t1 (j+1)
                )
            )
              (*if iv then (Graphics.lineto (350 - j * 60) (350 - i * 50); displayLine t1 (j+1))
                else (Graphics.lineto (350 - j * 60) (350 - i * 50);*)
          else
            (
              if id then
                (
                  Graphics.set_color Graphics.green;
                  Graphics.moveto (50 + j * 60 + 40) (350 - i * 50 + 5);
                  Graphics.lineto (50 + j * 60 - 40) (350 - i * 50 + 5);
                  Graphics.moveto (50 + j * 60 + 40) (350 - i * 50 - 5);
                  Graphics.lineto (50 + j * 60 - 40) (350 - i * 50 - 5);
                  Graphics.set_color Graphics.white;
                  
                  displayLine t1 (j+1)
                )
              else
                (
                  Graphics.set_color Graphics.red;
                  Graphics.moveto (50 + j * 60 + 40) (350 - i * 50);
                  Graphics.lineto (50 + j * 60 - 40) (350 - i * 50);
                  Graphics.set_color Graphics.white;
                  displayLine t1 (j+1) 
                )
            ) in 
    displayLine h 0 in
  displaySol solution 0;
  let rec loop = fun b ->
    (
      if Graphics.read_key () = 'a' then
        (
          Graphics.clear_graph();
          Graphics.set_color Graphics.black;
          Graphics.draw_rect 0 0 500 500;
          Graphics.fill_rect 0 0 500 500;
          Graphics.set_color Graphics.white;
           displaySol (solve puzzleTest1) 0; loop b
        )
      else
        (
          if Graphics.read_key () = 'z' then
            (
              Graphics.clear_graph();
              Graphics.set_color Graphics.black;
              Graphics.draw_rect 0 0 500 500;
              Graphics.fill_rect 0 0 500 500;
              Graphics.set_color Graphics.white;
              displaySol (solve puzzleTest2) 0; loop b
            )
          else loop b
        )
    ) in
  loop ();;

(*main();;*)

