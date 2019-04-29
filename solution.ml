
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

let getCell sol = function | (x,y) -> if(oob sol (x,y))then failwith"OULAH" else nth (nth sol x) y
      
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
  let bhs = Bridge {isVertical = false; isDoubled = false} in
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
          | c1,c2 -> res
        )
        
      | Island imp,_ -> (res)
      | _,_ -> failwith"je pensais pas en arriver là :("
                 
  in try aux nextPair dir sol with
  | BridgeMet -> raise BridgeMet
         
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

  
let est_complet = fun c -> fun sol ->
  let cell = getCell sol (fstcoord c, sndcoord c) in
  let importance =
    match cell with
    | Island a -> int_of_importance a
    | _ -> failwith ("Pas une ile !ec" ^ (string_of_pair (pair_from_coord c))) in
  let totalPont = nombre_de_pont sol (pair_from_coord c) in 
  if totalPont < importance then false
  else if importance = totalPont then true
  else failwith ("Trop de ponts ! sur la cell : " ^ (string_of_pair (pair_from_coord c)))

let ponts_restants = fun c -> fun sol ->
  let cell = getCell sol (fstcoord c, sndcoord c) in
  let importance =
    match cell with
    | Island a -> int_of_importance a
    | _ -> failwith "Pas une ile ! pr" in
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

let parcours_largeur sol pair =
  let rec aux pair file res =
    let voisins_pont = get_voisins_pont sol pair in
    let voisins_non_atteint = List.filter (fun x -> (not(List.mem x res ))) voisins_pont in
    let file' = (function |[] -> [] |h::t->t) (file@voisins_non_atteint) in
    match file' with
    |[] -> res
    |h::t -> aux h (file') (res@voisins_non_atteint)
  in aux pair [pair] [pair]


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

let fill pair sol puz =
  let lv = get_voisins sol pair puz in
    let rec aux l res =
      match l with
      |[] -> res
      |h::t -> let dir = dir_to_coord pair h in
               aux t (dessinerPonts res pair dir) 
    in aux lv sol 
       
let nfill pair sol puz =
  let rec aux pair res =
    let nbpr = (ponts_restants (coord_from_pair pair) res ) in
    if nbpr = 0 then res else aux pair (fill pair res puz)
  in aux pair sol

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
    | Nothing -> get_first_island nextPair dir
    | (Bridge b') as b -> if (bon_sens_pas_double (b,dir)) then (get_first_island nextPair dir) else []
    | (Island imp) -> if (est_complet (coord_from_pair pair') sol) then [] else
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

let get_voisins_test sol pair puz =
  let vp = get_voisins_pont sol pair in
  let rec get_first_island pair' dir =
    let bon_sens =
      function
      | Bridge {isVertical = true; isDoubled = _ },(Haut|Bas) -> true
      | Bridge {isVertical = false; isDoubled = _ },(Gauche|Droite) -> true
      | _,_ -> false in
    
    let nextPair = next_pair dir pair' in
    if (oob sol pair') then []
    else
      let current_cell = getCell sol pair' in

    match current_cell with
    |Nothing -> get_first_island nextPair dir
    |(Bridge b') as b ->if (bon_sens (b,dir)) then (get_first_island nextPair dir) else []
    |(Island imp) -> if (est_complet (coord_from_pair pair') sol) then(if (List.mem pair' vp )then [pair'] else []) else
                      if(est_complet (coord_from_pair pair) sol) then [] else
      let compo_connexe =
        let sol_sim = dessinerPonts sol pair dir in
        let liste_cw = parcours_largeur_pont sol_sim pair' in
        
        (test_est_composante_connexe liste_cw sol_sim) && (not(jeu_est_fini sol_sim puz)) in if (compo_connexe) then [] else [pair'] in
  (get_first_island (next_pair Gauche pair) Gauche)@
  (get_first_island (next_pair Haut pair) Haut)@
  (get_first_island (next_pair Droite pair) Droite)@
  (get_first_island (next_pair Bas pair) Bas)
  

let pont_max pair1 pair2 sol puz =
  let voisin_pair1 = get_voisins sol pair1 puz in
  let sont_voisins = List.mem pair2 voisin_pair1 in
  if not(sont_voisins) then failwith((string_of_pair(pair1))^" ; "^(string_of_pair(pair2))^" même pas voisins\n")
  else
    
    let rec aux p1 p2 sol' res =
      if ((est_complet (coord_from_pair p2) sol' )||(est_complet (coord_from_pair p1) sol' )) then res
      else
      let pontdess = dessinerPonts sol' p1 (dir_to_coord p1 p2) in
      let pl = parcours_largeur_pont pontdess pair1 in
      let compco = (test_est_composante_connexe pl pontdess) && not(jeu_est_fini pontdess puz) in
      if (compco || res = 2) then res else aux p1 p2 (pontdess) (res +1) 
    in aux pair1 pair2 sol 0

let somme_pont_max pair sol puz =
  let lv = get_voisins sol pair puz in
  let rec aux l res =
    match l with
      [] -> res
    | h::t ->
      let pmx = pont_max pair h sol puz in
      aux t (res+pmx)
  in aux lv 0

let rec equals = fun sol1 -> fun sol2 ->
  match sol1,sol2 with
  | [], [] -> true
  | h1::t1, h2::t2 ->
    begin
      let rec iter_ligne = fun l1 -> fun l2 ->
        match l1,l2 with
        | [],[]  -> equals t1 t2
        | h3::t3, h4::t4 ->
          begin
            match h3,h4 with
            | Nothing,Nothing -> iter_ligne t3 t4
            | Island i1, Island i2 -> if int_of_importance i1 != int_of_importance i2 then false else iter_ligne t3 t4
            | Bridge {isVertical = iv1; isDoubled = id1}, Bridge {isVertical = iv2; isDoubled = id2} -> if iv1 <> iv2 || id1 <> id2 then false else iter_ligne t3 t4
            | _,_ -> false
          end
        | _,_ -> false in
      iter_ligne h1 h2
    end
  | _,_ -> false
  
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
        | [] ->
          (
            res
          )
        | h::t ->
           (* let pontMAX = pont_max cell_pos h res puzzle in *)
           let pontMIN = ponts_min((ponts_restants (coord_from_pair cell_pos) res), nb_voisins) in
           (* let pontMIN = ponts_min((pont_max cell_pos h res puzzle), nb_voisins) in  *)
          if (est_complet (coord_from_pair cell_pos) res) || (est_complet (coord_from_pair h) res) then
            completer_voisins t res 
          else
            let ile_voisine = getCell res h in
            if importance >= (int_of_island ile_voisine) && List.length (get_voisins res h puzzle) = 1 then
              completer_voisins t (dessinerPonts res cell_pos (dir_to_coord cell_pos h)) 
                
            else
            if (pontMIN > 0) then 
              completer_voisins t (fill cell_pos res puzzle)
            else
              completer_voisins t res in
      let lvtest = (get_voisins_test res cell_pos puzzle) in
      let nb_voisins_test = List.length lvtest in 
      let pontMIN' = ponts_min (importance,nb_voisins_test)in
      let sum_pont_av = somme_pont_max cell_pos res puzzle in
      let pont_rest = (ponts_restants (coord_from_pair cell_pos) res) in
      if pontMIN' = importance then
        (
          aux t (nfill cell_pos res puzzle) 
        )
        
      else
      if ( sum_pont_av = pont_rest ) then aux t (nfill cell_pos res puzzle) 
      else(
        let sol_aux = completer_voisins voisins res in
        aux t sol_aux) in
  let rec solvecchaudla = fun sol ->
    let rec aux2 = fun sol1 -> fun res -> fun i ->
    match sol1 with
    | [] -> res
    | h::t ->
      let rec iter_ligne_sol = fun ligne -> fun j -> fun sol2 ->
        match ligne with
        | [] -> sol2
        | h1::t1 ->
          match h1 with
          | Nothing -> iter_ligne_sol t1 (j+1) sol2
          | Island a ->
            let cell_pos = (i,j) in
            if (est_complet (coord_from_pair cell_pos) sol2) then iter_ligne_sol t1 (j+1) sol2
            else
            let voisins = get_voisins sol cell_pos puzzle in
            let rec try_connexe = fun sol3 -> fun voisins ->
              match voisins with
              | [] -> sol3
              | h::t ->
                let direction = dir_to_coord cell_pos h in
                if (est_complet (coord_from_pair h) sol3) then try_connexe sol3 t
                else
                let sol_modifie = dessinerPonts sol3 cell_pos direction in
                let parcours = parcours_largeur sol_modifie cell_pos in
                let test = test_est_composante_connexe parcours sol_modifie in
                if not test then
                  let voisins = get_voisins sol_modifie cell_pos puzzle in
                  sol_modifie 
                else try_connexe sol3 t in
            
            let tryco = try_connexe sol2 voisins in
            iter_ligne_sol t1 (j+1) (tryco)
          | Bridge {isVertical = iv; isDoubled = id} -> iter_ligne_sol t1 (j+1) sol2 in
      aux2 t (iter_ligne_sol h 0 res) (i+1) in
    aux2 sol sol 0 in
 (* let rec apply = fun i -> fun last_res -> fun res ->
    if i = 0 then res
    else
      let to_apply = aux puzzle_l res in
      apply (i-1) res (to_apply) in
  apply 10 solution_vide solution_vide;; *)
  let rec apply = fun stop -> fun last_res -> fun res ->
    if stop then res       
    else
      let jeu_fini = jeu_est_fini res puzzle in
      let pas_bouge = equals last_res res in
      if jeu_fini || (pas_bouge && (not (equals res solution_vide))) then
        if jeu_fini then res
        else
          (
            (* on appelle solve c chaud là *)
            let soltest = solvecchaudla res in
            let soltestest = aux puzzle_l soltest in
            if jeu_est_fini soltestest puzzle then soltestest
            else failwith "Pas de solution !"
          )
      else
        apply ((jeu_est_fini res puzzle)) res (aux puzzle_l res) in
    apply (jeu_est_fini solution_vide puzzle) solution_vide solution_vide;;

let display_solution = fun puzzle ->
  let solutionRAW = solve puzzle in
  (* let solutionRAW = init_solution puzzle in *)(* demuter pour affichage seulement (preferable de mute la ligne audessus alors)) *)
  let solution = List.rev solutionRAW in
  let title = "Hashiwo Kakero desu" in
  Graphics.open_graph "";
  Graphics.resize_window 700 700;
  Graphics.set_color(Graphics.black);
  Graphics.draw_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ());
  Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ());
  Graphics.set_color(Graphics.white);
  Graphics.set_window_title title;
  let rec displaySol = fun sol -> fun i ->
    match sol with
    | [] -> ()
    | h::t ->
      let rec displayLine = fun line -> fun j ->
      match line with
        | [] -> displaySol t (i+1)
        | h1::t1 ->
          match h1 with
          | Nothing ->
            Graphics.moveto (20 + j * 60) (i * 50 + 20);
            displayLine t1 (j+1) 
          | Island island ->
            Graphics.draw_circle (20 + j * 60) (i * 50 + 20) 20;
            Graphics.moveto (20 + j * 60 - 2) (i * 50 - 5 + 20);
            Graphics.draw_string (string_of_int (int_of_importance island));
            Graphics.moveto (20 + j * 60) (i * 50 + 20);
            displayLine t1 (j+1)
          | Bridge { isVertical = iv; isDoubled = id } ->
            if iv then
              (
              if id then
                (
                  Graphics.set_color Graphics.green;
                  Graphics.moveto (50 + j * 60 - 5 - 30) (i * 50 + 30 + 20);
                  Graphics.lineto (50 + j * 60 - 5 - 30) (i * 50 - 30 + 20);
                  
                  Graphics.moveto (50 + j * 60 + 5 - 30) (i * 50 + 30 + 20);
                  Graphics.lineto (50 + j * 60 + 5 - 30) (i * 50 - 30 + 20);
                  Graphics.set_color Graphics.white;
                  displayLine t1 (j+1)
                )
              else
                (
                  Graphics.set_color Graphics.green;
                  Graphics.moveto (50 + j * 60 - 30) (i * 50 + 30 + 20);
                  Graphics.lineto (50 + j * 60 - 30) (i * 50 - 30 + 20);
                  Graphics.set_color Graphics.white;
                  displayLine t1 (j+1)
                )
            )
          else
            (
              if id then
                (
                  Graphics.set_color Graphics.green;
                  Graphics.moveto (50 + j * 60 + 40 - 30) (i * 50 + 5 + 20);
                  Graphics.lineto (50 + j * 60 - 40 - 30) (i * 50 + 5 + 20);
                  Graphics.moveto (50 + j * 60 + 40 - 30) (i * 50 - 5 + 20);
                  Graphics.lineto (50 + j * 60 - 40 - 30) (i * 50 - 5 + 20);
                  Graphics.set_color Graphics.white;
                  
                  displayLine t1 (j+1)
                )
              else
                (
                  Graphics.set_color Graphics.green;
                  Graphics.moveto (50 + j * 60 + 40 - 30) (i * 50 + 20);
                  Graphics.lineto (50 + j * 60 - 40 - 30) (i * 50 + 20);
                  Graphics.set_color Graphics.white;
                  displayLine t1 (j+1) 
                )
            ) in
      displayLine h 0 in
  displaySol solution 0;
