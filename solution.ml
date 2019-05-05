open Puzzle
open Coordinate
open List

(**
   Le type bridge nous permet de représenter un pont dans une solution
*)
type bridge = { isVertical : bool; isDoubled : bool}

(**
   Le type cell nous permet de représenter les différentes cellules de la solution : elles se définissent par le type Nothing, Island et Bridge.
   
*)
type cell = Nothing | Island of Puzzle.importance | Bridge of bridge

(**
   Le type solution nous permet de représenter une solution grâce à une liste à
   deux dimensions de cellules
*)
type solution = cell list list

(**
   Le type direction nous permet de représenter la direction dans laquelle nous
   devons dessiner un pont par rapport à la position de la cellule.
*)
type direction = Gauche | Haut | Droite | Bas

exception NotAnIsland
exception OutOfBounds
exception IslandMet
exception BridgeMet
exception UnlinkedCoords
exception TooMuchBridges
exception NoSolutionsFound
                               
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
  | (Bridge b) -> string_of_bridge b

let string_of_direction = fun d ->
  match d with
  | Haut -> "Haut"
  | Bas -> "Bas"
  | Gauche -> "Gauche"
  | Droite -> "Droite"
                
let importance_of_island = function
  | Island imp -> imp
  | Nothing -> raise NotAnIsland
  | Bridge b -> raise NotAnIsland
                 
let int_of_island = function
  | Island imp -> int_of_importance imp
  | _ -> raise NotAnIsland

let rec string_of_solution = fun s ->
  match s with
  | [] -> ""
  | h::t ->
     let rec toStringLigne = fun l ->
       match l with
       | [] -> ""
       | h1::t1 ->
         match h1 with
         | cell -> (string_of_cell h1) ^ toStringLigne t1 in
     (toStringLigne h) ^ "\n" ^(string_of_solution t)
(**
   val oob : 'a list list -> int * int -> bool 

   oob [[a1; ...;an]; ...;[z1; ...zn]] (x,y) verifie si la pair (x,y) est en dehors de la liste de liste
   retourne un booleen, true si la pair (x,y) est "out of bound", false sinon.
*)                                  
let oob sol =
  let mxR = (List.length sol)-1 in
  let mxC = if (mxR > 0) then (List.length (nth sol 0) -1)
            else failwith "La solution est vide !" in
  function
  | (x,y) -> (x > mxR) || (x < 0) || (y > mxC) || (y < 0)

(** 
   val init_solution : puzzle -> solution

   La fonction init_solution nous permet de créer une solution vide à partir
   d'un puzzle donné en paramètre. 
*)
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

(**
	val getCell : 'a list list -> int * int -> 'a

	getCell [[a1; ...;an]; ...;[z1; ...zn]] (i,j) retourne l'élement correspondant aux coordonnées (i,j)
*)
let getCell = fun sol -> function
  | (x,y) ->
    if (oob sol (x,y)) then raise OutOfBounds
    else nth (nth sol x) y

(**
	val next_pair dir pair : direction -> int * int -> int * int

	next_pair dir (x,y) retourne une futur pair en fonction de la direction  passé en paramètre
*)
let next_pair dir pair = 
  match (dir,pair) with
  | Gauche,(x,y) -> (x,y-1)
  | Haut,(x,y) -> (x-1,y)
  | Droite,(x,y) -> (x,y+1)
  | Bas,(x,y) -> (x+1,y)                 
    
(**
	val replace : 'a list list -> int * int -> 'a -> 'a list list

	replace sol (x,y) elt retourne une 'a list list avec l'élément aux coordonées (x,y) remplacé par elt
*)
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
          aux2 t2 c v (res2 @ [if (c = (i,j)) then v else h]) in
      aux1 t1 c v ((aux2 l2 c v [])::res1) in
  List.rev (aux1 l c v [])
        
(**
	val dessinerPonts : cell list list -> int * int -> direction -> cell list list

	dessinerPonts sol (x,y) dir retourne une cell list list avec un pont dessiné à partir des coordonnées (x,y) dans une direction dir
*)
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
      | _,_ -> raise Not_found
                 
  in try aux nextPair dir sol with
  | BridgeMet -> raise BridgeMet
         
(**
	val nombre_de_pont : cell list list -> int * int -> int

	nombre_de_pont sol (x,y) on considèrera que l'element aux coordonnées (x,y) est de type Island, la fonction retourne le nombre de pont partant de l'île aux coordonnées (x,y)
*)
let nombre_de_pont sol pair =
  let aux dir =
    let bon_sens = function
      | Bridge {isVertical = true; isDoubled = _ },(Haut|Bas) -> true
      | Bridge {isVertical = false; isDoubled = _ },(Gauche|Droite) -> true
      | _,_ -> false in
    
    let pair' = next_pair dir pair in
    if(oob sol pair') then 0
    else
      let cell = getCell sol pair' in
      match cell with
      |(Bridge {isVertical = _; isDoubled = double}) as bridge ->
        if (bon_sens (bridge,dir)) then (if (double) then 2 else 1)
        else 0
      | _ -> 0                                             
  in (aux Gauche) + (aux Haut) + (aux Droite) + (aux Bas)

(**
   val est_complet : cell -> solution -> bool

   La fonction est_complet nous permet de savoir si la cellule passée 
   en paramètre est complète ou non dans la solution, elle aussi passée
   en paramètre.
   
*)
let est_complet = fun c -> fun sol ->
  let cell = getCell sol (fstcoord c, sndcoord c) in
  let importance =
    match cell with
    | Island a -> int_of_importance a
    | _ -> raise NotAnIsland in
  let totalPont = nombre_de_pont sol (pair_from_coord c) in 
  if totalPont < importance then false
  else if importance = totalPont then true
  else raise TooMuchBridges

(**
	val pont_restants : coordinate -> cell list list -> int

	pont_restants (Coordinate (x,y)) l on considèrera que l'element aux coordonnées (x,y) est de type Island,
	la fonction retourne le nombre de pont qui manquent pour compléter l'île aux coordonnées (x,y)
*)
let ponts_restants = fun c -> fun sol ->
  let cell = getCell sol (fstcoord c, sndcoord c) in
  let importance =
    match cell with
    | Island a -> int_of_importance a
    | _ -> raise NotAnIsland in
  let total_ponts = nombre_de_pont sol (pair_from_coord c) in
  importance - total_ponts
  
(**
	val get_voisins_pont : cell list list -> int * int -> (int * int) list

	get_voisins_pont sol (x,y) on considèrera que l'element aux coordonnées (x,y) est de type Island, retourne une liste de pair comprenant tout les voisins de l'île relié par des ponts
*)
let get_voisins_pont sol pair =
  let bon_sens = function
    | Bridge {isVertical = true; isDoubled = _ },(Haut|Bas) -> true
    | Bridge {isVertical = false; isDoubled = _ },(Gauche|Droite) -> true
    | _,_ -> false in
  
  let rec get_first_island pair dir =
    let nextPair = next_pair dir pair in
    if (oob sol pair) then []
    else
      let current_cell = getCell sol pair  in

      match current_cell with
      | Nothing ->  []
      | (Bridge b') as b ->
        if (bon_sens (b,dir)) then (get_first_island nextPair dir)
        else []
      | (Island imp)-> [pair] in
  
  (get_first_island (next_pair Gauche pair) Gauche) @
  (get_first_island (next_pair Haut pair) Haut) @
  (get_first_island (next_pair Droite pair) Droite) @
  (get_first_island (next_pair Bas pair) Bas)

(**
	val parcours_largeur : cell list list -> int * int -> (int * int) list

	parcours_largeur sol (x,y) lance un parcours en largeur sur la cell aux coordonnées (x,y), les voisins de chaque noeud atteint sont obtenus avec la fonction "get_voisins_pont",
	retourne la liste des coordonnées des cell atteintes.
*)
let parcours_largeur sol pair =
  let rec aux pair file res =
    let voisins_pont = get_voisins_pont sol pair in
    let voisins_non_atteint = List.filter (fun x -> (not(List.mem x res ))) voisins_pont in
    let file' = (function | [] -> [] | h::t->t) (file@voisins_non_atteint) in
    match file' with
    | [] -> res
    | h::t -> aux h (file') (res@voisins_non_atteint)
  in aux pair [pair] [pair]


(**
	val parcours_largeur : cell list list -> int * int -> (int * int) list

	parcours_largeur sol (x,y) lance un parcours en largeur sur la cell aux coordonnées (x,y), les voisins de chaque noeud atteint sont obtenus avec la fonction "get_voisins_pont",
	retourne la liste des coordonnées des cell atteintes.
*)
let parcours_largeur_pont sol pair =
  let rec aux pair file res =
    let voisins_pont = get_voisins_pont sol pair in
    let voisins_non_atteint = List.filter (fun x -> (not(List.mem x res ))) voisins_pont in
    let file' = (function | [] -> [] | h::t->t) (file @ voisins_non_atteint) in
    match file' with
    | [] -> res
    | h::t -> aux h (file') (res@voisins_non_atteint)
  in aux pair [pair] [pair]

(**
	val test_est_composante_connexe : (int * int) list -> cell list list -> bool 

	test_est_composante_connexe liste sol, "liste" doit être une liste issu de la résultante de la fonction parcours_largeur_pont, si toutes les cell correspondant aux coordonnées de "liste" sont complètes (ref: est_complet) alors nous avons affaire à une composante connexe c'est à dire un graphe auquelle on ne pourra plus ajouté d'arc dans ce contexte présent 
	retourne true si c'est une composante connexe
*)   
let test_est_composante_connexe liste sol = (List.for_all (fun x -> est_complet (coord_from_pair x)  sol) liste)

(**
	val jeu_est_fini : cell list list -> puzzle -> bool

	jeu_est_fini sol puz, un jeu ici sera le fait de donner une solution terminée, si un jeu est fini alors nous avons ces conditions satisfaites : toutes les cell de sol sont complètes et appartiennent toutes au même graphe
	retourn true si le jeu est fini.
*)
let jeu_est_fini sol puz =
  let puz' = list_of_puzzle puz in 
  let liste_finale = 
    let first_pair = pair_from_coord(fst ((function |[]->failwith"puzzle vide"|h::t->h) puz')) in
    parcours_largeur_pont sol first_pair in
  ((List.length liste_finale) = (List.length puz')) && (test_est_composante_connexe liste_finale sol)

(**
	val get_voisins : cell list list -> int * int -> puzzle -> (int * int) list

	get_voisins sol (x,y) puz, un voisin ici est caractèrisé par une cell (appelons la cellVoisine) que la cell aux coordonnées (x,y) (appelons la cellInitiale) peut potentiellement mettre un pont entre elles, c'est à dire que cellVoisine ne doit pas être complète, elle ne doit pas créer plusieurs composantes connexe si on rajoute un pont à l'avenir entre les deux cell (cellVoisine et cellInitiale) et il ne doit pas y avoir de pont entre les deux cell orienté de façon à ce qu'il ne soit pas empruntable.
	retourne la liste de tous les voisins respectant ces contraintes.
*)
let get_voisins sol pair puz =
  let bon_sens_pas_double = function
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
      | (Bridge b') as b ->
        if (bon_sens_pas_double (b,dir)) then (get_first_island nextPair dir)
        else []
      | (Island imp) ->
        if (est_complet (coord_from_pair pair') sol) then []
        else
        if(est_complet (coord_from_pair pair) sol) then []
        else
          let compo_connexe =
            let sol_sim = dessinerPonts sol pair dir in
            let liste_cw = parcours_largeur_pont sol_sim pair' in
            (test_est_composante_connexe liste_cw sol_sim) && (not(jeu_est_fini sol_sim puz)) in 
          if (compo_connexe) then [] else [pair'] in
  
  (get_first_island (next_pair Gauche pair) Gauche)@
  (get_first_island (next_pair Haut pair) Haut)@
  (get_first_island (next_pair Droite pair) Droite)@
  (get_first_island (next_pair Bas pair) Bas)
 
let string_of_list string_of liste = (List.fold_right (fun x y-> ("[")^(string_of x)^"]"^y) (liste) "")


(**
   val dir_to_coord = (int * int) -> (int * int) -> direction
   
   La fonction dir_to_coord nous permet d'obtenir la direction dans laquelle
   il faut dessiner un pont pour partir de la cellule de coordonnées c1 à la 
   cellule de coordonnées c2.
*)
let dir_to_coord = fun c1 -> fun c2 ->
  match c1,c2 with
  | (i1,j1),(i2,j2) ->
    if i1 = i2 || j1 = j2 then
      if i1 = i2 then
        if j1 < j2 then Droite else Gauche
      else
        if i1 < i2 then Bas else Haut
    else raise UnlinkedCoords;;

(**
	val fill : int * int -> cell list list -> puzzle -> cell list list

	fill (x,y) sol puz, sans vérification au préalabe cette fonction applique une seule fois "dessinerPonts" entre la cell aux coordonnées (x,y) et chacun de ses voisin,
	retourne une cell list list avec les modifications.
*)
let fill pair sol puz =
  let lv = get_voisins sol pair puz in
    let rec aux l res =
      match l with
      |[] -> res
      |h::t -> let dir = dir_to_coord pair h in
               aux t (dessinerPonts res pair dir) 
    in aux lv sol 
       
(**
	val nfill : int * int -> cell list list -> puzzle -> cell list list

	nfill (x,y) sol puz, cette fonction appliquera la fonction fill tant que le nombre de pont restant de la cell aux coordonnées (x,y) n'est pas égale à Zéro
	retourne une cell list list avec les modifications.
*)
let nfill pair sol puz =
  let rec aux pair res =
    let nbpr = (ponts_restants (coord_from_pair pair) res ) in
    if nbpr = 0 then res
    else aux pair (fill pair res puz) in
  aux pair sol
    
(**
	val get_voisins : cell list list -> int * int -> puzzle -> (int * int) list

	get_voisins sol (x,y) puz, un voisin ici est caractèrisé par une cell (appelons la cellVoisine) que la cell aux coordonnées (x,y) (appelons la cellInitiale) peut potentiellement mettre un pont entre elles, c'est à dire que cellVoisine ne doit pas être complète, elle ne doit pas créer plusieurs composantes connexe si on rajoute un pont à l'avenir entre les deux cell (cellVoisine et cellInitiale) et il ne doit pas y avoir de pont entre les deux cell orienté de façon à ce qu'il ne soit pas empruntable.
	retourne la liste de tous les voisins respectant ces contraintes.
*)
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

(**
	val get_voisin_test : cell list list -> int * int -> puzzle -> (int * int) list 

	get_voisin_test sol (x,y) puz, cette fonction est du même acabit que get_voisin et get_voisin, bien qu'elle s'apparente à un mix des deux (on récupère les voisins avec et sans pont entre) il y a une autre subtilité, si une cell est complète on la considérera comme une cell voisine si elle appartient à la même composante que la cell aux coordonnées (x,y),
	retourne la liste de toutes les cell respectants les contraintes ci dessus
*)
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
      | Nothing -> get_first_island nextPair dir
      | (Bridge b') as b ->
        if (bon_sens (b,dir)) then (get_first_island nextPair dir)
        else []
      | (Island imp) ->
        if (est_complet (coord_from_pair pair') sol) then(if (List.mem pair' vp )then [pair'] else [])
        else
        if(est_complet (coord_from_pair pair) sol) then [] else
          let compo_connexe =
            let sol_sim = dessinerPonts sol pair dir in
            let liste_cw = parcours_largeur_pont sol_sim pair' in
            (test_est_composante_connexe liste_cw sol_sim) && (not(jeu_est_fini sol_sim puz)) in
          if (compo_connexe) then [] else [pair'] in
  (get_first_island (next_pair Gauche pair) Gauche) @
  (get_first_island (next_pair Haut pair) Haut) @
  (get_first_island (next_pair Droite pair) Droite) @
  (get_first_island (next_pair Bas pair) Bas)

(**
	val pont_max : int * int -> int * int -> cell list list -> puzzle -> int

	pont_max (x,y) (x',y') sol puz on considèrera que les éléments aux coordonnées (x,y) et (x',y') sont de type Island,
	la fonction va evaluer le nombre de pont que l'on peut mettre entre les Islands de coordonnées (x,y) et (x',y') sans créer plusieurs composantes connexes 
*)
let pont_max pair1 pair2 sol puz =
  let voisin_pair1 = get_voisins sol pair1 puz in
  let sont_voisins = List.mem pair2 voisin_pair1 in
  if not(sont_voisins) then failwith((string_of_pair(pair1))^" ; "^(string_of_pair(pair2))^" même pas voisins\n")
  else
    let rec aux p1 p2 sol' res =
      if ((est_complet (coord_from_pair p2) sol' ) || (est_complet (coord_from_pair p1) sol' )) then res
      else
        let pontdess = dessinerPonts sol' p1 (dir_to_coord p1 p2) in
        let pl = parcours_largeur_pont pontdess pair1 in
        let compco = (test_est_composante_connexe pl pontdess) && not(jeu_est_fini pontdess puz) in
        if (compco || res = 2) then res
        else aux p1 p2 (pontdess) (res +1) 
    in aux pair1 pair2 sol 0
      
(**
	val somme_pont_max : int * int -> cell list list -> puzzle -> int

	somme_pont_max (x,y) sol puz, 
	retourne l'addition des résultantes de la fonction pont_max effectuée sur chacun des voisins de la cell aux coordonnées (x,y)
*)
let somme_pont_max pair sol puz =
  let lv = get_voisins sol pair puz in
  let rec aux l res =
    match l with
    | [] -> res
    | h::t ->
      let pmx = pont_max pair h sol puz in
      aux t (res+pmx)
  in aux lv 0

(**
   val equals : solution -> solution -> bool

   La fonction equals nous permet de savoir si deux solutions sont équivalentes
   ou non.
*)
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
            | Island i1, Island i2 ->
              if int_of_importance i1 != int_of_importance i2 then false
              else iter_ligne t3 t4
            | Bridge {isVertical = iv1; isDoubled = id1}, Bridge {isVertical = iv2; isDoubled = id2} ->
              if iv1 <> iv2 || id1 <> id2 then false
              else iter_ligne t3 t4
            | _,_ -> false
          end
        | _,_ -> false in
      iter_ligne h1 h2
    end
  | _,_ -> false

(**
   val solve : puzzle -> solution
   
   La méthode solve nous permet d'appliquer une algorithme résolvant le puzzle
   passé en paramètre.

   Pour résoudre ce puzzle, l'algorithme va tout d'abord trouver les ponts 
   évidents à placer autour des différentes îles. Le nombre de ponts évidents
   se trouve grâce à la fonction : 
   
   val pont_min : (int * int) -> int

   La fonction pont_min prend un couple d'entier en paramètre correspondant 
   respectivement au nombre de pont restant ainsi qu'au nombre de voisins de 
   la cellule.
   
   Avec ce nombre de ponts minimum, l'algorithme va savoir placer les
   différents ponts évidents pour chaques îles.

   Cet algorithme va pouvoir s'appliquer un certain nombre de fois grâce à la
   fonction : 

   val apply = bool -> solution -> solution -> solution
   
   La fonction recursive terminale apply va nous permettre d'appliquer 
   l'algorithme tant que le bool en paramètre ne vaut pas true. 
   Lors de l'appel recursif il se rappellera avec une nouvelle solution
   ainsi qu'une ancienne solution afin de savoir si l'algorithme est bloqué
   ou non.

   Dans le cas ou l'algorithme est bloqué, c'est qu'il n'y a plus aucun ponts
   évidents à placer. Dans ce cas, l'algorithme va utiliser une fonction
   permettant de placer des ponts en gardant la connexité de la solution.
   Voici la fonction qui nous permet d'appliquer cette approche : 
   
   val solve_connexite : sol -> sol

   Cette fonction va itérer sur une solution pour trouver les différentes îles
   non complètes. Une fois ces différentes îles trouvées, l'algorithme va
   placer un pont vers un des voisins de cette île et va regarder si la solution
   reste connexe. Si oui il va garder ce pont et continuer d'appliquer 
   l'algorithme sinon il ne va pas garder ce pont et va continuer d'itérer sur
   la solution.
*)

let rec solve_sol = fun solution_vide -> fun puzzle ->
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
            if (pontMIN > 0) then 
              completer_voisins t (fill cell_pos res puzzle)
            else
              completer_voisins t res in
      let lvtest = (get_voisins_test res cell_pos puzzle) in
      let nb_voisins_test = List.length lvtest in 
      let pontMIN' = ponts_min (importance,nb_voisins_test)in
      let sum_pont_av = somme_pont_max cell_pos res puzzle in
      let pont_rest = (ponts_restants (coord_from_pair cell_pos) res) in
      if pontMIN' = importance then aux t (nfill cell_pos res puzzle) 
      else
      if sum_pont_av = pont_rest then aux t (nfill cell_pos res puzzle) 
      else
        let sol_aux = completer_voisins voisins res in
        aux t sol_aux in
  let rec solve_connexite = fun sol -> fun puzzle ->
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
                | h2::t2 ->
                  let direction = dir_to_coord cell_pos h2 in
                  if (est_complet (coord_from_pair h2) sol3) then try_connexe sol3 t2
                  else
                    let sol_modifie = dessinerPonts sol3 cell_pos direction in
                    let parcours = parcours_largeur sol_modifie cell_pos in
                    let test = test_est_composante_connexe parcours sol_modifie in
                    if (not test) then
                      (
                        aux2 t (sol_modifie) (i+1)
                      )
                    else try_connexe sol3 t2 in
              let tryco = try_connexe sol2 voisins in
              iter_ligne_sol t1 (j+1) (tryco)
          | Bridge {isVertical = iv; isDoubled = id} -> iter_ligne_sol t1 (j+1) sol2 in
      aux2 t (iter_ligne_sol h 0 res) (i+1) in
  aux2 sol sol 0 in
   (*let rec apply = fun i -> fun last_res -> fun res ->
     if i = 0 then
       (
         (*print_string (string_of_bool (jeu_est_fini (solve_connexite res) puzzle));
           solve_connexite res*)
         solve_connexite res
       )
    else
      let to_apply = aux puzzle_l res in
      apply (i-1) res (to_apply) in
   apply 1 solution_vide solution_vide;; *)
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
           let solraw = solve_connexite res puzzle in
            let solbon = aux puzzle_l solraw in
            if jeu_est_fini solbon puzzle then solbon
            else raise NoSolutionsFound
          )
      else
        apply ((jeu_est_fini res puzzle)) res (aux puzzle_l res) in
  apply (jeu_est_fini solution_vide puzzle) solution_vide solution_vide

let solve = fun puzzle ->
  let solution_vide = init_solution puzzle in
  solve_sol solution_vide puzzle

(**
   val display_solution : puzzle -> unit
   
   La fonction display_solution nous permet d'utiliser la librairie Graphique
   d'OCaml pour pouvoir afficher les différents puzzles résolus d'une manière
   plus propre que dans le terminal.
*)
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
