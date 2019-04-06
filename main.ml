
(* Types initiaux *)
(* type coordinate = Coordinate of (int * int);;
type importance = Importance of int;;
type puzzle = Puzzle of (coordinate * importance) list;;

(* Types Finaux *)
type bridge = { isVertical : bool; isDoubled : bool};;
type cell = Nothing | Island of importance | Bridge of bridge;;
type solution = cell list list;; *)

(* Puzzles *)
(* Coordinate(colonne,ligne) *)
open Puzzle;;
open Coordinate;;
let x = Coordinate.create (3,2);;
print_int (Coordinate.sndcoord x);;
print_string "\n";;

(* A réfléchir pour voir si il y a besoin de méthodes create *)
let puzzleTest = Puzzle.create
                   (
                     [
                       (Coordinate.create (0,0), Puzzle.getImportance 4);
                       (Coordinate.create (0,3), Puzzle.getImportance 4);
                       (Coordinate.create (0,6), Puzzle.getImportance 3);
                       (Coordinate.create (2,1), Puzzle.getImportance 1);
                       (Coordinate.create (2,3), Puzzle.getImportance 4);
                       (Coordinate.create (2,5), Puzzle.getImportance 2);
                       (Coordinate.create (3,0), Puzzle.getImportance 4);
                       (Coordinate.create (3,6), Puzzle.getImportance 5);
                       (Coordinate.create (5,0), Puzzle.getImportance 2);
                       (Coordinate.create (5,5), Puzzle.getImportance 1);
                       (Coordinate.create (6,2), Puzzle.getImportance 1);
                       (Coordinate.create (6,4), Puzzle.getImportance 3);
                       (Coordinate.create (6,6), Puzzle.getImportance 4)
                     ]
                   );;

print_string (Puzzle.toString puzzleTest);;

(*let puzzle2 =
  Puzzle.create
    [
    (Coordinate(0, 0), Importance 4);
    (Coordinate (0, 3), Importance 4);
    (Coordinate (0, 6), Importance 3);
    (Coordinate (2, 1), Importance 1);
    (Coordinate (2, 3), Importance 4);
    (Coordinate (2, 5), Importance 2);
    (Coordinate (3, 0), Importance 4);
    (Coordinate (3, 6), Importance 5);
    (Coordinate (5, 0), Importance 2);
    (Coordinate (5, 5), Importance 1);
    (Coordinate (6, 2), Importance 1);
    (Coordinate (6, 4), Importance 3);
    (Coordinate (6, 6), Importance 4)
    ];;*)

(*let rec getVoisins =
  fun c -> fun p ->
           match p with
           | Puzzle q ->
              begin
                match q with 
                | [] -> []
                | (Coordinate (x,y), Importance z)::t ->
                   if (x = (fst)c || y = (snd)c) && ((x,y) <> c)
                   then (Coordinate (x,y), Importance z)::getVoisins c (Puzzle t)
                   else getVoisins c (Puzzle t)              
              end;;

let rec getBords = fun p -> fun n ->
  match p with
  | Puzzle q ->
     begin
       match q with
       | (Coordinate (x,y), Importance z)::t ->
          if  x = 0 || y = 0 || x = n || y = n
          then (Coordinate (x,y), Importance z)::getBords (Puzzle t) n
          else getBords (Puzzle t) n
       | _ -> []
     end;;*)
(* fonctions pour travailler sur des coordinate s 

let fstcoord (Coordinate (x,y))=x;;
let sndcoord (Coordinate (x,y))=y;;

(* on definit des operateurs sur les coordinates  *)
let (>~) c1 c2 = (fstcoord c1) > (fstcoord c2);;
let (>~~) c1 c2 = (sndcoord c1) > (sndcoord c2);;
(* petit test *)
let _ = Coordinate (0,3) >~~ Coordinate (1,2);;
(* ça marche bien *)
 


(* on applique une fonction une paire de coordinnate... ah mais enfait elle sert à rien sert fonction *)
let fonc_sur_coord f (x:coordinate) (y:coordinate) = f x y;; 

(* recuperer c1 ou c2 selon un predicat *)
let filtercoord f c1 c2 = fonc_sur_coord (fun x y ->if (f x y) then x else y) c1 c2;;

(* max selon colonne et max selon ligne *)
let maxfstcoord c1 c2 = filtercoord (>~) c1 c2;;
let maxsndcoord c1 c2 = filtercoord (>~~) c1 c2;;

(* operation pour travailler sur des puzzle s *)

(* fonction de reduction sur des puzzle s *)
let reducepuzzle f (pzl : puzzle) =
  match pzl with
  | Puzzle plist -> List.fold_right (f)  plist (List.nth plist 0);;

(* petit test, ici on recupere la colonne max du puzzle *)
let _ = fstcoord (fst(reducepuzzle ((fun x y->if (fst x) >~ (fst y) then x else y)) puzzle2));;

(* nombre de (coordinate*importance) dans un puzzle  *)
let lengthpzl (Puzzle l)=List.length l;;
lengthpzl puzzle2;;*)
