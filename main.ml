(* Types initiaux *)
type coordinate = Coordinate of (int * int);;
type importance = Importance of int;;
type puzzle = Puzzle of (coordinate * importance) list;;

(* Types Finaux *)
type bridge = { isVertical : bool; isDoubled : bool};;
type cell = Nothing | Island of importance | Bridge of bridge;;
type solution = cell list list;;

(* Puzzles *)
let puzzle2 =
  Puzzle
    [
    (Coordinate (0, 0), Importance 4);
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
    ];;

let rec getVoisins =
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
     end;;

