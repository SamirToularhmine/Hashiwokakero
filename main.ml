open Puzzle
open Coordinate
open Solution

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
      ]
    );;

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


let _ = print_string (Solution.toString (Solution.solve puzzleTest2));;

let rec loop = fun b ->
  let input = Graphics.read_key () in
  if input = 'q' then
    Graphics.close_graph()
  else if input = '1' then
    (
      display_solution (puzzleTest1);
      loop b
    )
  else if input = '2' then
    (
      display_solution (puzzleTest2);
      loop b
    )
  else if input = '3' then
    (
      display_solution (puzzleTest3);
      loop b
    )
  else if input = '4' then
    (
      display_solution (puzzleTest4);
      loop b
    )
  else if input = '5' then
    (
      display_solution (puzzleTest5);
      loop b
    )
  else if input = '6' then
    (
      display_solution (puzzleTest6);
      loop b
    )
  else loop b in
display_solution (puzzleTest1);
loop ();;
