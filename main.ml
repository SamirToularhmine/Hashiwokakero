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

let coimp c n =
  let imp n = importance_of_int n in
  let cfp c = coord_from_pair c in
  (cfp c,imp n)

  let puzzleTest7 = puzzle_of_list [ coimp (0,0) 3;coimp (0,5) 2;coimp (0,7) 2;coimp (0,9) 1;coimp (2,2) 1;coimp (2,5) 3;coimp (2,9) 1;coimp (4,0) 4;coimp (4,5) 3;coimp (5,2) 4;coimp (5,6) 2;coimp (6,9) 2;coimp (9,0) 3;coimp (9,2) 5;coimp (9,7) 5;coimp (9,9) 3  ]


let puzzleTest8 = puzzle_of_list [
coimp (0,0) 2;coimp(0,2) 2;coimp(0,4) 3;coimp(0,6) 5;coimp(0,10) 1;coimp(0,12) 4;coimp(0,14) 4;
coimp (2,0) 4;coimp(2,6) 8;coimp(2,12) 7;coimp(2,14) 5;
coimp (3,8) 2;
coimp (4,0) 1;coimp(4,3) 4;coimp(4,5) 2;coimp(4,9) 4;coimp(4,12) 4;
coimp (5,4) 2;
coimp (6,0) 2;coimp(6,3) 4;coimp(6,9) 2;coimp(6,14) 2;
coimp (8,0) 3;coimp(8,2) 4;coimp(8,9) 1;coimp(8,12) 2;
coimp (9,4) 3;coimp(9,6) 6;coimp(9,8) 8;coimp(9,14) 5;
coimp (10,2) 2;
coimp (11,0) 2;coimp(11,3) 3;coimp(11,6) 4;coimp(11,8) 7;coimp(11,10) 3;coimp(11,12) 2;
coimp (12,14) 4;
coimp (13,0) 3;coimp(13,7) 2;coimp(13,12) 1;
coimp (14,2) 2;coimp(14,8) 3;coimp(14,14) 2 ]


let puzzleTest9 = puzzle_of_list [
    coimp(0,0) 2;coimp(0,2) 5;coimp(0,24) 1;
    coimp(1,4) 1;coimp(1,6) 3;coimp(1,9) 4;coimp(1,12) 1;
coimp(2,0) 1;coimp(2,3) 2;coimp(2,5) 6;coimp(2,7) 4;coimp(2,11) 3;coimp(2,18) 5;coimp(2,21) 1;coimp(2,24) 2;
coimp(3,9) 3; coimp(3,20) 2; coimp(3,23) 2; 
coimp(7,21) 2; coimp(7,24) 4; 
coimp(8,7) 2; coimp(8,12) 2; coimp(8,14) 3; coimp(8,18) 5; 
coimp(9,3) 2; coimp(9,5) 7; coimp(9,9) 3; coimp(9,11) 3; 
coimp(10,12) 2; coimp(10,18) 4; coimp(10,21) 3; coimp(10,23) 2; 
coimp(11,2) 3; coimp(11,5) 4; coimp(11,11) 5; coimp(11,20) 5; coimp(11,24) 4; 
coimp(12,0) 4; coimp(12,10) 3; 
coimp(13,2) 2; coimp(13,6) 3; coimp(13,8) 2; coimp(13,16) 1; coimp(13,18) 2; coimp(13,22) 2; 
coimp(16,1) 1; coimp(16,4) 1; coimp(16,6) 3; coimp(16,22) 4; 
coimp(17,0) 4; coimp(17,2) 4; coimp(17,8) 6; coimp(17,10) 4; 
coimp(18,3) 2; coimp(18,6) 1; 
coimp(20,3) 6; coimp(20,6) 3; coimp(20,10) 1; coimp(20,22) 4; 
coimp(21,0) 3;
coimp(23,0) 1; coimp(23,2) 1;   
coimp(24,1) 3; coimp(24,3) 5; coimp(24,8) 3; coimp(24,11) 3; coimp(24,18) 2; coimp(24,20) 3; coimp(24,22) 5; coimp(24,24) 3
  ]

let puzzleTest10 = puzzle_of_list
    [
      coimp(0,0) 3; coimp(0,2) 3;
      coimp (2,0) 3; coimp(2,2) 3
    ]

let puzzleTest11 = puzzle_of_list
    [
      coimp(0,0) 4; coimp(0,2) 5; coimp(0,4) 5; coimp(0,7) 2;
      coimp(2,4) 2; coimp(2,7) 2;
      coimp(3,0) 4;
      coimp(5,0) 2; coimp(5,2) 6; coimp(5,4) 6; coimp(5,7) 4;
      coimp(7,0) 1; coimp(7,2) 3; coimp(7,4) 4; coimp(7,7) 2 
    ]

(* let _ = print_string (Solution.toString (Solution.init_solution(puzzleTest9)));; *)
let _ = print_string (Solution.toString (Solution.solve puzzleTest11));;

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
  else if input = '7' then
    (
      display_solution (puzzleTest7);
      loop b
    )
  else if input = '8' then
    (
      display_solution (puzzleTest8);
      loop b
    )
  else if input = '9' then
    (
      display_solution (puzzleTest9);
      loop b
    )
  else loop b in
display_solution (puzzleTest1) ;
loop ();;
