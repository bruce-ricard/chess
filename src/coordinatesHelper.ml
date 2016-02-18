open CoordinatesTypes

let int_of_rank : rank -> int = Obj.magic
let int_of_file : file -> int = Obj.magic

let rank_of_int : int -> rank = Obj.magic
let file_of_int : int -> file = Obj.magic

let square_of_ints file rank =
  print_int file; print_string ", "; print_int rank; print_newline ();
  if file >= 0 && file <= 7 && rank >= 0 && rank <= 7 then
    Some (Square ((file_of_int file), (rank_of_int rank)))
  else
    None

let is_square file rank = file >= 0 && file <= 7 && rank >= 0 && rank <= 7

let ints_of_square = function
  | Square(file, rank) -> ((int_of_file file), (int_of_rank rank))

let rec remove_none = function
  | [] -> []
  | None :: q -> remove_none q
  | Some x :: q -> x :: remove_none q

let knight_moves square =
  let file,rank = ints_of_square square in
  remove_none
    [square_of_ints (file + 1) (rank + 2);
     square_of_ints (file + 1) (rank - 2);
     square_of_ints (file - 1) (rank + 2);
     square_of_ints (file - 1) (rank - 2);
     square_of_ints (file + 2) (rank + 1);
     square_of_ints (file + 2) (rank - 1);
     square_of_ints (file - 2) (rank + 1);
     square_of_ints (file - 2) (rank - 1)]

let rec diagonal file rank north east =
  let x_dir = if north then 1 else -1
  and y_dir = if east then 1 else -1 in
  let rec diagonal_aux file rank =
    if is_square file rank then
      square_of_ints file rank :: diagonal_aux (file + x_dir) (rank + y_dir)
    else
      []
  in diagonal_aux (file + x_dir) (rank + y_dir)

let bishop_moves square =
  let file,rank = ints_of_square square in
  remove_none (diagonal file rank true true @
    (diagonal file rank true false) @
    (diagonal file rank false true) @
    (diagonal file rank false false));;

    bishop_moves (Square (E, Three));;

let rook_moves square =
  let file,rank = ints_of_square square in ()
