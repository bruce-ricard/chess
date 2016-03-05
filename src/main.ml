open Position

module ChessPosition = Position(Board.ArrayBoard)

open ChessPosition

let position = new_position();;

let _ = print_endline (position_to_fen (position))

open Board.ArrayBoard
let board = init_board ()

open Coordinates

let s1 = Square (A, One)
let s2 = Square (D, Four)
let s3 = Square (B, One)
let s4 = Square (D, Five)

let mmove s1 s2 = let open Board.ArrayBoard in
  {starting_square = s1; destination_square = s2}

let board2 = move (mmove s3 s4) (move (mmove s1 s2) board)

let () = print_endline (to_fen board2)
