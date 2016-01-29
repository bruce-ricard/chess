open Position

module ChessPosition = Position(Board.ArrayBoard)

open ChessPosition

let position = new_position();;

print_endline (position_to_fen (position))
