open Coordinates

type piece_move_description = {
  piece : Piece.non_pawn;
  starting_file : file option;
  starting_rank : rank option;
  final_square : square;
  capture : bool
}

type pawn_capture_description = {
  starting_file : file;
  final_file : file;
  final_rank : rank option
}

type move =
  | Pawn of square
  | PawnPromotion of file * Piece.promotable
  | PawnCapture of pawn_capture_description
  | PieceMove of piece_move_description

type game = Game of move list
