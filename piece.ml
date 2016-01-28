module Piece =
  struct
    type piece_kind = King | Queen | Rook | Bishop | Knight | Pawn
    type color = White | Black
    type t = Piece of color * piece_kind

    let fen_letter = function
      | King -> 'k'
      | Queen -> 'q'
      | Rook -> 'r'
      | Bishop -> 'b'
      | Knight -> 'n'
      | Pawn -> 'p'

    let to_fen = function
      | Piece (color, kind) ->
	if color = White then
	  Char.uppercase (fen_letter kind)
	else
	  Char.lowercase (fen_letter kind)
  end
