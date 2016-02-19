type promotable = [ `Queen | `Rook | `Bishop | `Knight ]
type non_pawn = [ `King | promotable ]
type piece_kind = [ non_pawn | `Pawn ]
type color = White | Black
type t = Piece of color * piece_kind

let (fen_letter : piece_kind -> char) = function
  | `King -> 'k'
  | `Queen -> 'q'
  | `Rook -> 'r'
  | `Bishop -> 'b'
  | `Knight -> 'n'
  | `Pawn -> 'p'

let to_fen = function
  | Piece (color, kind) ->
    if color = White then
      Char.uppercase (fen_letter kind)
    else
      Char.lowercase (fen_letter kind)
