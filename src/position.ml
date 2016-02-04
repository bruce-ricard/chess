module Position (Board : Board.BOARD) =
struct
  type active_color = White | Black

  let color_to_fen = function
    | White -> 'w'
    | Black -> 'b'

  type castling_rights =
      KingSide | QueenSide | Both | None

  let castling_rights_to_fen = function
    | KingSide -> "k"
    | QueenSide -> "q"
    | Both -> "kq"
    | None -> ""

  type players_castling_rights =
      { white : castling_rights;
	black : castling_rights }

  let players_castling_rights_to_fen = function
    | {white = wcr; black = bcr} ->
      String.uppercase (castling_rights_to_fen wcr) ^
	(String.lowercase (castling_rights_to_fen bcr))

  let x = players_castling_rights_to_fen {white = QueenSide; black = Both}

  type rank_name = One | Two | Three | Four | Five | Six | Seven | Eight

  let rank_name_to_fen = function
    | One -> '1'
    | Two -> '2'
    | Three -> '3'
    | Four -> '4'
    | Five -> '5'
    | Six -> '6'
    | Seven -> '7'
    | Eight -> '8'

  type file_name = A | B | C | D | E | F | G | H

  let file_name_to_fen = function
    | A -> 'a'
    | B -> 'b'
    | C -> 'c'
    | D -> 'd'
    | E -> 'e'
    | F -> 'f'
    | G -> 'g'
    | H -> 'h'

  type square = Square of file_name * rank_name

  let (^$) c s = String.make 1 c ^ s

  let square_to_fen = function
    | Square (file, rank) ->
      file_name_to_fen file ^$ rank_name_to_fen rank ^$ ""

  type en_passant = EP of square option

  let en_passant_to_fen = function
    | EP None -> "-"
    | EP (Some square) -> square_to_fen square

  module type HALF_MOVES =
  sig
    type  t

    val increment : t -> t
    val make : unit ->  t
    val reset : unit -> t
    val to_fen : t -> string
  end

  module HalfMoves : HALF_MOVES =
  struct
    type t = int
    let increment moves = moves + 1
    let make () = 0
    let reset = make
    let to_fen = string_of_int
  end
    type full_moves = int

  type t =
      {
	board : Board.t;
	 active_color : active_color;
	 castling_rights : players_castling_rights ;
	 en_passant : en_passant ;
	 half_moves : HalfMoves.t ;
	 full_moves : HalfMoves.t ;
      }

  let new_position () =
    { board = Board.init_board();
      active_color = White;
      castling_rights =
	{ white = Both ; black = Both };
      en_passant = EP None;
      half_moves = HalfMoves.make ();
      full_moves = HalfMoves.make ();
    }

  let (^^) s1 s2 = s1 ^ " " ^ s2

  let position_to_fen position =
    (Board.to_fen position.board ^^
      players_castling_rights_to_fen position.castling_rights ^^
      en_passant_to_fen position.en_passant ^^
      HalfMoves.to_fen position.half_moves ^^
      HalfMoves.to_fen position.full_moves)

end
