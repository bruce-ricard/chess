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

  type en_passant = EP of Coordinates.square option

  let en_passant_to_fen = function
    | EP None -> "-"
    | EP (Some square) -> Coordinates.square_to_fen square

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



exception ImpossibleMove

let move_pawn_once color to_square board =
  let open CoordinatesHelper in
  match CoordinatesHelper.pawn_moves color to_square with
    | OnePossible(from_square) ->
      begin
        match Board.on_square board square with
          | Some `Pawn ->
            {
              starting_square = from_square;
              destination_square = to_square
            }
          | _ -> raise ImpossibleMove
      end
    | TwoPossible(from_square1, from_square2) ->
      failwith "extract method above and reuse"

let move_once color board = function
  | Pawn(square) -> move_pawn_once color square board

end
