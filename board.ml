module type PIECE =
sig
  type t
end

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

module LList =
struct
  type zero
  type 'a succ = S
  type nonsense = int succ

  type (_,_) t =
    |  Nil : (zero, 'b) t
    | Cons : ('b * ('a, 'b) t) -> ('a succ, 'b) t
end

module LListBoard =
struct
  open LList
  open Piece
  type eight = zero succ succ succ succ succ succ succ succ

  type rank = (eight,Piece.t option) LList.t
  type t = (eight,rank) LList.t

  let piece_rank color : rank =
    let pawn = Some (Piece.Piece (color, Pawn)) in
    Cons
      (pawn,
       (Cons (pawn,
	      (Cons (pawn,
		     (Cons (pawn,
			    (Cons
			       (pawn,
				(Cons (pawn,
				       (Cons (pawn,
					      (Cons (pawn, Nil)))))))))))))))

end

module ArrayBoard =
struct
  open Piece

  type rank = Piece.t option array
  type t = rank array

  let piece_rank color p1 p2 p3 p4 p5 p6 p7 p8 : rank =
    let p pn = Some (Piece (color, pn)) in
    [|p p1; p p2; p p3; p p4; p p5; p p6; p p7; p p8|]

  let pawn_rank color =
    piece_rank color Pawn Pawn Pawn Pawn Pawn Pawn Pawn Pawn

  let first_rank color =
    piece_rank color
      Rook Knight Bishop Queen King Bishop Knight Rook

  let empty_rank () : rank = Array.make 8 None

  let init_board () =
    let board = Array.make 8 [||] in
    board.(0) <- first_rank Black;
    board.(1) <- pawn_rank Black;
    board.(2) <- empty_rank ();
    board.(3) <- empty_rank ();
    board.(4) <- empty_rank ();
    board.(5) <- empty_rank ();
    board.(6) <- pawn_rank White;
    board.(7) <- first_rank White;
    board

  type fen_list = FenPiece of Piece.t | Blanks of int

  let rec rank_to_list (rank : rank) blanks = function
    | 8 -> if blanks > 0 then [Blanks(blanks)] else []
    | n ->
      begin
	match rank.(n), blanks with
	  | None, _ -> rank_to_list rank (blanks + 1) (n + 1)
	  | Some(piece), 0 ->
	    FenPiece(piece) :: rank_to_list rank 0 (n + 1)
	  | Some(piece), blanks ->
	    Blanks(blanks) :: FenPiece(piece) :: rank_to_list rank 0 (n + 1)
      end

  let rec board_to_list (board : t) = function
    | 8 -> []
    | n -> rank_to_list board.(n) 0 0 :: board_to_list board (n+ 1)

  let rec rank_list_to_fen = function
    | [] -> ""
    | Blanks n :: q -> string_of_int n ^ rank_list_to_fen q
    | FenPiece piece :: q ->
      String.make 1 (Piece.to_fen piece) ^ rank_list_to_fen q

  let rec board_list_to_fen = function
    | [rank] -> rank_list_to_fen rank
    | rank :: ranks -> rank_list_to_fen rank ^ "/" ^ board_list_to_fen ranks
    | [] -> failwith "impossible"

  let board_to_fen board =
    board_list_to_fen (board_to_list board 0)

  let x = board_to_fen (init_board ())

  let _ = print_endline x
end;;

ArrayBoard.init_board ();;

module type BOARD =
sig
  type t
  val init_board : unit -> t
end

module Position (Board : BOARD)=
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
  end

  module HalfMoves : HALF_MOVES =
  struct
    type t = int
    let increment moves = moves + 1
    let make () = 0
    let reset = make
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

  let position_to_fen =()
end
