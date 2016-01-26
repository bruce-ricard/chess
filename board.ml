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

  type row = (eight,Piece.t option) LList.t
  type t = (eight,row) LList.t

  let piece_row color : row =
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

  let row2 = pawn_row Black
  let row7 = pawn_row White
end

module ArrayBoard =
struct
  open Piece

  type row = Piece.t option array
  type t = row array

  let piece_row color p1 p2 p3 p4 p5 p6 p7 p8 : row =
    let p pn = Some (Piece (color, pn)) in
    [|p p1; p p2; p p3; p p4; p p5; p p6; p p7; p p8|]

  let pawn_row color =
    piece_row color Pawn Pawn Pawn Pawn Pawn Pawn Pawn Pawn

  let first_row color =
    piece_row color
      Rook Knight Bishop Queen King Bishop Knight Rook

  let empty_row () : row = Array.make 8 None

  let init_board () =
    let board = Array.make 8 [||] in
    board.(0) <- first_row White;
    board.(1) <- pawn_row White;
    board.(2) <- empty_row ();
    board.(3) <- empty_row ();
    board.(4) <- empty_row ();
    board.(5) <- empty_row ();
    board.(6) <- pawn_row Black;
    board.(7) <- first_row Black;
    board
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
    | One -> "1"
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
      
  type file_name = A | B | C | D | E | F | G | H
  type square = Square of file_name * rank_name
    
  type en_passant = EP of square option

  let en_passant_to_fen = function
    | EP None -> ""
    
  type half_moves = int
  type full_moves = int
	
  type t =
      {
	board : Board.t;
	 active_color : active_color;
	 castling_rights : players_castling_rights ;
	 en_passant : en_passant ;
	 half_moves : half_moves ;
	 full_moves : full_moves ;
      }
end
