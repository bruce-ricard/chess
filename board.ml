module type PIECE =
sig
  type t
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
