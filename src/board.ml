module type BOARD =
sig
  type t
  type descriptive_move
  val init_board : unit -> t
(*  val on_square : t -> Coordinates.square -> Piece.t option*)
  val move : descriptive_move -> t -> t
  val to_fen : t -> string
end

module ArrayBoard : BOARD =
struct
  open Piece

  type rank = Piece.t option array
  type t = rank array
  type square = Square of Coordinates.file * Coordinates.rank

  type descriptive_move = {
    starting_square : Coordinates.square;
    destination_square : Coordinates.square
  }


  let square_to_fen = function
    | Square (file, rank) ->
      file_name_to_fen file ^$ rank_name_to_fen rank ^$ ""


  let piece_rank color p1 p2 p3 p4 p5 p6 p7 p8 : rank =
    let p pn = Some (Piece (color, pn)) in
    [|p p1; p p2; p p3; p p4; p p5; p p6; p p7; p p8|]

  let pawn_rank color =
    piece_rank color `Pawn `Pawn `Pawn `Pawn `Pawn `Pawn `Pawn `Pawn

  let first_rank color =
    piece_rank color
      `Rook `Knight `Bishop `Queen `King `Bishop `Knight `Rook

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

  let rec rank_to_list rank blanks = function
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
    | n -> rank_to_list board.(n) 0 0 :: board_to_list board (n + 1)

  let rec rank_list_to_fen = function
    | [] -> ""
    | Blanks n :: q -> string_of_int n ^ rank_list_to_fen q
    | FenPiece piece :: q ->
      String.make 1 (Piece.to_fen piece) ^ rank_list_to_fen q

  let rec board_list_to_fen = function
    | [rank] -> rank_list_to_fen rank
    | rank :: ranks -> rank_list_to_fen rank ^ "/" ^ board_list_to_fen ranks
    | [] -> failwith "impossible"

  let to_fen board =
    board_list_to_fen (board_to_list board 0)

let rank_name_to_position = let open Coordinates in function
  | One -> 0
  | Two -> 1
  | Three -> 2
  | Four -> 3
  | Five -> 4
  | Six ->  5
  | Seven -> 6
  | Eight -> 7

let file_name_to_position = let open Coordinates in function
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3
  | E -> 4
  | F -> 5
  | G -> 6
  | H -> 7

let square_to_position = let open Coordinates in function
Square (file, rank) -> file_name_to_position file, (rank_name_to_position rank)

let on_square board s =
  let y,x = square_to_position s in
  board.(x).(y)

let move init_square end_square board =
  let x0,y0 = square_to_position init_square
  and x1,y1 = square_to_position end_square in

  board.(y1).(x1) <- board.(y0).(x0);
  board.(y0).(x0) <- None;
  board
end
