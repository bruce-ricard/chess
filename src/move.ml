type rank = One | Two | Three | Four | Five | Six | Seven | Eight

type file = A | B | C | D | E | F | G | H

type square = Square of file * rank

type piece_move = ToSquare square
		  | FileToSquare file * square
		  | RankToSquare rank * square

type move =
    Pawn of Square
  | PawnCapture of file * file
