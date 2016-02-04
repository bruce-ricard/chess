
module LList =
struct
  type zero
  type 'a succ = S

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
