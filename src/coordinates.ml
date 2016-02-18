include CoordinatesTypes

let rank_name_to_fen = function
  | One -> '1'
  | Two -> '2'
  | Three -> '3'
  | Four -> '4'
  | Five -> '5'
  | Six -> '6'
  | Seven -> '7'
  | Eight -> '8'

let file_name_to_fen = function
  | A -> 'a'
  | B -> 'b'
  | C -> 'c'
  | D -> 'd'
  | E -> 'e'
  | F -> 'f'
  | G -> 'g'
  | H -> 'h'

let (^$) c s = String.make 1 c ^ s

let square_to_fen = function
  | Square (file, rank) ->
    file_name_to_fen file ^$ rank_name_to_fen rank ^$ ""
