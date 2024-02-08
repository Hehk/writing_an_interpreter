type t =
  | Ident
  | Int of int
  | Assign
  | Plus
  | Comma
  | Semicolon
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Equal
  | Function
  | Let
  [@@deriving show, eq]

