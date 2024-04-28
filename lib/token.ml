type t =
  | Identifier of string
  | Int of int
  | Assign
  | Plus
  | Minus
  | Divide
  | Multiply
  | Equal
  | NotEqual
  | Not
  | Less
  | Greater
  | Comma
  | Semicolon
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Function
  | Let
  | For
  | True
  | False
  | If
  | Else
  | Return
[@@deriving show, eq]

