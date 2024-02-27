type expression = Identifier of Token.t | Int of int [@@deriving show, eq]
type let_statement = { ident : string; value : expression } [@@deriving show, eq]

type return_statement = { token : Token.t; value : expression }
[@@deriving show, eq]

type if_statement = {
  token : Token.t;
  condition : expression;
  consequence : statement list;
  alternative : statement list;
}
[@@deriving show, eq]

and statement =
  | LetStatement of let_statement
  | ReturnStatement of return_statement
  | IfStatement of if_statement
[@@deriving show, eq]

type program = statement list
[@@deriving show, eq]
