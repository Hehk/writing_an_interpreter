type expression = Identifier of string | Int of int [@@deriving show, eq]
type let_statement = { ident : string; expression : expression } [@@deriving show, eq]

type if_statement = {
  token : Token.t;
  condition : expression;
  consequence : statement list;
  alternative : statement list;
}
[@@deriving show, eq]

and statement =
  | ExpressionStatement of expression
  | LetStatement of let_statement
  | ReturnStatement of expression
[@@deriving show, eq]

let show_debug_espression = show_expression
let show_expression = function
  | Identifier i -> i
  | Int i -> string_of_int i

let show_debug_statement = show_statement
let show_statement = function
  | ExpressionStatement e ->
      show_expression e ^ ";"
  | LetStatement { ident; expression } ->
      Printf.sprintf "let %s = %s;" ident (show_expression expression)
  | ReturnStatement e ->
      Printf.sprintf "return %s;" (show_expression e)



