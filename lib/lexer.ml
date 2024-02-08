open Base

type state = { code : string; pos : int }
[@@deriving show, eq]

type token =
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

type token_node = { token : token; pos : int; len : int }
[@@deriving show, eq]
type token_result = Token of token_node | Eof | Invalid of int
[@@deriving show, eq]

let char (c : char) t { pos; code } =
  let sub = String.sub ~pos ~len:(String.length code - pos) code in
  if String.length sub = 0 then None
  else
    let c' = String.get code pos in
    if Char.equal c c' then Some { token = t; pos; len = 1 } else None

let lbrace = char '{' Lbrace
let rbrace = char '}' Rbrace
let lparen = char '(' Lparen
let rparen = char ')' Rparen
let equal = char '=' Equal
let ( <|> ) a b s = match a s with None -> b s | x -> x
let next_token = lbrace <|> rbrace <|> lparen <|> rparen <|> equal

let lex code =
  let rec lex' code state =
    match next_token state with
    | Some token ->
        Token token :: lex' code { state with pos = state.pos + token.len }
    | None ->
        let end_node = if state.pos = String.length state.code then Eof else Invalid state.pos in
        [
          end_node
        ]
  in
  lex' code { code; pos = 0 }
