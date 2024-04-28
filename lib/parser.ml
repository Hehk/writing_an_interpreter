open Base

type parse_error =
  | SyntaxError of string
  | Mismatch 
  [@@deriving show, eq]

type program = {
  statements : Ast.statement list;
  syntax_errors : parse_error list;
}
[@@deriving show, eq]

let (let*) x f = Result.bind x ~f

let show_list show l = List.fold ~init:"" ~f:(fun acc x -> acc ^ " " ^ show x) l

let peek token tokens =
  match tokens with
  | [] -> false
  | t :: _ -> Token.equal t token

let recover_at token tokens =
  let _, rest =
    List.split_while tokens ~f:(fun t -> not (Token.equal t token))
  in
  match rest with
  | [] -> []
  | _ :: rest -> rest

let parse_expression tokens =
  let open Token in
  match tokens with
  | Int i :: rest -> 
      Ok (Ast.Int i, rest)
  | Identifier ident :: rest -> 
      Ok (Ast.Identifier ident, rest)
  | _ -> Error (SyntaxError("Unimplemented Expression: " ^ show_list Token.show tokens))

let parse_expression_statement token =
  let* expr, rest = parse_expression token in
  Ok (Ast.ExpressionStatement expr, rest)

let parse_let tokens =
  let open Token in
  match tokens with
  | Let :: Identifier ident :: Assign :: rest -> (
      let* expression, rest = parse_expression rest in
      Ok(Ast.LetStatement { ident; expression }, rest)
  )
  | _ ->
    Error (Mismatch) 

let parse_return tokens =
  let open Token in
  match tokens with
  | Return :: rest -> (
      let* expression, rest = parse_expression rest in
      Ok(Ast.ReturnStatement expression, rest)
  )
  | _ -> Error Mismatch

(* I don't like these next two functions that much... *)
(* Trying to make this like or *)
let (<|>) f g x =
  match f x with
  | Ok _ as ok -> ok
  | Error Mismatch -> g x
  | Error _ as error -> error

let with_semicolon f tokens =
  let* statement, rest = f tokens in
  match rest with
  | Token.Semicolon :: rest -> Ok (statement, rest)
  | _ -> Error (SyntaxError "Expected Semicolon")

let parse_statement =
  with_semicolon parse_let <|> with_semicolon parse_return <|> parse_expression_statement

let parse tokens =
  let rec parse' tokens program =
    match tokens with
    | [] -> program
    | _ ->
        match parse_statement tokens with
        | Error error ->
            let program = { program with syntax_errors = error :: program.syntax_errors } in
            let rest = recover_at Semicolon tokens in
            parse' rest program
        | Ok (statement, rest) ->
            let program = { program with statements = statement :: program.statements } in
            parse' rest program
  in
  let program = parse' tokens { statements = []; syntax_errors = [] } in
  { statements = List.rev program.statements; syntax_errors = List.rev program.syntax_errors }
