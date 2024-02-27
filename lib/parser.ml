let parse_expression tokens =
  let open Token in
  match tokens with
  | Int i :: rest -> (Ast.Int i, rest)
  | _ -> raise (Invalid_argument "Invalid next token")

let rec parse tokens =
  let open Token in
  let statement, tokens =
    match tokens with
    | Let :: Ident ident :: Assign :: rest -> (
        let expression, rest = parse_expression rest in
        match rest with
        | Semicolon :: rest ->
            (Ast.LetStatement { ident; value = expression }, rest)
        | _ -> raise (Invalid_argument "Expected semicolon"))
    | _ ->
        print_endline "tokens left";
        print_endline (String.concat " " (List.map Token.show tokens));
        raise (Invalid_argument "Expected let")
  in
  match tokens with [] -> [ statement ] | _ -> statement :: parse tokens
