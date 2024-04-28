open Base
open Stdio

type state = { code : string; pos : int } [@@deriving show, eq]

type token_node = { token : Token.t; pos : int; len : int }
[@@deriving show, eq]

type token_result = Token of token_node | Eof | Invalid of int
[@@deriving show, eq]

let ( % ) f g x = f (g x)

let get_tokens nodes =
  List.filter_map nodes ~f:(function
    | Token t -> Some t.token
    | Invalid _ -> raise (Invalid_argument "invalid token")
    | Eof -> None)

module Test_Helpers = struct
  let uncurry f (a, b) = f a b

  let compare_token_list a b =
    let show_token_list =
      Printf.sprintf "[%s]" % String.concat ~sep:"; "
      % List.map
          ~f:
            (String.substr_replace_first ~pattern:"Lexer." ~with_:""
            % Token.show)
    in
    let on_mismatch a b =
      printf "Expected: %s\nGot: %s\n" (show_token_list b) (show_token_list a)
    in
    match List.zip a b with
    | List.Or_unequal_lengths.Ok zipped ->
        let equals = List.map zipped ~f:(uncurry Token.equal) in
        if List.for_all equals ~f:(fun x -> x) then ()
        else
          let () = on_mismatch a b in
          raise (Invalid_argument "Lists are not equal")
    | List.Or_unequal_lengths.Unequal_lengths ->
        on_mismatch a b;
        raise (Invalid_argument "Lists are not of equal length")
end

let char (c : char) t { pos; code } =
  let sub = String.sub ~pos ~len:(String.length code - pos) code in
  if String.length sub = 0 then None
  else
    let c' = String.get code pos in
    if Char.equal c c' then Some { token = t; pos; len = 1 } else None

let string (s : string) t { pos; code } =
  let sub = String.sub ~pos ~len:(String.length code - pos) code in
  if String.length sub < String.length s then None
  else
    let sub' = String.sub ~pos ~len:(String.length s) code in
    if String.equal sub' s then Some { token = t; pos; len = String.length s }
    else None

let advance_while is_valid { pos; code } =
  let sub = String.sub ~pos ~len:(String.length code - pos) code in
  let safe_get s i =
    if i < String.length s then Some (String.get s i) else None
  in
  let is_valid = Option.value ~default:false % Option.map ~f:is_valid in
  let rec loop i =
    if i = String.length sub then i
    else if is_valid @@ safe_get sub i then loop (i + 1)
    else i
  in
  loop 0

let take_while is_valid f { pos; code } =
  let len = advance_while is_valid { pos; code } in
  if len = 0 then None
  else
    let token = f @@ String.sub ~pos ~len code in
    Some { token; pos; len }

let lbrace = char '{' Lbrace
let rbrace = char '}' Rbrace
let lparen = char '(' Lparen
let rparen = char ')' Rparen
let assign = char '=' Assign
let plus = char '+' Plus
let minus = char '-' Minus
let divide = char '/' Divide
let multiply = char '*' Multiply
let less = char '<' Less
let greater = char '>' Greater
let comma = char ',' Comma
let semicolon = char ';' Semicolon
let equal = string "==" Equal
let not' = char '!' Not
let not_equal = string "!=" NotEqual
let int' = take_while Char.is_digit (fun s -> Int (Int.of_string s))

let check_keywords =
  Token.(
    function
    | Identifier "let" -> Let
    | Identifier "fn" -> Function
    | Identifier "for" -> For
    | Identifier "true" -> True
    | Identifier "false" -> False
    | Identifier "if" -> If
    | Identifier "else" -> Else
    | Identifier "return" -> Return
    | x -> x)

let identifier =
  take_while
    (fun c -> Char.is_alpha c || Char.is_digit c || Char.equal c '_')
    (fun s -> Identifier s)

let ( <|> ) a b s = match a s with None -> b s | x -> x
let ( >>= ) a f s = match a s with None -> None | Some x -> f x

let ( >>| ) a f s =
  match a s with None -> None | Some x -> Some { x with token = f x.token }

let ( *> ) a b s = match a s with None -> None | _ -> b s

let next_token =
  equal <|> not_equal <|> not' <|> less <|> greater <|> minus <|> divide
  <|> multiply <|> lbrace <|> rbrace <|> lparen <|> rparen <|> assign <|> plus
  <|> comma <|> semicolon <|> int'
  <|> (identifier >>| check_keywords)

let skip_whitespace { pos; code } =
  let len = advance_while Char.is_whitespace { pos; code } in
  { code; pos = pos + len }

let lex code =
  let rec lex' code state =
    let state = skip_whitespace state in
    match next_token state with
    | Some token ->
        Token token :: lex' code { state with pos = state.pos + token.len }
    | None ->
        let end_node =
          if state.pos = String.length state.code then Eof
          else Invalid state.pos
        in
        [ end_node ]
  in
  lex' code { code; pos = 0 }

let%test_unit "lex" =
  let code = "=+(){}" in
  code |> lex |> get_tokens
  |> Test_Helpers.compare_token_list
       [ Assign; Plus; Lparen; Rparen; Lbrace; Rbrace ]

let%test_unit "lex operators" =
  let code = "!-/*5;\n5 < 10 > 5;" in
  code |> lex |> get_tokens
  |> Test_Helpers.compare_token_list
       [
         Not;
         Minus;
         Divide;
         Multiply;
         Int 5;
         Semicolon;
         Int 5;
         Less;
         Int 10;
         Greater;
         Int 5;
         Semicolon;
       ]

let%test_unit "lex" =
  let code = "let forty = 40;" in
  code |> lex |> get_tokens
  |> Test_Helpers.compare_token_list
       [ Let; Identifier "forty"; Assign; Int 40; Semicolon ]

let%test_unit "lex" =
  let code =
    {|if (5 < 10) {
      return true;
    } else {
      return true;
    }|}
  in
  code |> lex |> get_tokens
  |> Test_Helpers.compare_token_list
       [
         If;
         Lparen;
         Int 5;
         Less;
         Int 10;
         Rparen;
         Lbrace;
         Return;
         True;
         Semicolon;
         Rbrace;
         Else;
         Lbrace;
         Return;
         True;
         Semicolon;
         Rbrace;
       ]

let result_content code = function
  | Token t -> String.sub ~pos:t.pos ~len:t.len code
  | Invalid pos -> String.sub ~pos ~len:(String.length code - pos) code
  | Eof -> "EOF"
