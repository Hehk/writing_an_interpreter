open Monkey.Parser
open Alcotest

let simple_lets = {|
let x = 5;
let y = 6;
let foobar = 838383;
|}

let test_simple_lets () =
  let tokens = Monkey.Lexer.lex simple_lets |> Monkey.Lexer.get_tokens in
  let () =
    tokens |> List.map Monkey.Token.show
    |> List.fold_left (fun a b -> a ^ ", " ^ b) ""
    |> print_endline
  in
  let program = parse tokens in
  let () = print_endline @@ Monkey.Ast.show_program program in
  check bool "simple_lets" true true

let () =
  run "Parser Tests"
    [ ("let statements", [ test_case "simple_lets" `Quick test_simple_lets ]) ]
