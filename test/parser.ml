open Monkey
open Monkey.Parser
open Alcotest

let pp_program ppf p = Format.fprintf ppf "%s" (Monkey.Ast.show_program p)
let program_testable = Alcotest.testable pp_program Monkey.Ast.equal_program

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
  (check program_testable) "simple_lets"
    [
      Ast.LetStatement { Ast.ident = "x"; value = Ast.Int 5 };
      Ast.LetStatement { Ast.ident = "y"; value = Ast.Int 6 };
      Ast.LetStatement { Ast.ident = "foobar"; value = Ast.Int 838383 };
    ]
    program

let () =
  run "Parser Tests"
    [ ("let statements", [ test_case "simple_lets" `Quick test_simple_lets ]) ]
