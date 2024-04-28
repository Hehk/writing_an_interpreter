open Monkey
open Monkey.Parser
open Alcotest

let pp_program ppf p = Format.fprintf ppf "%s" (Monkey.Parser.show_program p)
let program_testable = Alcotest.testable pp_program Monkey.Parser.equal_program
let to_program statements = { statements; syntax_errors = [] }

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
    (to_program [
      Ast.LetStatement { Ast.ident = "x"; expression = Ast.Int 5 };
      Ast.LetStatement { Ast.ident = "y"; expression = Ast.Int 6 };
      Ast.LetStatement { Ast.ident = "foobar"; expression = Ast.Int 838383 };
    ])
    program

let lets_with_errors = {|
let x 5;
let = 10;
let 838383;
|}

let test_lets_with_errors () =
  let tokens = Monkey.Lexer.lex lets_with_errors |> Monkey.Lexer.get_tokens in
  let () =
    tokens |> List.map Monkey.Token.show
    |> List.fold_left (fun a b -> a ^ ", " ^ b) ""
    |> print_endline
  in
  let program = parse tokens in
  (check int) "syntax_errors" 3 (List.length program.syntax_errors)

let return_statements = {|
return 5;
return 10;
return 838383;
|}

let test_return_statements () =
  let tokens = Monkey.Lexer.lex return_statements |> Monkey.Lexer.get_tokens in
  let () =
    tokens |> List.map Monkey.Token.show
    |> List.fold_left (fun a b -> a ^ ", " ^ b) ""
    |> print_endline
  in
  let program = parse tokens in
  (check program_testable) "return_statements"
    (to_program [
      Ast.ReturnStatement (Ast.Int 5);
      Ast.ReturnStatement (Ast.Int 10);
      Ast.ReturnStatement (Ast.Int 838383);
    ])
    program

let test_input input () = 
  let input = String.trim input in
  let tokens = Monkey.Lexer.lex input |> Monkey.Lexer.get_tokens in
  let program = parse tokens in
  let program_content = program.statements |> List.map Ast.show_statement |> String.concat "\n" in
  pp_program Format.std_formatter program;
  (check string) "test_input" input program_content

let () =
  run "Parser Tests"
    [
      ( "let statements",
        [
          test_case "simple_lets" `Quick test_simple_lets;
          test_case "lets_with_errors" `Quick test_lets_with_errors;
          test_case "simple_lets show" `Quick (test_input simple_lets);
        ] );
      ( "return statements",
        [
          test_case "return_statements" `Quick test_return_statements;
          test_case "return_statements show" `Quick (test_input return_statements);
        ] );
      ( "expressions",
        [
          test_case "foobar" `Quick (test_input "foobar;");
        ] );
    ]
