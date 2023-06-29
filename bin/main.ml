open! Syntax
open! Env
open! Eval
open! Comm
open! MyParser
open! MyLexer
open! Print

let filenames = ref []

let rec main = function
  [] -> ()
  | x :: xs ->
    try 
      let lexbuf = Lexing.from_channel x
      in let rec loop env tenv =
        try
          let comm = MyParser.main MyLexer.token lexbuf in
          let Command_result (env',tenv') = exe_comm env tenv comm in
          loop env' tenv'
        with 
          Unbound_value var_name -> print_string "Unbound_value "; print_endline var_name; loop env tenv
          | Eval_error ->  print_endline "Eval Error!"; loop env tenv
          | Ty_error ->  print_endline "Ty Error!"; loop env tenv
          | Multi_time_Pattern_bind var_name -> print_name var_name; print_endline " is bound several times in this matching"; loop env tenv
          | Parsing.Parse_error -> print_endline "Parse Error!"; loop env tenv
      in loop Env.emptyenv []
    with
      | End_of_file -> (); close_in x; main xs
      
let _ =
  Arg.parse [] (fun s -> filenames := s :: !filenames) "";
  if !filenames = []
  then main [stdin]
  else let channels = List.map (fun s -> open_in s) (List.rev !filenames) 
    in main channels