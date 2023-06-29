open Syntax

let emptyenv : env = []
let rec find : env -> name -> thunk option
    = fun env name -> match env with
    [] -> None
    | (name', thunk) :: env' -> if name = name' then Some thunk else find env' name 
let add : env -> (name * thunk) -> env
    = fun env x -> x :: env
let append : env -> env -> env
    = ( @ )