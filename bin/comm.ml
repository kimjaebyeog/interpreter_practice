open Syntax
open! Ty
open Infer
open Env
open Eval
open Print

let exe_comm : env -> ty_env -> command -> command_result
    = fun env tenv c -> 
    let (res, tenv') = infer_cmd tenv c
    in
    match c with
    | CExp e -> 
        print_cexp_result (snd (List.hd res),eval env e);
        Command_result (env, tenv')
    | CLet (x, e) -> 
        let v = Thunk (e, env)
        in let env' = add env (x, v)
        in print_clet_result [Some x, snd (List.hd res)];
        Command_result (env', tenv')
    | CRLet (f, e) -> 
        let v = RThunk (f, e, env)
        in let env' = add env (f, v)
        in print_clet_result [Some f, snd (List.hd res)];
        Command_result (env', tenv')
    | CMRLet l -> 
        let rec mut_rec i l' env' = match l' with
            [] -> env'
            | (f, _e) :: rest -> mut_rec (i+1) rest (add env' (f, MRThunk (i,l,env)))
        in let make_result i (f, _v) = (Some f, snd (List.nth res i))
        in let fs = mut_rec 1 l Env.emptyenv
        in print_clet_result (List.mapi make_result (List.rev fs));
        Command_result (Env.append fs env, tenv')