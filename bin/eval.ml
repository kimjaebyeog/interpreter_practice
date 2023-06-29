open Syntax
open Env


let rec eval : env -> expr -> value 
    = fun env e ->
    try begin
        match e with
        EValue v -> v
        | EVar x -> begin
            match find env x with
            Some (Thunk (e', env')) -> eval env' e'
            | Some (RThunk (f, e', env')) -> eval (add env' (f, RThunk(f, e', env'))) e'

            | Some (MRThunk (i, l, env)) ->
                let rec mut_rec i l' env' = match l' with
                    [] -> env'
                    | (f, _e) :: rest -> mut_rec (i+1) rest (add env' (f, MRThunk (i,l,env)))
                in eval (mut_rec 1 l env) (snd (List.nth l (i - 1)))
            | None -> raise (Unbound_value x)
            end
        (* 먼저 EBin (Op, e1,e2) 로 match 시키고 하위에서 match사용 *)
        | EBin (OpAdd, e1, e2) -> begin
            match (eval env e1, eval env e2) with 
            (VInt v1,VInt v2) -> VInt (v1+v2)
            | _ -> raise Eval_error
            (* todo : match failure with a massage *)
        end
        | EBin (OpSub, e1, e2) -> begin
            match (eval env e1, eval env e2) with 
            (VInt v1,VInt v2) -> VInt (v1-v2)
            | _ -> raise Eval_error
        end
        | EBin (OpMul, e1, e2) -> begin
            match (eval env e1, eval env e2) with 
            (VInt v1,VInt v2) -> VInt (v1*v2)
            | _ -> raise Eval_error
        end
        | EBin (OpDiv, e1, e2) -> begin
            match (eval env e1, eval env e2) with 
            (VInt v1,VInt v2) -> VInt (v1/v2)
            | _ -> raise Eval_error
        end
        | EBin (OpEq, e1, e2) -> begin 
            match (eval env e1, eval env e2) with
            (VInt v1, VInt v2) -> VBool (v1 = v2)
            | (VBool v1, VBool v2) -> VBool (v1 = v2)
            | (VCons (v11, v12), VCons (v21, v22)) ->
                if (eval env (EBin (OpEq, EValue (eval env (EThunk (v11))), EValue (eval env (EThunk (v21))))) = VBool true)
                && (eval env (EBin (OpEq, EValue (eval env (EThunk (v12))), EValue (eval env (EThunk (v22))))) = VBool true)
                then VBool true
                else VBool false
            | (VTuple (v11, v12), VTuple (v21, v22)) ->
                if (eval env (EBin (OpEq, EValue (eval env (EThunk (v11))), EValue (eval env (EThunk (v21))))) = VBool true)
                && (eval env (EBin (OpEq, EValue (eval env (EThunk (v12))), EValue (eval env (EThunk (v22))))) = VBool true)
                then VBool true
                else VBool false
            | (VNil, VNil) -> VBool true
            | _ -> VBool false
        end
        | EBin (OpLt, e1, e2) -> begin 
            match (eval env e1, eval env e2) with
            (VInt v1, VInt v2) -> VBool (v1 < v2)
            | (VBool v1, VBool v2) -> VBool (v1 < v2)
            | (VCons (v11, v12), VCons (v21, v22)) ->
                let (ev11, ev21) =  EValue (eval env (EThunk (v11))), EValue (eval env (EThunk (v21))) in
                if (eval env (EBin (OpLt, ev11, ev21)) = VBool true)
                then VBool true
                else if (eval env (EBin (OpEq, ev11, ev21)) = VBool true)
                then if (eval env (EBin (OpLt, EValue (eval env (EThunk (v12))), EValue (eval env (EThunk (v22))))) = VBool true)
                then VBool true
                else VBool false
                else VBool false
            | (VTuple (v11, v12), VTuple (v21, v22)) ->
                let (ev11, ev21) =  EValue (eval env (EThunk (v11))), EValue (eval env (EThunk (v21))) in
                if (eval env (EBin (OpLt, ev11, ev21)) = VBool true)
                then VBool true
                else if (eval env (EBin (OpEq, ev11, ev21)) = VBool true)
                then if (eval env (EBin (OpLt, EValue (eval env (EThunk (v12))), EValue (eval env (EThunk (v22))))) = VBool true)
                then VBool true
                else VBool false
                else VBool false
            | _ -> VBool false
        end
        | EIf (e0, e1, e2) -> begin
            match eval env e0 with
            VBool b -> if b then eval env e1 else eval env e2
            | _ -> raise Eval_error
        end
        | ELet (x, e1, e2) -> eval (add env (x, Thunk (e1, env))) e2
        | ERLet (f, e1, e2) -> let env' = add env (f, RThunk (f, e1, env))
            in eval env' e2
        | EMRLet (l, e) -> 
            let rec mut_rec i l' env' = match l' with
                [] -> env'
                | (f, _e) :: rest -> mut_rec (i+1) rest (add env' (f, MRThunk (i,l,env)))
            in eval (mut_rec 1 l env) e
        | EFun (x, e) -> VFun (x, e, env)
        | EApp (e1, e2) -> begin
            match eval env e1 with
            VFun (x, e, oenv) ->
                eval (add oenv (x, Thunk (e2, env))) e
            | _ -> raise Eval_error
            end
        | EMatch (e, l) ->
            let rec find_match : env -> pattern -> expr -> env option
                = fun env' p v -> match p with
                PValue pv -> if eval env (EBin (OpEq, EValue pv, v)) = VBool true 
                    then Some env' else None
                | PVar px ->
                   Some (add env' (px, Thunk (v,env')))
                | PList (p1, p2) -> begin 
                    match eval env v with
                    VCons (t1, t2) -> begin
                        match find_match env' p1 (EThunk t1) with
                        Some env'' -> find_match env'' p2 (EThunk t2)
                        | None -> None
                        end 
                    | _ -> None
                    end
                | PTuple (p1, p2) -> begin 
                    match eval env v with
                    VTuple (t1, t2) -> begin
                        match find_match env' p1 (EThunk t1) with
                        Some env'' -> find_match env'' p2 (EThunk t2)
                        | None -> None
                        end 
                    | _ -> None
                    end
                | PWild -> Some env'
            in let rec pattern_match : expr -> (pattern * expr) list -> value
                = fun v l -> match l with
                [] -> raise Eval_error
                | (p, e2) :: l' ->
                    match find_match env p v with
                    None -> pattern_match v l'
                    | Some env' -> eval env' e2
            in pattern_match e l
        | EList (e1, e2) -> 
            VCons (Thunk (e1,env),Thunk (e2,env))
        | ETuple (e1, e2) -> 
            VTuple (Thunk (e1,env),Thunk (e2,env))
        | EThunk (Thunk (e', env')) -> eval env' e'
        | EThunk (RThunk (f, e', env')) -> eval (add env' (f, RThunk(f, e', env'))) e'
        | EThunk (MRThunk (i, l, env)) ->
                let rec mut_rec i l' env' = match l' with
                    [] -> env'
                    | (f, _e) :: rest -> mut_rec (i+1) rest (add env' (f, MRThunk (i,l,env)))
                in eval (mut_rec 1 l env) (snd (List.nth l (i - 1)))
    end
    with Match_failure _ -> raise Eval_error
    (* todo : error message *)