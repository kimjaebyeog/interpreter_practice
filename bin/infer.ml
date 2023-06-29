open Syntax
open Ty

let infer_expr : ty_env -> expr -> ty * ty_env
    = fun tenv e ->
    let (t, c) = gather_ty_constraints tenv e
    in let s = ty_unify c
    in (apply_ty_subst s t, List.map (fun (x,t) -> (x, apply_ty_subst_to_ty_scheme s t)) tenv)

let infer_cmd : ty_env -> command -> ty_env * ty_env
    = fun tenv cmd ->
    match cmd with
        CExp e -> 
            let (t, tenv') = infer_expr tenv e
            in ([("",scheme_of_ty t)],tenv')
        | CLet (x,e)-> 
            let (t, tenv') = infer_expr tenv e
            in ([(x,scheme_of_ty t)], (x,scheme_of_ty t)::tenv')
        | CRLet (x, e) ->
            let a = new_ty_var () in
            let (t, tenv') = infer_expr ((x,([], TVar a))::tenv) e
            in ([(x,scheme_of_ty t)], (x,scheme_of_ty t)::tenv')
        | CMRLet l -> 
            let types = List.map (fun _ -> (TVar (new_ty_var ()))) l in
            let tenv' = (List.map2 (fun (f,_) t -> (f,([], t))) l types)@tenv in
            let tncs = 
            (* list of (t,c) *)
                (List.map (fun (_,e) -> gather_ty_constraints (tenv') e) l) in
            let crs = List.concat (List.mapi (fun i (t,c) ->
                let t2 = List.nth types i
                in (t,t2)::c) tncs) in 
            let sigma = ty_unify crs in
            let ss = List.map (fun (t,_) -> apply_ty_subst sigma t) tncs in
            let x = List.map2 (fun (f,_) s -> (f,scheme_of_ty s)) l ss in
            let tenv'' = List.map (fun (x, sc)-> (x,apply_ty_subst_to_ty_scheme sigma sc)) tenv
            in (x,x@tenv'')
