open Syntax

let tv : ty_var ref = ref 0

let new_ty_var : unit -> ty_var
    = fun () -> incr tv; !tv

let rec apply_ty_subst : ty_subst -> ty -> ty
    = fun s t ->
    match t with
    TVar i1 -> begin
        match s with
        [] -> t
        | (i2, t2) :: rest ->
            if i1 = i2 then t2 else apply_ty_subst rest t
            (* todo : List.assoc *)
        end
    | TFun (t11, t12) ->
        TFun (apply_ty_subst s t11, apply_ty_subst s t12)
    | TList t -> 
        TList (apply_ty_subst s t)
    | TTuple (t11, t12) -> 
        TTuple (apply_ty_subst s t11, apply_ty_subst s t12)
    | _ -> t

let rec apply_ty_subst_to_ty_scheme : ty_subst -> ty_scheme -> ty_scheme
    = fun s sc ->
    match sc with
    (l, TVar i1) -> begin
        if List.mem i1 l then sc
        else match List.assoc_opt i1 s with
            None -> sc
            | Some t2 -> (l,t2)
        end
    | (l,TFun (t11, t12))-> 
        let (_,t1) = apply_ty_subst_to_ty_scheme s (l,t11) in
        let (_,t2) = apply_ty_subst_to_ty_scheme s (l,t12) in
        (l,TFun (t1, t2))
    | (l,TList t11) -> 
        let (_,t1) = apply_ty_subst_to_ty_scheme s (l,t11) in
        (l,TList (t1))
    | (l,TTuple (t11, t12)) ->
        let (_,t1) = apply_ty_subst_to_ty_scheme s (l,t11) in
        let (_,t2) = apply_ty_subst_to_ty_scheme s (l,t12) in
        (l,TTuple (t1, t2))
    | _ -> sc

let rec make_new_ty : ty_scheme -> ty
    = fun sc ->
    match sc with
    ([], t) -> t
    | (x :: xs, t) -> apply_ty_subst [(x, (TVar (new_ty_var ())))] (make_new_ty (xs, t))

let scheme_of_ty : ty -> ty_scheme
    = fun t ->
    let rec remove_free_tvar : ty_scheme -> ty_scheme
        = fun sc ->
            match sc with
            (l, TVar i1) ->
                if List.mem i1 l then sc
                else (i1 :: l, TVar i1)
            | (l,TFun (t11, t12))-> 
                let (l',t1) = remove_free_tvar (l,t11) in
                let (l'',t2) = remove_free_tvar (l',t12) in
                (l'',TFun (t1, t2))
            | (l,TList t11) -> 
                let (l',t1) = remove_free_tvar (l,t11) in
                (l',TList (t1))
            | (l,TTuple (t11, t12)) ->
                let (l',t1) = remove_free_tvar (l,t11) in
                let (l'',t2) = remove_free_tvar (l',t12) in
                (l'',TTuple (t1, t2))
            | _ -> sc in
    remove_free_tvar ([],t)

let rec compose_ty_subst : ty_subst -> ty_subst -> ty_subst
    = fun s1 s2 ->
    let rec subst_exist : ty -> ty_subst -> bool
        = fun t s ->
        match t with
        TVar i1 -> begin
            match s with
            [] -> false
            | (i2, _) :: rest -> if i1 = i2 then true else subst_exist t rest
            end
        | _ -> false
    in let rec compose_rest : ty_subst -> ty_subst
        = fun s ->
        match s with
        [] -> []
        | (i, t) :: rest ->
            if subst_exist (TVar i) s2 then compose_rest rest
            else (i, t) :: compose_rest rest
    in match s2 with
    [] -> compose_rest s1
    | (i2, t2) :: rest -> (i2, apply_ty_subst s1 t2) :: (compose_ty_subst s1 rest)

let rec ty_unify : ty_constraints -> ty_subst
    = fun c ->
    let rec is_include : int -> ty -> bool
    (* occurs check *)
        = fun i t -> match t with
            TVar i' -> i = i'
            | TList t' -> is_include i t'
            | TFun (t1', t2') -> (is_include i t1') || (is_include i t2')
            | _ -> false
    in match c with
    [] -> []
    | (t1,t2) :: rest when t1 = t2 -> ty_unify rest
    | (TFun (t11, t12), TFun (t21, t22)) :: rest ->
        ty_unify ((t11,t21)::(t12,t22)::rest)
    | (TList t1', TList t2') :: rest ->
        ty_unify ((t1',t2')::rest)
    | (TTuple (t11, t12), TTuple (t21, t22)) :: rest ->
        ty_unify ((t11,t21)::(t12,t22)::rest)
    | (TVar i, t2) :: rest ->
        if is_include i t2 then raise Ty_error
        else compose_ty_subst
                (ty_unify (List.map 
                    (fun (a, b) -> (apply_ty_subst [(i, t2)] a, apply_ty_subst [(i, t2)] b)) 
                    rest))
                [(i, t2)] 
    | (t1, TVar i) :: rest -> ty_unify ((TVar i, t1):: rest)
    | _ -> raise Ty_error

let rec gather_ty_constraints : ty_env -> expr -> ty * ty_constraints
(* should this be in infer.ml? *)
    = fun tenv e ->
    match e with
    EValue v -> begin
        match v with
        VInt _ -> (TInt, [])
        | VBool _ -> (TBool, [])
        | VNil -> 
            let a = new_ty_var ()
            in (TList (TVar a), [])
        | _ -> raise Ty_error 
    end
    | EVar x -> begin
        match List.assoc_opt x tenv with
        None -> raise (Unbound_value x)
        | Some sc -> (make_new_ty sc, [])
    end
    | EBin (OpAdd, e1, e2) | EBin (OpSub, e1, e2) | EBin (OpMul, e1, e2) | EBin (OpDiv, e1, e2) -> 
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in (TInt, (t1,TInt)::(t2,TInt)::c1@c2)
    | EBin (OpEq, e1, e2) | EBin (OpLt, e1, e2) -> 
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in (TBool, (t1,t2)::c1@c2)
    | EIf (e1, e2, e3) ->
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in let (t3, c3) = gather_ty_constraints tenv e3
        in (t2, (t1, TBool)::(t2, t3)::c1@c2@c3)
    | ELet (x, e1, e2) ->
        let (t1, c1) = gather_ty_constraints tenv e1 in
        let sigma = ty_unify c1 in
        let s1 = apply_ty_subst sigma t1 in
        let tenv' = List.map (fun (x, sc)-> (x,apply_ty_subst_to_ty_scheme sigma sc)) tenv
        in let (t2, c2) = gather_ty_constraints ((x,scheme_of_ty s1)::tenv') e2
        in (t2, c1 @ c2)
    | EFun (x, e1) ->
        let a = new_ty_var ()
        in let (t, c) = gather_ty_constraints ((x, ([],TVar a))::tenv) e1
        in (TFun (TVar a, t), c)
    | EApp (e1, e2) -> 
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in let a = new_ty_var ()
        in (TVar a, ((t1, TFun(t2, TVar a))::c1@c2))
    | ERLet (f, e1, e2) ->
        let a = new_ty_var () in
        let (t1, c1) = gather_ty_constraints ((f,([],TVar a))::tenv) e1 in
        let sigma = ty_unify c1 in
        let s1 = apply_ty_subst sigma t1 in
        let tenv' = List.map (fun (x, sc)-> (x,apply_ty_subst_to_ty_scheme sigma sc)) tenv in
        let (t2, c2) = gather_ty_constraints ((f,scheme_of_ty s1)::tenv') e2 in
        (t2, c1 @ c2)
    | EMRLet (l, e) ->
        let types = List.map (fun _ -> (TVar (new_ty_var ()))) l in
        let tenv' = (List.map2 (fun (f,_) t -> (f,([], t))) l types)@tenv in
        let tncs = 
        (* list of (t,c) *)
            (List.map (fun (_,e) -> gather_ty_constraints (tenv') e) l) in
        let crs = List.concat (List.mapi (fun i (t,c) ->
            let t2 = List.nth types i
            in (t,t2)::c) tncs)
        in 
        
        let sigma = ty_unify crs in
        let ss = List.map (fun (t,_) -> apply_ty_subst sigma t) tncs in
        let x = List.map2 (fun (f,_) s -> (f,scheme_of_ty s)) l ss in
        let tenv'' = List.map (fun (x, sc)-> (x,apply_ty_subst_to_ty_scheme sigma sc)) tenv in

        let (t, c) = gather_ty_constraints (x@tenv'') e
        in (t, crs@c)
    | EMatch (e, l) ->
        let rec gather_ty_constraints_pattern : pattern -> ty * ty_env * ty_constraints
            = fun p -> match p with
            PValue (VInt _) -> (TInt, [], [])
            | PValue (VBool _) -> (TBool, [], [])
            | PValue (VNil) -> let a = new_ty_var ()
                in (TList (TVar a), [], [])
            | PVar x -> let a = new_ty_var ()
                in (TVar a, [(x, ([],TVar a))], [])
            | PList (p1, p2) -> 
                let (t1, tenv1, c1) = gather_ty_constraints_pattern p1
                in let (t2, tenv2, c2) = gather_ty_constraints_pattern p2
                in begin
                match List.find_opt (fun (x1, _) -> (List.exists (fun (x2,_) -> x1 = x2) tenv2)) tenv1 with
                    None -> (TList t1, tenv1@tenv2, (TList t1, t2)::c1@c2) 
                    | Some (x,_) -> raise (Multi_time_Pattern_bind x)
                end
            | PTuple (p1, PValue VNil) -> 
                let (t1, tenv1, c1) = gather_ty_constraints_pattern p1
                in (TTuple (t1, TNil), tenv1, c1)
            | PTuple (p1, p2) -> 
                let (t1, tenv1, c1) = gather_ty_constraints_pattern p1
                in let (t2, tenv2, c2) = gather_ty_constraints_pattern p2
                in begin
                match List.find_opt (fun (x1, _) -> (List.exists (fun (x2,_) -> x1 = x2) tenv2)) tenv1 with
                    None -> (TTuple (t1, t2), tenv1@tenv2, c1@c2)
                    | Some (x,_) -> raise (Multi_time_Pattern_bind x)
                end
            | PWild -> let a = new_ty_var ()
                in (TVar a, [], [])
            | _-> raise Ty_error
        in let temp = List.map (fun (p, e) ->
            let (t', tenv', c') = gather_ty_constraints_pattern p
            in let (t, c) = gather_ty_constraints (tenv'@tenv) e
            in (t', t, c'@c)) l
        in let a = new_ty_var ()
        in let (t, c) = gather_ty_constraints tenv e
        in let pattern_type_constraints = List.map (fun (x,_,_) -> (x, t)) temp
        in let expr_type_constraints = List.map (fun (_,x,_) -> (TVar a, x)) temp
        in let extra_type_constraints = List.concat (List.map (fun (_,_,x) -> x) temp)
        in (TVar a, c@pattern_type_constraints@expr_type_constraints@extra_type_constraints)
    | EList (e1, e2) ->
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in (TList t1, (TList t1, t2)::c1@c2)
    | ETuple (e1, EValue (VNil)) ->
        let (t1, c1) = gather_ty_constraints tenv e1
        in (TTuple (t1,TNil), c1)
    | ETuple (e1, e2) ->
        let (t1, c1) = gather_ty_constraints tenv e1
        in let (t2, c2) = gather_ty_constraints tenv e2
        in (TTuple (t1,t2), c1@c2)
    | _ -> raise Ty_error