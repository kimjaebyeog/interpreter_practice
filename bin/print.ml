open Syntax
open Eval

let print_name = print_string 

let rec print_value = function 
    | VInt i -> print_int i 
    | VBool true -> print_string "true"
    | VBool false -> print_string " false"
    | VFun _ -> print_string "<fun>"
    | VCons (t1, t2) ->
        (* print_value v1; print_string "::"; print_value v2 *)
        let rec print_list_ele l = match l with 
            VCons (t1, t2) ->
                print_string";"; print_value (eval [] (EThunk t1)); print_list_ele (eval [] (EThunk t2))
            | VNil -> ()
            | _ -> () (* should not be able to reach here, todo : raise error*)
        in print_string "["; print_value (eval [] (EThunk t1)); 
            print_list_ele (eval [] (EThunk t2)); print_string "]"
    | VTuple (t1, t2) -> 
        let rec print_tuple_ele t = match t with 
            VTuple (t1, t2) ->
                print_string","; print_value (eval [] (EThunk t1)); print_tuple_ele (eval [] (EThunk t2))
            | VNil -> ()
            | _ -> () (* should not be able to reach here, todo : raise error*)
        in print_string "("; print_value (eval [] (EThunk t1)); 
            print_tuple_ele (eval [] (EThunk t2)); print_string ")"
    | VNil -> print_string "[]"

let rec print_type = function
    | TInt -> print_string "int"
    | TBool -> print_string "bool"
    | TFun (t1, t2) -> 
        (match t1 with
        TFun _ -> print_string "("; print_type t1; print_string")"
        | _ -> print_type t1; );
        print_string " -> "; 
        (match t2 with
        TList _ | TTuple _ -> print_string "("; print_type t2; print_string")"
        | _-> print_type t2)
    | TVar i -> print_string "'"; print_int i
    (* todo : arrange i *)
    | TList t -> 
        (match t with
        TFun _ | TTuple _ -> print_string "("; print_type t; print_string")"
        | _-> print_type t); print_string " list"
    | TTuple (t1, TNil) ->
        (match t1 with
        TFun _ -> print_string "("; print_type t1; print_string")"
        | _ -> print_type t1; )
    | TTuple (t1, t2) ->
        (match t1 with
        TFun _ | TTuple _ -> print_string "("; print_type t1; print_string")"
        | _ -> print_type t1; );
        print_string " * "; 
        (match t2 with
        TFun _ -> print_string "("; print_type t2; print_string")"
        | _ -> print_type t2; );
    | TNil -> () 
    
let rec print_clet_result : ((name option * ty_scheme) list) -> unit
    = fun r -> match r with
   (None, t)::rest -> 
        print_string "- : " ;
        print_type (snd t);
        print_string " = " ;
        print_string "<thunk>";
        print_newline ();
        print_clet_result (rest)
    | (Some x, t)::rest ->
        print_string "val ";
        print_name x;
        print_string " : " ;
        print_type (snd t);
        print_string " = ";
        print_string "<thunk>";
        print_newline ();
        print_clet_result (rest)
    | []-> ()

let print_cexp_result : ty_scheme * value -> unit
    = fun (t,v) ->
        print_string "- : " ;
        print_type (snd t);
        print_string " = " ;
        print_value v;
        print_newline ();