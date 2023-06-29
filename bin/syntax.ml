type name = string
type binOp = OpAdd | OpSub | OpMul | OpDiv | OpEq | OpLt
type value = VInt of int | VBool of bool 
  | VFun of name * expr * env
  | VCons of thunk * thunk
  | VTuple of thunk * thunk
  | VNil
  (* | VThunk of thunk 
    (* need to add force eval *) *)
and thunk = 
  Thunk of expr * env
  | RThunk of name * expr * env
  | MRThunk of int * ((name * expr) list) * env
and env = (name * thunk) list
and expr = 
  EValue of value
  | EVar of name
  | EBin of binOp * expr * expr
  | EIf of expr * expr * expr
  | ELet of name * expr * expr
  | EFun of name * expr
  | EApp of expr * expr  
  | ERLet of name * expr * expr
  | EMRLet of ((name * expr) list) * expr
  | EMatch of expr * ((pattern * expr) list)
  | EList of expr * expr
  | ETuple of expr * expr
  | EThunk of thunk
and pattern = PValue of value | PVar of name
  | PList of pattern * pattern
  | PTuple of pattern * pattern
  | PWild
type ty_var = int
type ty =
  | TInt
  | TBool
  | TFun of ty * ty
  | TVar of ty_var
  | TList of ty
  | TTuple of ty * ty 
  | TNil
type ty_scheme = (ty_var list) * ty
type ty_subst = (ty_var * ty) list
type ty_constraints = (ty * ty) list
type ty_env = (name * ty_scheme) list
type command = 
  CExp of expr
  | CLet of name * expr
  | CRLet of name * expr
  | CMRLet of (name * expr) list
type command_result = Command_result of env * ty_env
    
exception Eval_error
exception Unbound_value of name
exception Ty_error
exception Multi_time_Pattern_bind of name