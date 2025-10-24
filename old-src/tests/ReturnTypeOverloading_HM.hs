{-# LANGUAGE  LambdaCase #-}
type Lam = Value -> Value

data Value
   = Int Integer
   | Bool Bool
   | Fn Type Type Lam
   | Undecided Type (Type -> Value)

data Type
  = TyInt
  | TyBool
  | TyFn Type Type
  | TVar (IORef TVar)

data TVar = Bound Type
        | Unbound TVar_ID Classes

type TVar_ID = Int
data Classes = Numeric | Nothing

combine_classes :: Classes -> Classes -> Classes
combine_classes Numeric Numeric = Numeric
combine_classes x Nothing = x
combine_classes Nothing x = x

data Expr
  = Value Value
  | App Expr Expr
  | UFn Lam

find_type :: Value -> Type
find_type (Int _) = TyInt
find_type (Bool _) = TyBool
find_type (Fn a b _) = TyFn a b
find_type (Undecided t _) = t

force_type t (Undecided _ tf) = tf t
force_type t v = assert (find_type v == t) v

eval (Value v) = return v
eval (App op e) = do
  f <- eval op
  x <- eval e
  t' <- newvar_t
  unify (find_type f) (Fn (find_type x) t')
  return $ Undecided t' (\t ->
    case force_type (Fn (find_type x) t) f of
        Fn _ _ f_un -> f_un x )
eval (UFn lam) = do
  t <- newvar_t
  t' <- newvar_t
  return $ Fn t t' lam

eval_strict (Undecided ty f) = do
  ty' <- resolve ty
  f ty
eval_strict x = x


resolve (TVar tv) = do
  t <- readIORef tv
  case t of
    Unbound tid l cls ->
      case cls of
        Numeric -> do
          unify tv Int
          return TyInt
        Nothing -> error "Ambiguous type" tv
    Bound t -> resolve t
resolve (TyFn a b) = TyFn <$> resolve a <*> resolve b
resolve r = return r

current_typevar = ref 0
newvar = atomicModifyIORef current_typevar (\n -> (n+1, n))
newvar_t = do
  vid <- newvar
  r <- newIORef (Unbound vid)
  return $ TVar r


-- Can a monomorphic TVar(a) be found inside this type?
occurs a_id a_level (TVar r) =  do
  readIORef r >>= \case
    Bound t -> occurs a_id a_level t
    Unbound b_id b_level -> do
      let min_level = min a_level b_level
      writeIORef r (Unbound (b_id, min_level))
      return $ a_id == b_id
occurs a_id a_level (Fn b c) = liftA2 (||) (occurs a_id a_level b) (occurs a_id a_level c)

unify : Type -> Type -> IO ()
unify va@(TVar ra) vb@(TVar rb) = do
  a <- readIORef ra
  b <- readIORef rb
  case (a,b) of
    (Bound a', _) -> unify a' vb
    (_, Bound b') -> unify va b'

    | (TVar({ contents = Unbound(a_id, a_level) } as a), b) ->
        (* create binding for boundTy that is currently empty *)
        if t1 = t2 then () else (* a = a, but dont create a recursive binding to itself *)
        if occurs a_id a_level b then raise TypeError else
        a := Bound b

    | (a, TVar({ contents = Unbound(b_id, b_level)} as b)) ->
        (* create binding for boundTy that is currently empty *)
        if t1 = t2 then () else
        if occurs b_id b_level a then raise TypeError else
        b := Bound a

    | (Fn(a, b), Fn(c, d)) ->
        unify a c;
        unify b d

    | (a, b) -> raise TypeError


(* Find all typevars and wrap the type in a PolyType *)
(* e.g.  generalize (a -> b -> b) = forall a b. a -> b -> b  *)
let generalize (t: typ) : polytype =
    (* collect all the monomorphic typevars *)
    let rec find_all_tvs = function
        | TUnit -> []
        | TVar({ contents = Bound t }) -> find_all_tvs t
        | TVar({ contents = Unbound (n, level)}) ->
            if level > !current_level then [n]
            else []
        | Fn(a, b) -> find_all_tvs a @ find_all_tvs b

    in find_all_tvs t
    |> List.sort_uniq compare
    |> fun typevars -> PolyType(typevars, t)


(* For the Abs/Lambda rule, parameter types need to be stored in *)
(* our polytype map, though parameters shouldn't be generalized  *)
(* since their types shouldn't change (be instantiated) within the function. *)
(* This helper function performs the conversion while making that explicit. *)
let dont_generalize (t: typ) : polytype =
    PolyType([], t)


(* The main entry point to type inference *)
(* All branches (except for the trivial Unit) of the first match in this function
   are translated directly from the rules for algorithm J, given in comments *)
(* infer : polytype SMap.t -> Expr -> Type *)
let rec infer env : expr -> typ = function
    | Unit -> TUnit

    (* Var
     *   x : s ∊ env
     *   t = inst s
     *   -----------
     *   infer env x = t
     *)
    | Identifier x ->
        let s = SMap.find x env in
        let t = inst s in
        t

    (* App
     *   infer env f = t0
     *   infer env x = t1
     *   t' = newvar ()
     *   unify t0 (t1 -> t')
     *   ---------------
     *   infer env (f x) = t'
     *)
    | FnCall(f, x) ->
        let t0 = infer env f in
        let t1 = infer env x in
        let t' = newvar_t () in
        unify t0 (Fn(t1, t'));
        t'

    (* Abs
     *   t = newvar ()
     *   infer (SMap.add x t env) e = t'
     *   -------------
     *   infer env (fun x -> e) = t -> t'
     *)
    | Lambda(x, e) ->
        let t = newvar_t () in
        (* t must be a polytype to go in our map, so make an empty forall *)
        let env' = SMap.add x (dont_generalize t) env in
        let t' = infer env' e in
        Fn(t, t')

    (* Let
     *   infer env e0 = t
     *   infer (SMap.add x (generalize t) env) e1 = t'
     *   -----------------
     *   infer env (let x = e0 in e1) = t'
     *
     * enter/exit_level optimizations are from
     * http://okmij.org/ftp/ML/generalization.html
     * In this implementation, they're required so we
     * don't generalize types that escape into the environment.
     *)
    | Let(x, e0, e1) ->
        enter_level ();
        let t = infer env e0 in
        exit_level ();
        let t' = infer (SMap.add x (generalize t) env) e1 in
        t'
