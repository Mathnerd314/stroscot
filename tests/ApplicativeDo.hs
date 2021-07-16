data Expr = Var | App Expr Expr | Lambda Pat Expr | Tuple [Expr] | Do Seq Expr

data Pattern = Var | Tuple [Pattern]

data Seq = [Stmt]

data Stmt = ParBlock [Seq] | Bind Pat | Expr | Let [(Pat,Expr)] Expr
