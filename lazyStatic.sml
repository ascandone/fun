use "ast.sml";

datatype 'var env =
  EmptyEnv
  | aug of 'var env * 'var binding
and 'var binding = <- of 'var * 'var ast

datatype 'var vals = 
  Const of int
  | Closure of 'var * 'var ast * 'var env
  
infix 5 ++
fun env ++ EmptyEnv = env
  | env ++ (env' aug b) = env ++ env' aug b

fun env |- p = case p of
  SConst k => Const k
  | Var x => (case env of
    EmptyEnv => raise UnboundVariable
    | env' aug (x' <- p') =>
      if x = x' then env |- p'
      else env' |- p)
  | m :+ n => (case (env |- m, env |- n) of
    (Const k, Const k') => Const (k + k')
    | _ => raise Parsing)
  | Let (x, m, n) => (env aug (x <- m)) |- n
  | param ~> body => Closure (param, body, env)
  | Apply (f, arg) => (case env |- f of
    Closure (param, body, env') => (env ++ env' aug (param <- arg)) |- body
    | _ => raise Parsing)
