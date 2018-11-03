use "ast.sml";

datatype (''var, 'vals) env =
  EmptyEnv
  | aug of (''var, 'vals) env * (''var, 'vals) binding
and (''var, 'vals) binding = <- of ''var * 'vals

datatype ''var vals = 
  Const of int
  | Closure of ''var * ''var ast

fun env |- p = case p of
  SConst k => Const k
  | Var x => (case env of
    EmptyEnv => raise UnboundVariable
    | env' aug (x' <- v) =>
      if x = x' then v
      else env' |- p)
  | m :+ n => (case (env |- m, env |- n) of
    (Const k, Const k') => Const (k + k')
    | _ => raise Parsing)
  | Let (x, m, n) => env aug (x <- (env |- m)) |- n
  | param ~> body => Closure (param, body)
  | Apply (f, arg) => (case (env |- f, env |- arg) of
    (Closure (x, m'), v) => env aug (x <- v) |- m'
    | _ => raise Parsing)
