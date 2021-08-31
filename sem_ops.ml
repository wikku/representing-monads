open Common

type value =
  | Int of int
  | Fun of (value -> value)

let ($) = function
  | Int _ -> failwith "type error: ($)"
  | Fun(f) -> f

let get_int = function
  | Int i -> i
  | Fun _ -> failwith "type error: get_int"

let value_to_string = function
  | Int i -> string_of_int i
  | Fun _ -> "<fun>"


let id = Fun(fun x -> x)
let cps_id = Fun(fun x -> Fun(fun g -> g $ x))

let succ = Fun(fun x -> Fun(fun k -> k $ Int(get_int x + 1)))

let int_op op =
  Fun(fun x -> Fun(fun k  -> k  $
  Fun(fun y -> Fun(fun k' -> k' $
  Int(op (get_int x) (get_int y))))))


let (let$) v k = v $ Fun k

let (>>=) m f = Fun(fun c -> m $ Fun(fun a -> f $ a $ c))
(*let ( let* ) m f = Fun(fun c -> m $ Fun(fun a -> f a $ c))*)

(*let (and$) v u = *)

let pure x = fun k -> k $ x

let rec sem expr env = Fun(match expr with
  | Lit(i) -> pure (Int i)
  | Var(x) -> pure (get x env)
  | Lam(x, e) -> pure (Fun(fun v -> sem e (extend x v env)))
  | App(e1, e2) -> fun k ->
    let$ f = sem e1 env in
    let$ a = sem e2 env in
    f $ a $ k
  | Callcc(e) -> fun k ->
    let$ f = sem e env in
    (let$ f = f in Fun(fun _k' -> k $ f)) $ k
  | Shift(e) -> fun k ->
    let$ f = sem e env in
    (let$ v = f in
     Fun(fun k' ->
     k $ v >>= k'
     (*
     let* r = k $ v in
     k' $ r
     *)
     (*
     Fun(fun g'' -> let$ w = k $ v
     in k' $ w $ g'')
     *)
    ))
    $ cps_id
  | Reset(e) -> fun k ->
    sem e env $ cps_id >>= k
    (*
    let* r = sem e env $ cps_id
    in k $ r
    *)
    (*
    Fun(fun g ->
    let$ r = sem e env $ cps_id
    in k $ r $ g)
    *)
  )

let init_env = empty
  |> extend "succ" succ
  |> extend "zero" (Int 0)
  |> extend "+" (int_op ( + ))
  |> extend "*" (int_op ( * ))

let run expr = sem expr init_env $ cps_id $ id
