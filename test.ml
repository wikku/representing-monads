open Common

module Run(S : Sem) =
struct
  let progs = [
    Var("zero"); (* 0 *)
    Lit(0); (* 0 *)
    App(Lam("x", Var("x")), Lit(1)); (* 1 *)
    App(Lam("x", Var("zero")), Lam("x", Var("x"))); (* 0 *)
    App(Var("succ"), Lit(2)); (* 3 *)
    App(Var("succ"),App(Var("succ"), Lit(3))); (* 5 *)
    App(App(Var("+"), Lit(100)), Lit(6));  (* 106 *)
    App(App(Var("+"), Lit(7)), App(App(Var("*"),Lit(20)),Lit(5))); (* 107 *)
    App(App(Var("+"), Lit(8)), Reset(App(App(Var("*"),Lit(20)),Lit(5)))); (* 108 *)
    App(App(Var("+"), Lit(1)),
        Reset(App(App(Var("*"), Lit(2)),
                  Shift(Lam("k", App(Var("k"), App(Var("k"), Lit(10)))))))); (* 41 *)
    App(App(Var "+", Lit 3),
        Callcc(Lam("k", App(App(Var "+", Lit 6),
                            App(Var "k", Lit 1))))); (* 4 *)
    App(Reset(App(Var "+", Lit 3)),
        Reset(Callcc(Lam("k", App(App(Var "+", Lit 6),
                                  App(Var "k", Lit 1)))))); (* 4 *)
  ]
  let _ = List.map (fun p -> S.run p |> S.value_to_string |> print_endline) progs
end

module R = Run(Sem)
module R = Run(Sem_ops)
module R = Run(Sem_fo)
