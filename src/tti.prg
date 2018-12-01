 [ (Assign "a" (Const (I 98))),
  (Assign "b" (Const (I 56))),
  (Try
    (Seq
      (Print (Var "a"))
      (Seq (Print (Var "b"))
           (Print (Var "x")) ))
    (Print (Var "b"))
  ),
  (While (Not (Eq (Var "a") (Var "b")))
         (If (Gt (Var "a") (Var "b"))
           (Assign "a" (Sub (Var "a") (Var "b")))
           (Seq
             (Assign "b" (Sub (Var "b") (Var "a")))
             (Assign "b" (Var "a"))
           )
         )
  ),
  (Print (Var "a"))]
