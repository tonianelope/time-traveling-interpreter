[ (Assign "a" (Const (I 98))),
  (Assign "b" (Const (I 56))),
  (Print (Var "a")),
  (Print (Var "b")),
  (While (Not (Eq (Var "a") (Var "b")))
         (If (Gt (Var "a") (Var "b"))
           (Assign "a" (Sub (Var "a") (Var "b")))
           (Seq
             (Assign "b" (Sub (Var "b") (Const (I 10))))
             (Assign "b" (Var "b")))
         )
  ),
   (Try
    (Seq
      (Print (Var "a"))
      (Seq (Print (Var "b"))
           (Print (Var "x")) ))
    (Print (Var "b"))
  )]
