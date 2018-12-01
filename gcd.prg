[
  (Assign "a" (Const (I 98))),
  (Assign "b" (Const (I 56))),
  (While (Not (Eq (Var "a") (Var "b")))
         (If (Gt (Var "a") (Var "b"))
           (Assign "a" (Sub (Var "a") (Var "b")))
           (Assign "b" (Sub (Var "b") (Var "a")))
          )
  ),
  (Print (Var "a"))
]
