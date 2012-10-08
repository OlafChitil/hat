module Ratio
(Var "%",Value {args = 2, fixity = L, priority = 7, letBound = True, traced = False})
(Var "approxRational",Value {args = 2, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "denominator",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "numerator",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(TypeClass "Ratio",TyCls (Ty [":%"] []))
(TypeClass "Rational",TyCls (Syn 0 THelper))
