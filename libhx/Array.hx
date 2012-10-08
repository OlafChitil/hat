module Array
(Var "!",Value {args = 1, fixity = L, priority = 9, letBound = True, traced = False})
(Var "//",Value {args = 2, fixity = L, priority = 9, letBound = True, traced = False})
(Var "accum",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "accumArray",Value {args = 3, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "array",Value {args = 2, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "assocs",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "bounds",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "elems",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "indices",Value {args = 0, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "ixmap",Value {args = 3, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "listArray",Value {args = 2, fixity = Def, priority = 9, letBound = True, traced = False})
(Method "Ix" "(7:8-7:9,Ix).inRange",Value {args = -1, fixity = Def, priority = 9, letBound = True, traced = False})
(Method "Ix" "(7:8-7:9,Ix).index",Value {args = -1, fixity = Def, priority = 9, letBound = True, traced = False})
(Method "Ix" "(7:8-7:9,Ix).range",Value {args = -1, fixity = Def, priority = 9, letBound = True, traced = False})
(Method "Ix" "(7:8-7:9,Ix).rangeSize",Value {args = -1, fixity = Def, priority = 9, letBound = True, traced = False})
(Method "Ix" "inRange",Value {args = -1, fixity = Def, priority = 9, letBound = True, traced = False})
(Method "Ix" "index",Value {args = -1, fixity = Def, priority = 9, letBound = True, traced = False})
(Method "Ix" "range",Value {args = -1, fixity = Def, priority = 9, letBound = True, traced = False})
(Method "Ix" "rangeSize",Value {args = -1, fixity = Def, priority = 9, letBound = True, traced = False})
(TypeClass "Array",TyCls (Ty [] []))
(TypeClass "Ix",TyCls (Cls ["range","index","inRange","rangeSize"]))