module System.Exit
(Var "exitFailure",Value {args = 0, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "exitSuccess",Value {args = 0, fixity = Def, priority = 9, letBound = True, traced = False})
(Var "exitWith",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(Con Data "ExitCode" "ExitFailure",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(Con Data "ExitCode" "ExitSuccess",Value {args = 0, fixity = Def, priority = 9, letBound = True, traced = False})
(TypeClass "ExitCode",TyCls (Ty ["ExitSuccess","ExitFailure"] []))
