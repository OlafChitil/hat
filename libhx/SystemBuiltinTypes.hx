module SystemBuiltinTypes
(Con Data "ExitCode" "ExitFailure",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = True})
(Con Data "ExitCode" "ExitSuccess",Value {args = 0, fixity = Def, priority = 9, letBound = True, traced = True})
(TypeClass "ExitCode",TyCls (Ty ["ExitSuccess","ExitFailure"] []))
