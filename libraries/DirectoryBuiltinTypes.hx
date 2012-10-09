module DirectoryBuiltinTypes
(Con Data "Permissions" "Permissions",Value {args = 4, fixity = Def, priority = 9, letBound = True, traced = False})
(Field "Permissions" "executable",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(Field "Permissions" "readable",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(Field "Permissions" "searchable",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(Field "Permissions" "writable",Value {args = 1, fixity = Def, priority = 9, letBound = True, traced = False})
(TypeClass "Permissions",TyCls (Ty ["Permissions"] ["readable","writable","executable","searchable"]))
