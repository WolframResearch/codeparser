
Paclet[
    Name -> "AST",
    Version -> "999", (* use very high version number for development; actual version number is supplied during build process *)
    WolframVersion -> "11.0+",
    Extensions -> {
    		{"Kernel", Context -> "AST`"},
    		{"Documentation", Language -> "English"},
			{"Resource",
				SystemID->"Linux-x86-64",
				Resources -> {{"wl-ast", "ASTResources/Linux-x86-64/wl-ast"}}},
			{"Resource",
				SystemID->"MacOSX-x86-64",
				Resources -> {{"wl-ast", "ASTResources/MacOSX-x86-64/wl-ast"}}},
			{"Resource",
				SystemID->"Windows",
				Resources -> {{"wl-ast", "ASTResources/Windows/wl-ast.exe"}}},
			{"Resource",
				SystemID->"Windows-x86-64",
				Resources -> {{"wl-ast", "ASTResources/Windows-x86-64/wl-ast.exe"}}}
    }
]
