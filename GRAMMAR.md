## **Program Structure**
A program consists of multiple statements.

$$
\begin{align}
    [\text{Prog}] &\to [\text{Stmt}]^* \\
    [\text{Stmt}] &\to [\text{FnDecl}] \mid [\text{VarDecl}] \mid [\text{StructDef}] \mid [\text{EnumDef}] \mid [\text{Expr}] ; \\
    [\text{FnDecl}] &\to \text{fn} \ [\text{Ident}] ( [\text{Params}]? ) \ [\text{Type}] \ \{ [\text{Stmt}]^* \} \\
    [\text{VarDecl}] &\to \text{const} \ [\text{Ident}] : [\text{Type}] = [\text{Expr}] \\
    [\text{StructDef}] &\to \text{type} \ [\text{Ident}] \ \text{struct} \ \{ [\text{StructBody}] \} \\
    [\text{StructBody}] &\to ([\text{VarDecl}] \mid [\text{TypeDef}] \mid [\text{FnDecl}])^* \\
    [\text{EnumDef}] &\to \text{enum} \ [\text{Ident}] \ \{ [\text{EnumValues}] \} \\
    [\text{EnumValues}] &\to [\text{Ident}] (, [\text{Ident}])^* \\
    [\text{ForLoop}] &\to \text{for} \ ( [\text{Ident}] \ \text{in} \ [\text{Expr}] ) \ \{ [\text{Stmt}]^* \} \\
    [\text{MethodCall}] &\to [\text{Expr}] . [\text{Ident}] ( [\text{Args}]? ) \\
    [\text{Expr}] &\to [\text{Literal}] \mid [\text{Ident}] \mid [\text{FnCall}] \mid [\text{CondExpr}] \mid [\text{StructInit}] \\
    [\text{StructInit}] &\to [\text{Ident}] \{ ([\text{Ident}] : [\text{Expr}])^* \} \\
    [\text{Ident}] &\to [a-zA-Z_][a-zA-Z_0-9]*
\end{align}
$$
