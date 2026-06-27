## **Program Structure**
A program consists of multiple statements.

$$
\begin{align}
    [\text{Prog}] &\to [\text{Stmt}]^* \\
    [\text{Stmt}] &\to [\text{Use}] \mid [\text{FnDecl}] \mid [\text{VarDecl}] \mid [\text{StructDef}] \mid [\text{EnumDef}] \mid [\text{Expr}] ; \\
    [\text{Use}] &\to \text{use} \ [\text{Ident}] (\text{::} [\text{Ident}])^* ; \\
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

## **Modules (`use`)**

A `use` statement imports from another `.cb` file. Paths resolve relative to the
importing file's directory using *longest file prefix wins*: the longest leading
run of `::` segments that names a real `.cb` file is the module file; a trailing
segment selects an item inside it.

- `use math::add;` — item import. Brings the **public** `add` into scope; call it
  as `add(...)`. Resolves to `math.cb`, item `add`.
- `use math;` — whole-module import. Access items through the namespace:
  `math::add(...)`. Resolves to `math.cb`.

Only `pub` items may be imported or accessed across modules. Import cycles are
rejected.

**v1 limitations:** the program namespace is flat, so top-level names must be
unique across all modules (no name mangling yet). Brace-group imports
(`use a::{x, y}`), in-file nested namespaces (paths with more than one trailing
selector), and `std`/`core` are not implemented.
