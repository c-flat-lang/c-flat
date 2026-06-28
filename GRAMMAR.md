## **Program Structure**

A program consists of multiple statements.

$$
\begin{align}
    [\text{Prog}] &\to [\text{Stmt}]^* \\
    [\text{Stmt}] &\to [\text{Use}] \mid [\text{FnDecl}] \mid [\text{VarDecl}] \mid [\text{StructDef}] \mid [\text{EnumDef}] \mid [\text{Expr}] ; \\
    [\text{Use}] &\to \text{use} \ [\text{Ident}] (\text{::} [\text{Ident}])^* ; \\
    [\text{FnDecl}] &\to \text{fn} \ [\text{Ident}] \ [\text{TypeParams}]? ( [\text{Params}]? ) \ [\text{Type}] \ \{ [\text{Stmt}]^* \} \\
    [\text{VarDecl}] &\to \text{const} \ [\text{Ident}] : [\text{Type}] = [\text{Expr}] \\
    [\text{StructDef}] &\to \text{type} \ [\text{Ident}] \ [\text{TypeParams}]? \ \text{struct} \ \{ [\text{StructBody}] \} \\
    [\text{TypeParams}] &\to \text{<(} \ [\text{Ident}] : [\text{Type}] (, [\text{Ident}] : [\text{Type}])^* \ \text{)>} \\
    [\text{TypeArgs}] &\to \text{<(} \ [\text{Type}] (, [\text{Type}])^* \ \text{)>} \\
    [\text{StructBody}] &\to ([\text{VarDecl}] \mid [\text{TypeDef}] \mid [\text{FnDecl}])^* \\
    [\text{EnumDef}] &\to \text{enum} \ [\text{Ident}] \ \{ [\text{EnumValues}] \} \\
    [\text{EnumValues}] &\to [\text{Ident}] (, [\text{Ident}])^* \\
    [\text{ForLoop}] &\to \text{for} \ ( [\text{Ident}] \ \text{in} \ [\text{Expr}] ) \ \{ [\text{Stmt}]^* \} \\
    [\text{MethodCall}] &\to [\text{Expr}] . [\text{Ident}] ( [\text{Args}]? ) \\
    [\text{Expr}] &\to [\text{Literal}] \mid [\text{Ident}] \mid [\text{FnCall}] \mid [\text{CondExpr}] \mid [\text{StructInit}] \\
    [\text{FnCall}] &\to [\text{Ident}] \ [\text{TypeArgs}]? ( [\text{Args}]? ) \\
    [\text{StructInit}] &\to [\text{Ident}] \ [\text{TypeArgs}]? \{ (. [\text{Ident}] = [\text{Expr}])^* \} \\
    [\text{Ident}] &\to [a-zA-Z_][a-zA-Z_0-9]*
\end{align}
$$

## **Generics**

Structs and functions may be parameterized over types. Type parameters are
written in a _cradle_ `<( ... )>` rather than plain angle brackets, which keeps
the grammar unambiguous with the `<` / `>` comparison operators (no Pratt parser
or lexer hack required). Each parameter is constrained with `: type`.

```
type Pair<(T: type)> struct {
    first: T,
    second: T,
}

fn add<(T: type)>(x: T, y: T) T {
    return x + y;
}
```

At use sites, concrete type arguments go in the same cradle:

```
let p: Pair<(s32)> = make_pair<(s32)>(10, 20);
let n = add<(s32)>(123, 321); // explicit type argument
let m = add(100, 44); // T inferred from the arguments
```

Generics are compiled by **monomorphization**: before type checking, every
concrete instantiation (`Pair<(s32)>`, `add<(s32)>`) is specialized into its own
name-mangled item (`Pair__s32`, `add__s32`) with the type parameters substituted,
and the generic templates are dropped. Every later stage only ever sees concrete
types, so a type parameter never reaches code generation.

**Limitations:** the `: type` slot is reserved for future bounds but only `type`
is accepted today; type-argument inference covers literal arguments — pass an
explicit `<( .. )>` otherwise; const/value generics, generic enums, and generic
methods are not implemented.

## **Modules (`use`)**

A `use` statement imports from another `.cb` file. Paths resolve relative to the
importing file's directory using _longest file prefix wins_: the longest leading
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
