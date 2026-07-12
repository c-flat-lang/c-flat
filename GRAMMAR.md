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
    [\text{EnumDef}] &\to \text{type} \ [\text{Ident}] \ \text{enum} \ \{ [\text{EnumValues}] \} \\
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

## **Pointers**

A pointer type is written `*T` (a pointer to a value of type `T`), following the
Zig/C convention. Pointers nest (`**T`) and compose with other type forms
(`*[u8]`, `*List`). Like Zig, pointers are **non-null by default** — there is no
null pointer today; a typed null/optional pointer (`?*T`) is planned for later.

```
fn set_list(list: *List, index: usize, value: s32) void { ... }

pub fn println(string: *[u8]) void { ... }
```

Two operators work with pointers:

- **Address-of** `&expr` — takes the address of `expr`, producing a `*T`.
- **Dereference** `expr.*` — reads through a pointer, producing the pointee `T`.
  It is a postfix operator, so it chains: `p.*`, `p.*.field`, `p.*[i]`.

```
let x: s32 = 10;
let p: *s32 = &x;   // address-of: *s32
let y: s32 = p.*;   // dereference: s32
```

Grammar additions:

$$
\begin{align}
    [\text{Type}] &\to \text{*} \ [\text{Type}] \\
    [\text{Expr}] &\to \text{\&} \ [\text{Expr}] \mid [\text{Expr}] . \text{*}
\end{align}
$$

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
