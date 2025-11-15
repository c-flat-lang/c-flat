# Commit Naming Convention for c-flat

## Types Prefixes

| Type         | Meaning                                  | Example                                             |
| ------------ | ---------------------------------------- | --------------------------------------------------- |
| **feat**     | New feature / capability                 | `feat(codegen): emit wasm if/else blocks`           |
| **fix**      | Bug fix or correctness change            | `fix(parser): reject unclosed block`                |
| **refactor** | Change internals without behavior change | `refactor(ir): merge BasicBlock and BlockNode`      |
| **perf**     | Performance improvement                  | `perf(optimizer): avoid redundant dominator checks` |
| **test**     | Add or modify tests                      | `test(parser): cover nested function parsing`       |
| **docs**     | Documentation updates                    | `docs(readme): describe lowering phase`             |
| **chore**    | Maintenance, build, CI                   | `chore(ci): add wasm32 target to test matrix`       |
| **style**    | Formatting / naming cleanup              | `style(ir): rename IfElse_ → IfElse`                |

## Scopes

- Scope names match your crate/module names (e.g., ir, parser, codegen).
  | Scope | Description |
  | ------------- | --------------------------------------- |
  | **lexer** | Tokenization logic |
  | **parser** | Grammar / AST building |
  | **ir** | Intermediate representation |
  | **ssa** | SSA construction or destruction |
  | **typecheck** | Type inference / checking |
  | **optimizer** | Constant folding, DCE, etc. |
  | **codegen** | WebAssembly / machine code emission |
  | **emitter** | Textual or binary encoding output |
  | **runtime** | Any built-in runtime or support library |
  | **cli** | Frontend command-line tool |
  | **tests** | Integration and regression tests |
  | **error** | Error handling |
  | **docs** | Docs, specs, design notes |

## Breaking Changes

`feat(ir)!: change representation of IfElse_`
