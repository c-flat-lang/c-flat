# C-flat

<details>
<summary><a href="./bitbox/README.md">bitbox</a></summary>
C-flat language backend, targeting wasm32, x86_64-linux

<details>
<summary><a href="./bitbox/src/backend/wasm32/README.md">wasm32 backend</a></summary>
helpful links
</details>

<details>
<summary><a href="./bitbox/src/backend/x86_64/README.md">x86_64 backend</a></summary>
helpful links
</details>

</details>

C-flat is a strongly typed systems programming language inspired by Rust, Zig, Go, and other modern languages. It's a passion project built for fun, without deadlines, and with an open mind about where it goes. None of the syntax is set in stone — it can and does change as the language gets used to build real things.

> ⚠️ **Work in progress.**
> The type system especially is still being actively designed and hardened.
> Expect rough edges, missing features, and the occasional compiler bug (we find plenty of them by actually building things in the language).
> Syntax shown below reflects the language _today_ and may shift.

## Why C-flat?

The name comes from music, where C♭ is a note one step lower than C.
Similarly, this language takes inspiration from C but aims for more modern ergonomics and control, especially around memory.
Also C flat sounds like B 🤣 and the file extention is `.cb` which just so happens to fit my online name cowboy 🤣.

C-flat compiles to both **native x86-64 (Linux)** and **WebAssembly**, so the same source can run as a native binary or in the browser (with a Raylib binding for graphics/audio in the wasm build).

## Features

- **Strong static typing**, with the type system still very much under construction.
- **Generics with monomorphization**, using a "type cradle" syntax: `<( )>` — see below.
- **Manual memory management** via raw syscalls (`mmap`/`munmap` today; pluggable custom allocators are a future goal, similar to Zig).
- **Structs and enums**, including default field values for structs.
- **Pointers** `*T`, plus fat pointer slices (`*[T]`) that carry a length. Deref Pointer by `T.*`
- **Fixed-size arrays** (`[N; T]`).
- **`extern C` declarations** for calling into host/runtime functions (syscalls, or a host bridge like Raylib in the wasm build).
- No methods yet — functions are free functions, conventionally namespaced by a `Type_verb` naming convention (e.g. `Board_move`, `ArrayList_push`). Methods (`self`-taking functions with dot-call syntax) are planned but not implemented.
- No Unions yet - We plan on adding unions at some point, syntax being similar to structs and enums.

## Example Code

```cflat
// Struct definition, with a default field value
type Point struct {
    x: s32,
    y: s32,
}

type HexTile struct {
    // Default values
    value: u32 = 0,
    exists: bool = false,
}

// Enum definition
type Color enum {
    red,
    green,
    blue,
}

// Struct with a generic type parameter <(cradle syntax)>
type ArrayList<(T: type)> struct {
    items: *[T],
    len: usize = 0,
}

// Generic free function, monomorphized per T
fn ArrayList_new<(T: type)>() ArrayList<(T)> {
    // ...
}

fn ArrayList_push<(T: type)>(self: *ArrayList<(T)>, item: T) void {
    // ...
}

// extern declarations bring in host/runtime functions
// see [C helper functions](./runtime.c) for all helper definitions.
extern C fn write_s32(s32) void;


// Currently working on a std lib as we develop the language.
// At some point we will be moving raylib out of std to a
// vendor import or better yet have a way to use 3rd party libs.
use std::raylib;
use std::math;

fn tile_color(value: s32) Color {
    if value == 2 {
        return Color::red;
    } else if value == 4 {
        return Color::green;
    } else {
        return Color::blue;
    }
}

// Entry point
pub fn main() void {
    let mut list = ArrayList_new<(s32)>();
    ArrayList_push<(s32)>(&list, 42);

    let mut i: usize = 0;
    while i < list.len {
        write_s32(list.items[i]);
        i = i + 1;
    }
}
```

### A few syntax notes

- `let` for bindings, `let mut` for mutable ones, no `const` keyword currently.
- `fn Name(...) ReturnType { ... }`; `void` for no return value.
- Generic parameters use `<( )>` at both the definition site (`fn foo<(T: type)>(...)`) and call site (`ArrayList_new<(s32)>()`), rather than Rust/Zig-style angle brackets alone.
- `if`/`else if`/`else` — no `match`/`switch` yet.
- `while` loops only — no `for`-in loops yet (planned).
- `and`/`or` for boolean logic rather than `&&`/`||`.
- Struct field access and struct literals use `.field` (e.g. `Point { .x = 0, .y = 0 }`).
- Enum variants are accessed with `::` (e.g. `Color::red`).

## What's Next?

C-flat is being developed alongside real programs, currently a small Raylib based [game](https://github.com/cowboy8625/raylib-6-game-jam),
which surfaces genuine language and compiler bugs faster than writing the compiler in a vacuum ever could.
Big open areas: a more solid type system, methods/`self`-calls, `for`-in iteration, and a real allocator story.

This is a project driven by curiosity, one bug and one feature at a time.
No deadlines, no expectations, just the joy of building something new. 🚀
