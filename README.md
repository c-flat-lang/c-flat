# C-flat

C-flat is a strongly typed programming language inspired by modern languages like Rust, Zig, Go and many other languages. This is a passion project that I’m developing for fun, without any rush, and with an open mind about its evolution. None of the syntax is set in stone—it could change at any moment as I explore new ideas and refine the language.

## Why C-flat?

The name *C-flat* comes from music, where C♭ is a note one step lower than C. Similarly, this language takes inspiration from C but aims to provide more modern features and flexibility, particularly in memory management.

One of the core ideas behind C-flat is giving developers full control over memory allocation, much like Zig allows for custom allocators. While this isn't implemented yet, it’s a major goal for the future—after all, everything starts as an idea!

## Features (Work in Progress)
- **Strong static typing** to catch errors early.
- **Modern syntax** inspired by Rust, Zig, and Go.
- **Custom allocators** (planned) to provide fine-grained memory control.
- **Structs and Enums** to support complex data types.
- **For-loops with iterators**, making iteration expressive and powerful.

## Example Code
Here's a small taste of C-flat’s syntax:

```cflat
// Define a struct
type Point struct {
    x: s32,
    y: s32,
}

// Define an enum
enum Color {
    Red,
    Green,
    Blue,
}

// Nested types within a struct
type ArrayList struct {
    type Iterator struct {
        current: s32,
        end: s32,
    }

    fn iterator(&self) Iterator {
        Iterator {
            current: 0,
            end: self.len(),
        }
    }
}

// Entry point
fn main() void {
    const a: ArrayList = ArrayList{};
    const b: ArrayList.Iterator = a.iterator();
    for (i in b) {
        println(i);
    }
}
```

## What’s Next?
C-flat is still in its extreme early stages, and I'm figuring things out as I go. Right now, I’m experimenting with syntax, semantics, and figuring out how best to structure the language. If you're interested, feel free to follow the project!

This is a project driven by curiosity and passion. No deadlines, no expectations—just the joy of building something new. 🚀

