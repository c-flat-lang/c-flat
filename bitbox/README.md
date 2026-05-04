# Bitbox

## Overview

Bitbox is the backend for the C-flat language, serving a role similar to LLVM.
However, it is **not derived from or based on LLVM’s design**.
Instead, Bitbox independently implements the kind of functionality a modern compiler backend requires.

LLVM is often used as a backend for compilers,
but it is not the only approach—projects like GCC and Go use their own compiler infrastructures without relying on LLVM.
Bitbox follows a similar philosophy:
building a backend tailored specifically for C-flat.

## Design Direction

The long-term goal is to decouple Bitbox from C-flat so it can eventually function as a standalone backend,
usable independently of the language itself.

To support this, a textual representation of the intermediate representation (IR) instructions has been introduced.
This provides a bridge in cases where direct cross-language interoperability is not yet available,
allowing other tools or languages to interface with Bitbox through a stable, language-agnostic format.

## Notes

Bitbox can be thought of as a conceptual specification of what a system like LLVM provides, without reusing its architecture or code.
It aims to deliver comparable capabilities through an original design.

Like any ambitious systems project, Bitbox is evolving over time.
Large, complex systems aren’t built all at once or by a single contributor—they grow incrementally through iteration and development.

## Targets

- WASM32
- X86_64-Linux
