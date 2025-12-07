# COOL Compiler
<p align="center">
  <img src="assets/cool-compiler-banner.svg" alt="COOL Compiler" width="100%"/>
</p>

A compiler for the **COOL** (Classroom Object-Oriented Language) programming language, developed as part of the Stanford CS143 Compilers course.

There is still active development on this for educationa purposes. If you are interested in the original solutions for these assignments, please check the first commit of this project.

Note: This repository has been created to allow me to experiment with some stuff with compilers. As much as you might be tempted to copy this code for your own assignment, please refrain from doing so. There is a lot to learn from undertaking these projects yourself with minimal input from other sources.

## What is COOL?

COOL is a small object-oriented language designed for teaching compiler construction. It includes:

- **Classes and Inheritance** - Single inheritance with a root `Object` class
- **Static Typing** - Compile-time type checking with `SELF_TYPE` support
- **Automatic Memory Management** - Garbage collection
- **Type Matching** - `case` expressions for runtime type dispatch

## Project Structure

The compiler is built in five phases (programming assignments):

```
assignments/
├── PA2/          # Lexical Analysis (Flex)
│   └── cool.flex # Lexer specification
├── PA3/          # Parsing (Bison)
│   └── cool.y    # Parser grammar
├── PA4/          # Semantic Analysis
│   └── semant.cc # Type checking & scope analysis
└── PA5/          # Code Generation
    └── cgen.cc   # MIPS assembly generation
```

### Supporting Directories

- `bin/` - Prebuilt compiler tools (lexer, parser, semant, cgen, spim)
- `examples/` - Sample COOL programs
- `include/` - Header files for each phase
- `lib/` - Runtime support (`trap.handler` for SPIM)

## Compilation Pipeline

```
COOL Source (.cl)
       │
       ▼
   ┌────────┐
   │ Lexer  │  (PA2: cool.flex → tokens)
   └────────┘
       │
       ▼
   ┌────────┐
   │ Parser │  (PA3: cool.y → AST)
   └────────┘
       │
       ▼
   ┌────────┐
   │ Semant │  (PA4: type checking, scope)
   └────────┘
       │
       ▼
   ┌────────┐
   │ Cgen   │  (PA5: MIPS code generation)
   └────────┘
       │
       ▼
  MIPS Assembly (.s)
       │
       ▼
   ┌────────┐
   │ SPIM   │  (MIPS simulator)
   └────────┘
       │
       ▼
    Output
```

## Example COOL Program

```cool
class Main inherits IO {
    main() : Object {
        out_string("Hello, World!\n")
    };
};
```

## Built-in Classes

COOL provides these default classes:

| Class | Description & methods |
|-------|-------------|
| `Object` | Root of class hierarchy (`abort`, `type_name`, `copy`) |
| `IO` | Input/output (`out_string`, `out_int`, `in_string`, `in_int`) |
| `Int` | 32-bit integers |
| `String` | Strings (`length`, `concat`, `substr`) |
| `Bool` | Boolean values |

## Key Features Implemented

- [x] **Lexical Analysis** - Tokenization, string/comment handling, error recovery
- [x] **Parsing** - Full COOL grammar with operator precedence
- [x] **Semantic Analysis** - Type inference, inheritance checking, scope management
- [x] **Code Generation** - MIPS assembly with runtime support for:
  - Object layout and dispatch tables
  - Dynamic dispatch (virtual methods)
  - Static dispatch (`@Type.method()`)
  - Memory allocation and initialization
  - Case expressions (runtime type checking)
  - Some built-in method via trap.handler

## Runtime System

The generated code runs on SPIM with `trap.handler` providing:

- Memory management (heap allocation)
- Garbage collection support
- Built-in class implementations (String, IO, etc.)
- Runtime error handling (dispatch on void, case abort, etc.)

## License

**My code** (assignments in `assignments/PA*/`) is released under the [MIT License](LICENSE).

**Course materials** (skeleton code, headers, runtime) are Copyright © 1995-1996 The Regents of the University of California (see `include/*/copyright.h`).

**SPIM simulator** code in `lib/trap.handler` is Copyright © James R. Larus - personal/educational use only.

