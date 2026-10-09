# Stone

Stone is a small imperative language with built-in concurrency, written for the
Programming Paradigms final project at the University of Twente. The compiler is
written in Haskell. It parses Stone source code, type-checks and elaborates it,
and generates code for [Sprockell](https://github.com/bobismijnnaam/sprockell),
a simple multi-core processor simulator.

Authors: Daniel Chitoraga, Stef Waalders

## Language overview

```
// Two threads update shared accounts under a lock
shared int john = 1000
shared int alex = 1000
shared int barrier
shared lock l

fork {
  int transaction = 500
  lock(l)
  john = john - transaction
  alex = alex + transaction
  barrier = barrier + 1
  unlock(l)
}
fork {
  int transaction = -200
  lock(l)
  john = john - transaction
  alex = alex + transaction
  barrier = barrier + 1
  unlock(l)
}

while (not(barrier == 2)) {}
print(john)
print(alex)
```

### Types and declarations

| Type   | Example              | Notes                                        |
|--------|----------------------|----------------------------------------------|
| `int`  | `int x = 5`          | Defaults to `0` when not initialised         |
| `bool` | `bool b = true`      | Stored as `1`/`0`; `print` outputs the integer |
| `lock` | `shared lock l`      | Must be declared `shared`                    |

Variables are local by default. Prefix a declaration with `shared` to put it in
shared memory, where every thread can see it.

### Statements

| Statement      | Syntax                               |
|----------------|--------------------------------------|
| Declaration    | `[shared] <type> name [= expr]`      |
| Assignment     | `name = expr`                        |
| If / if-else   | `if (cond) { ... } [else { ... }]`   |
| While loop     | `while (cond) { ... }`               |
| Print          | `print(expr)`                        |
| Spawn a thread | `fork { ... }`                       |
| Acquire lock   | `lock(l)`                            |
| Release lock   | `unlock(l)`                          |

Statements are separated by whitespace or newlines. There are no semicolons.
Line comments start with `//`.

### Expressions

These operators are listed from lowest to highest precedence:

| Operators        | Kind        |
|------------------|-------------|
| `or`             | logical     |
| `and`            | logical     |
| `==` `<` `<=`    | comparison  |
| `+` `-`          | arithmetic  |
| `*`              | arithmetic  |
| `not`            | unary       |

Parentheses group expressions as usual. Literals are integers, `true` and `false`.

### Scoping and concurrency rules

The elaborator enforces these rules at compile time:

- Inner blocks may shadow outer variables. Each variable is renamed to a unique
  name during elaboration.
- A variable must be declared before it is used, and it is only visible inside
  the block where it is declared.
- `shared` declarations are only allowed in the global scope. They are not
  allowed inside `if`, `while` or `fork` blocks.
- `fork` is not allowed inside `if` or `while` blocks. A `fork` may be nested
  inside another `fork`.
- A forked thread can only access shared variables. It cannot read the local
  variables of other threads.
- Conditions of `if` and `while` must be `bool`. Assignments and initialisers
  must match the type of the variable they assign to.
- `lock` and `unlock` only accept variables of type `lock`.

Each `fork` block runs on its own Sprockell core. Output is printed as
`Sprockell <n> says <value>`, where `0` is the main thread.

## Project structure

```
app/Main.hs           Executable entry point
src/Parser.hs         Parsec lexer/parser -> AST (Program, Instr, Expr)
src/Elaborator.hs     Type checking, variable renaming, bool -> int lowering, fork numbering
src/CodeGen.hs        Memory allocation and Sprockell code generation; runCode / runFile
test/Spec.hs          HSpec test suite (system and unit tests)
test/demos/
  mandatory/          Banking system and Peterson's algorithm
  legal/              Valid example programs (p0-p8)
  illegal/            Programs that must be rejected (contextual and semantic errors)
```

`src/MyParser.hs` and `src/MyCodeGen.hs` are left over from the course boilerplate
(a Fibonacci example) and are not used by the Stone compiler.

The compilation pipeline in `CodeGen.runCode` is:

```
runParseProgram -> elaborateProgram -> checkProgram -> removeBValFromProgram -> codeGen -> Sprockell.run
```

## Prerequisites

You need [Stack](https://docs.haskellstack.org/en/stable/README/) version 2.7 or
higher. Stack installs the right GHC version and the dependencies, including
Sprockell, which is pinned to a specific commit in `stack.yaml`.

```
stack --version
```

## Building

```
stack build
```

## Running

`stack run` runs the program at the path hard-coded in `app/Main.hs`. To run
another program, change the path passed to `runFile` there.

You can also compile and run a program from GHCi:

```
stack ghci
ghci> runFile "test/demos/mandatory/peterson"
Sprockell 0 says 25
ghci> runCode "print((1+2)*10)"
Sprockell 0 says 30
```

## Testing

```
stack test
```

The test suite in `test/Spec.hs` contains:

- **System tests** run every demo program in `test/demos/` and compare its
  output. They also check that the illegal programs are rejected or do not
  terminate.
- **Unit tests** cover the parser, type checker, elaborator and code generator
  separately.

## License

MIT. See `LICENSE`.
