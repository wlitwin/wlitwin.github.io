# The MiniML Language

MiniML is a statically-typed functional programming language that blends ML's powerful type inference with Haskell-style type classes and algebraic effects. Programs are fully type-checked at compile time with almost no annotations required — the compiler infers types through Hindley-Milner unification — while type classes provide ad-hoc polymorphism for operators and common interfaces.

What makes MiniML distinctive is how it combines features that rarely coexist in a single language: structural record subtyping (like TypeScript), type classes with dictionary passing (like Haskell), algebraic effects with first-class continuations (like Eff/Koka), and controlled mutability (like OCaml) — all unified under a clean, expression-oriented syntax.

## A Taste of the Language

```
-- A polymorphic binary tree with derived printing
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
  deriving Show

-- Insert into a binary search tree (requires Ord constraint)
let rec insert x t where Ord 'a =
  match t with
  | Leaf -> Node (Leaf, x, Leaf)
  | Node (left, v, right) ->
    if x < v do Node (insert x left, v, right)
    else if x > v do Node (left, v, insert x right)
    else t

-- Build a tree from a list using fold
let tree =
  for x in [5; 3; 7; 1; 4; 6; 8] with t = Leaf do
    insert x t
  end

print $"tree: {show tree}"

-- Flatten to sorted list using algebraic effects
effect Yield = yield : int -> unit

let rec emit t =
  match t with
  | Leaf -> ()
  | Node (left, v, right) ->
    emit left;
    perform yield v;
    emit right

let sorted =
  let mut result = [] in
  try emit tree
  with yield (v) -> result := v :: result;
  List.rev result

print $"sorted: {show sorted}"
```

GADTs let you encode type-level invariants directly in constructors, and
locally abstract types enable polymorphic recursion over them:

```
type 'a expr =
  | IntLit : int -> int expr
  | BoolLit : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

let rec (type 'a) eval (e : 'a expr) : 'a =
  match e with
  | IntLit n -> n
  | BoolLit b -> b
  | Add (a, b) -> eval a + eval b
  | If (cond, t, f) -> if eval cond do eval t else eval f
```

## Key Features

- **Type inference** — Types are inferred; annotations are optional and only needed for disambiguation
- **Type classes** — `Num`, `Eq`, `Ord`, `Show`, `Iter`, and more, with `where` constraints and `deriving`
- **GADTs** — Generalized Algebraic Data Types for encoding richer invariants in constructors, with polymorphic recursion via `(type 'a)` for type-safe evaluators, formatters, and more
- **Algebraic effects** — First-class effect handlers with one-shot and multi-shot continuations; effects are tracked in the type system with optional explicit annotations (`/ IO`, `/ pure`) and unhandled effects are caught at compile time
- **Pattern matching** — Exhaustiveness-checked, with guards, or-patterns, as-patterns, and destructuring
- **Structural records** — Width subtyping: a function expecting `{x: int}` accepts `{x: int; y: string}`
- **Modules** — Namespacing with `pub`/private visibility, opaque types, selective `open`
- **Rich collections** — Lists, arrays, maps, and sets with literal syntax and iteration support
- **Controlled mutability** — `let mut` for mutable locals, `mut` fields in records, `:=` for assignment
- **Looping** — Unified `for` syntax: conditional loops (`for cond do ... end`), iterator-based `for`/`in` loops, fold loops (`for x in xs with acc = init do ... end`), infinite loops (`for do ... end`), and while-let (`for let pat = expr do ... end`)
- **Control flow** — `break`, `continue`, and `return` as loop control primitives
- **Mutual recursion** — `let rec ... and ...` for functions, `type ... and ...` for types
- **Lazy sequences** — Infinite sequences powered by algebraic effects under the hood
- **Pipe operator** — `value |> f |> g` for readable data transformation pipelines

## Documentation

- **[Syntax Reference](syntax.md)** — Complete guide to expressions, operators, patterns, and declarations
- **[Type System](types.md)** — Type inference, polymorphism, type classes, structural subtyping, and how it compares to OCaml and Haskell
- **[Algebraic Effects](effects.md)** — Effect declarations, handlers, continuations, and practical patterns
- **[Standard Library](stdlib.md)** — Built-in functions, type classes, and module reference
- **[Module System](modules.md)** — Defining modules, visibility, opaque types, and open declarations
- **[JavaScript VM](js-vm.md)** — Running MiniML programs in Node.js or the browser
