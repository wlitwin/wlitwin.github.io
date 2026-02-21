Here's the compilation pipeline:

```
  Source string
      │
      ▼
  ┌──────────┐
  │ 1. LEXER │  lib/lexer.ml
  │          │  string → Token.token list
  └────┬─────┘
       │
       ▼
  ┌───────────┐
  │ 2. PARSER │  lib/parser.ml
  │           │  token list → Ast.program (= Ast.decl list)
  └────┬──────┘
       │
       ▼
  ┌────────────────┐
  │ 3. TYPECHECKER │  lib/typechecker.ml
  │                │  Ast.program → tprogram (= tdecl list of texpr)
  │  3a. synth/    │  Hindley-Milner inference with unification
  │      check     │  Attaches Types.ty to every node
  │                │
  │  3b. fundep    │  improve_fundeps_in_expr (before generalize)
  │      improve   │  apply_fundep_improvement (whole-program post-pass)
  │                │
  │  3c. transform │  transform_constraints
  │      constraints│  Rewrites typeclass calls → dictionary field accesses
  └────┬───────────┘
       │
       ▼
  ┌──────────────┐
  │ 4. COMPILER  │  lib/compiler.ml
  │              │  tprogram → Bytecode.compiled_program
  │              │  Walks texpr, emits stack-based bytecode
  └────┬─────────┘
       │
       ▼
  ┌──────────────┐
  │ 5. VM        │  lib/vm.ml
  │              │  compiled_program → Bytecode.value
  │              │  Stack-based, 81 opcodes, tail-call opt
  └──────────────┘
```

  Key IRs:

- Ast.expr — untyped, sugar included (EFor, EAnnot, etc.)
- texpr — typed, desugared (every node has ty: Types.ty), constraints transformed to explicit dict passing
- Bytecode.opcode — stack-based instructions (CONST, ADD, CALL, CLOSURE, JUMP_IF_FALSE, etc.)

  Where optimizations would go:

  There are two natural insertion points:

  1. Between 3c and 4 — typed AST optimization (new pass over tprogram/texpr). This is the richest IR since you have full type info +
  tree structure. Good for:
    - Constant folding: 3 + 5 → 8, "a" ^ "b" → "ab"
    - Dead branch elimination: if true do X else Y → X
    - Match simplification: single-arm matches, wildcard-only, known-tag matches
    - Inlining simple functions: let f x = x + 1 in f 3 → 4
    - Let flattening: let x = 5 in x → 5
  2. After 4 — bytecode peephole optimization (new pass over opcode array). Good for:
    - Redundant load/store: SET_LOCAL n; GET_LOCAL n → DUP; SET_LOCAL n
    - Jump threading: JUMP A where A is JUMP B → JUMP B
    - Constant propagation through stack: CONST 1; CONST 2; ADD → CONST 3
    - Pop after push: CONST x; POP → nothing
    - Tail call detection improvements

  The typed AST pass (option 1) is probably the bigger win since it can do structural transformations. The bytecode peephole (option 2)
  catches mechanical redundancies the compiler generates. Both are additive — they'd be new files/functions without changing existing
  code.
