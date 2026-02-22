# JavaScript VM

The JavaScript VM lets you run MiniML programs in Node.js or the browser. Programs are compiled to bytecode by the OCaml compiler, serialized to JSON, and executed by the JS VM.

```
OCaml Compiler                    JS VM
┌──────────────────────┐          ┌──────────────────────┐
│ Source → Parse →     │          │ Load JSON bundle     │
│ Typecheck → Compile  │── JSON ──│ Register JS builtins │
│ → Serialize          │  bundle  │ Replay stdlib setup  │
└──────────────────────┘          │ Execute main program │
                                  └──────────────────────┘
```

## Quick Start

### Node.js

```bash
# Compile a program to a JSON bundle
dune exec bin/main.exe -- --emit-json program.mml > bundle.json

# Run it
node -e "require('./js/loader').loadBundle(require('fs').readFileSync('bundle.json','utf-8'))"
```

### Browser

```bash
# Generate the browser bundle (one-time)
node js/browser.js

# Compile a program
dune exec bin/main.exe -- --emit-json program.ml > js/sample_bundle.json

# Open the playground
open js/index.html
```

The playground page lets you paste a JSON bundle and run it in the browser.

## JSON Bundle Format

The `--emit-json` flag produces a JSON bundle containing everything needed to run the program:

```json
{
  "version": 1,
  "global_names": ["print", "Stdlib.print", "String.length", ...],
  "native_globals": { ... },
  "setup": [ <prototype>, ... ],
  "main": <prototype>
}
```

### Fields

| Field | Description |
|-------|-------------|
| `version` | Format version (currently 1) |
| `global_names` | Array of all global variable names, indexed by slot |
| `native_globals` | Map of slot index → native function or typeclass dictionary |
| `setup` | Array of prototypes that rebuild the source-compiled stdlib |
| `main` | The main program prototype |

### Prototypes

A prototype represents a compiled function:

```json
{
  "name": "factorial",
  "arity": 1,
  "num_locals": 2,
  "code": [["CONST",0], ["GET_LOCAL",0], ["ADD"], ["RETURN"]],
  "constants": [{"t":"i","v":42}, {"t":"p","v":{...}}]
}
```

Opcodes are encoded as string-tagged arrays: `["CONST",0]`, `["ADD"]`, `["CLOSURE",2,[["local",1],["upvalue",3]]]`.

### Constant Value Tags

| Tag | Type | Example |
|-----|------|---------|
| `i` | int | `{"t":"i","v":42}` |
| `f` | float | `{"t":"f","v":3.14}` |
| `b` | bool | `{"t":"b","v":true}` |
| `s` | string | `{"t":"s","v":"hello"}` |
| `y` | byte | `{"t":"y","v":65}` |
| `r` | rune | `{"t":"r","v":955}` |
| `u` | unit | `{"t":"u"}` |
| `p` | prototype | `{"t":"p","v":{...}}` |
| `T` | tuple | `{"t":"T","v":[...]}` |
| `L` | list | `{"t":"L","v":[...]}` |
| `V` | variant | `{"t":"V","tag":1,"name":"Some","payload":{...}}` |

### Native Globals

Native globals describe builtins that need JS reimplementations:

```json
{
  "0": { "type": "external", "name": "print", "arity": 1 },
  "54": {
    "type": "dict",
    "fields": {
      "+": { "name": "num_add_int", "arity": 2 },
      "-": { "name": "num_sub_int", "arity": 2 }
    }
  }
}
```

- `external`: A single native function
- `dict`: A typeclass dictionary (record of native functions)

## How It Works

The stdlib has two kinds of functions:

1. **Native functions** (OCaml `VExternal`) — These need JS reimplementations. Examples: `print`, `String.length`, `Array.make`, arithmetic operators. The JS VM registers equivalent JS functions at the same global slots.

2. **Source-compiled functions** — These are written in MiniML source and compiled to bytecode. Examples: `List.map`, `Seq.range`, `Enum.join`. Their bytecode is captured during stdlib initialization and included in the `setup` array. The JS VM replays these setup prototypes to rebuild the stdlib state.

When loading a bundle, the JS VM:

1. Creates a fresh VM with the global name table
2. Registers all native builtins at their slot indices
3. Executes each setup prototype in order (rebuilds source-compiled stdlib)
4. Executes the main prototype

## File Overview

| File | Description |
|------|-------------|
| `lib/serialize.ml` | OCaml bytecode-to-JSON serializer |
| `lib/interp.ml` | Capture mode for stdlib setup bytecodes |
| `bin/main.ml` | `--emit-json` CLI flag |
| `js/vm.js` | VM core: value types, dispatch loop, fibers, effects |
| `js/builtins.js` | JS implementations of ~120 native functions |
| `js/loader.js` | JSON deserialization and bundle execution |
| `js/index.html` | Browser playground page |
| `js/test.js` | Test harness (107 tests) |
| `js/browser.js` | Build script for browser bundle |
| `js/miniml.bundle.js` | Generated browser bundle (~40 KB) |

## Capturing Print Output

The VM routes `print` calls through `globalThis._vmOutput` if set. This lets you capture output in tests or the browser:

```javascript
const outputs = [];
globalThis._vmOutput = (s) => outputs.push(s);
const result = MiniML.loadBundle(jsonString);
globalThis._vmOutput = null;
// outputs now contains all printed lines
```

## Browser API

After loading `miniml.bundle.js`, the `MiniML` global provides:

```javascript
MiniML.loadBundle(jsonString)  // Execute a JSON bundle, returns result value
MiniML.ppValue(value)          // Format a value as a string
MiniML.RuntimeError            // Error class for runtime errors
MiniML.VUNIT                   // The unit value
```

## Limitations

- **IO/Sys**: File operations (`IO.read_file`, `IO.write_file`) are Node.js only. In the browser, they throw errors.
- **Runtime.eval**: Not supported in the JS VM (requires the OCaml compiler).
- **Integer semantics**: JS uses IEEE 754 doubles (safe integers up to 2^53) vs OCaml's 63-bit integers. This is fine for most programs.
- **Compilation**: The OCaml compiler is still needed to produce JSON bundles. The JS VM only handles execution.

## Running Tests

```bash
# Build the OCaml compiler first
dune build

# Run JS VM tests (107 tests covering all major features)
node js/test.js

# Run OCaml tests (622 tests)
dune test
```

## Adding New Builtins

When a new native function is added to the OCaml side:

1. Add it to `interp.ml` or `std.ml` (OCaml implementation)
2. The serializer automatically includes it in `native_globals`
3. Add a matching JS implementation in `js/builtins.js`:

   ```javascript
   reg("Module.function_name", arity, (args) => {
     // implementation
     return vm.vstring("result");
   });
   ```

4. Regenerate the browser bundle: `node js/browser.js`
