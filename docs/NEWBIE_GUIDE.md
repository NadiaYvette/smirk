# Smirk Newbie Quickstart & Token-Conservation Guide

**Audience:** New human contributors and AI coding assistant drivers (Antigravity, Claude Code, Cursor, Aider).  
**Parent Strategy:** [`docs/notes/DOC_STRATEGY.md`](notes/DOC_STRATEGY.md)  
**Corpus Inventory:** [`docs/INDEX.md`](INDEX.md)  
**Interactive Tutorial:** [`docs/TUTORIAL.md`](TUTORIAL.md)  
**Maintainer Guide:** [`docs/MAINTAINER_GUIDE.md`](MAINTAINER_GUIDE.md)  

---

## 1. The Progressive Disclosure Funnel

To conserve context tokens, reduce latency, and prevent attention degradation, follow the 3-level progressive disclosure funnel:

```
Level 0: Global Navigation  (docs/INDEX.md, package overview)      →   ~10–50 tokens
Level 1: Structural Outline (module export lists, type headers)   →   ~100–250 tokens
Level 2: Targeted Slice     (view_file StartLine / EndLine)       →   ~200–500 tokens
```

- **Level 0 (Global):** Start by identifying the responsible module from the symbol navigation table below.
- **Level 1 (Structural):** View only the module export header and type definitions.
- **Level 2 (Targeted):** Use line range slices (`StartLine` / `EndLine`) to examine the exact type or function needed. Never read an entire module when a 25-line slice suffices.

---

## 2. Fast Package & Symbol Navigation

### Core Engine & QuasiQuoters (`smirk`)

| If you are looking for... | Look in module | Source Location |
| :--- | :--- | :--- |
| `compileRegex`, `execMatch`, `testMatch`, `execMatchWithFuel` | `Smirk` | [`packages/smirk/src/Smirk.hs`](../packages/smirk/src/Smirk.hs) |
| `AST`, `CharClass`, `CharRange`, `Anchor`, `RepKind`, `Flags` | `Smirk.Core.AST` | [`packages/smirk/src/Smirk/Core/AST.hs`](../packages/smirk/src/Smirk/Core/AST.hs) |
| `Opcode`, `Program`, `Regex` | `Smirk.Core.Bytecode` | [`packages/smirk/src/Smirk/Core/Bytecode.hs`](../packages/smirk/src/Smirk/Core/Bytecode.hs) |
| `compileAST`, `compileToRegex`, `assignGroupIndices` | `Smirk.Core.Compiler` | [`packages/smirk/src/Smirk/Core/Compiler.hs`](../packages/smirk/src/Smirk/Core/Compiler.hs) |
| `parsePCRE`, `parsePCREWithFlags`, `pAlternation`, `pGroup` | `Smirk.Core.Parser` | [`packages/smirk/src/Smirk/Core/Parser.hs`](../packages/smirk/src/Smirk/Core/Parser.hs) |
| `re`, `ed`, `Editor`, `applyEditor`, `replaceMatch` | `Smirk.Core.Quote` | [`packages/smirk/src/Smirk/Core/Quote.hs`](../packages/smirk/src/Smirk/Core/Quote.hs) |
| `RegexTarget`, `seqLength`, `seqIndex`, `seqSlice`, `seqToText` | `Smirk.Engine.Sequence` | [`packages/smirk/src/Smirk/Engine/Sequence.hs`](../packages/smirk/src/Smirk/Engine/Sequence.hs) |
| `Fuel`, `MatchResult`, `MatchStatus`, `MatchContinuation`, `runMatch`, `stepVM` | `Smirk.Engine.VM` | [`packages/smirk/src/Smirk/Engine/VM.hs`](../packages/smirk/src/Smirk/Engine/VM.hs) |
| `compileGlob`, `compileWildcard`, `matchGlob`, `glob` | `Smirk.Glob` | [`packages/smirk/src/Smirk/Glob.hs`](../packages/smirk/src/Smirk/Glob.hs) |
| Full unit & property tests | `Spec` (Test) | [`packages/smirk/test/Spec.hs`](../packages/smirk/test/Spec.hs) |

### Effectful Ecosystem (`smirk-effectful`)

| If you are looking for... | Look in module | Source Location |
| :--- | :--- | :--- |
| `RegexEngine`, `matchRegex`, `runRegexEnginePure`, `runRegexEngineWithFuel` | `Smirk.Effect.Effectful` | [`packages/smirk-effectful/src/Smirk/Effect/Effectful.hs`](../packages/smirk-effectful/src/Smirk/Effect/Effectful.hs) |
| Effectful test suite | `Spec` (Test) | [`packages/smirk-effectful/test/Spec.hs`](../packages/smirk-effectful/test/Spec.hs) |

### Polysemy Ecosystem (`smirk-polysemy`)

| If you are looking for... | Look in module | Source Location |
| :--- | :--- | :--- |
| `RegexEngine`, `matchRegex`, `runRegexEnginePure`, `runRegexEngineWithFuel` | `Smirk.Effect.Polysemy` | [`packages/smirk-polysemy/src/Smirk/Effect/Polysemy.hs`](../packages/smirk-polysemy/src/Smirk/Effect/Polysemy.hs) |
| Polysemy test suite | `Spec` (Test) | [`packages/smirk-polysemy/test/Spec.hs`](../packages/smirk-polysemy/test/Spec.hs) |

### Penelope Ecosystem (`smirk-penelope`)

| If you are looking for... | Look in module | Source Location |
| :--- | :--- | :--- |
| `RegexEngine`, `matchRegex`, `runRegexEnginePure`, `runRegexEngineM` | `Smirk.Effect.Penelope` | [`packages/smirk-penelope/src/Smirk/Effect/Penelope.hs`](../packages/smirk-penelope/src/Smirk/Effect/Penelope.hs) |
| Penelope test suite | `Spec` (Test) | [`packages/smirk-penelope/test/Spec.hs`](../packages/smirk-penelope/test/Spec.hs) |

---

## 3. The Pike VM Mental Model

Traditional regular expression engines in scripting languages use recursive backtracking. This implementation model exhibits two critical weaknesses:
1. It relies on the operating system's native call stack, which can overflow on deep nesting.
2. It cannot pause or yield execution without terminating the computation.

### How Smirk Executes Bytecode
Smirk compiles regular expressions into linear vectors of `Opcode` instructions:
- Concatenation is represented as sequential instructions (`OpChar 'a'`, `OpChar 'b'`).
- Alternation (`a|b`) and repetition (`a*`, `a+`) are represented using `OpSplit d1 d2`.
- Execution maintains an active `ThreadState` containing program counter `pc`, character index `strIdx`, and capture registers `saves`.
- When branching, `branch1` is pursued immediately, while `branch2` is pushed to an explicit heap list (`[ThreadState]`).
- On mismatch, the engine pops the next alternative from the heap stack.
- Each transition consumes 1 fuel unit. If the budget is exhausted, the entire machine state is bundled into a `MatchContinuation` for resumption.

---

## 4. 15-Minute Quickstart Recipes

### Recipe 1: Compile-Time Regex Validation
```haskell
{-# LANGUAGE QuasiQuotes #-}
import Smirk

-- Detects syntax errors at compile time:
let myPattern = [re|[0-9]{3}-[0-9]{4}|]
testMatch myPattern ("555-0199" :: Text) -- True
```

### Recipe 2: Safe Search with ReDoS Fuel Budget
```haskell
import Smirk

let reg = [re|(a+)+$|]
let untrustedInput = "aaaaaaaaaaaaaaaaaaaaaaaaaaaa!" :: Text

case execMatchWithFuel reg untrustedInput (StepBudget 10000) of
  Matched res      -> putStrLn "Matched"
  NoMatch          -> putStrLn "No match"
  OutOfFuel _cont  -> putStrLn "ReDoS averted: step ceiling reached."
```

### Recipe 3: Search and Replace with Capture Substitution
```haskell
{-# LANGUAGE QuasiQuotes #-}
import Smirk

let edScript = [ed|key:([a-z]+)///val:${1}|]
let result = applyEditor edScript "key:foo and key:bar"
-- result == "val:foo and val:bar"
```

### Recipe 4: Zero-Copy Matching on ByteStrings
```haskell
import qualified Data.ByteString.Char8 as BS
import Smirk

let reg = [re|HTTP/1\.[01]|]
testMatch reg (BS.pack "GET / HTTP/1.1\r\n") -- True
```

---

## 5. Next Steps

- For a complete walkthrough of every feature, read [`docs/TUTORIAL.md`](TUTORIAL.md).
- For design decisions and invariants, read [`docs/notes/SMIRK_DESIGN.md`](notes/SMIRK_DESIGN.md).
- For verification commands and triage runbooks, read [`docs/MAINTAINER_GUIDE.md`](MAINTAINER_GUIDE.md).
