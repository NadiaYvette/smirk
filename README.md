# Smirk (;-P) — Pure Native Resumable PCRE Engine in Haskell

**Smirk** is a 100% native Haskell Perl-Compatible Regular Expression (PCRE) engine. It provides high-assurance, pure functional regular expression matching without C dependencies, buffer allocations, or FFI overhead.

## Key Features

1. **Step-Bounded Execution & ReDoS Defense**:
   - Every match operation executes with an explicit **Step / Gas Budget** (`Fuel`).
   - Exponential worst-case backtracking terminates gracefully with `OutOfFuel` rather than locking the runtime thread.

2. **Pausable & Resumable Matching**:
   - Matches can be paused, inspected, and resumed via continuation closures (`MatchContinuation`).

3. **Dual Algebraic Effect System Support**:
   - **`effectful`**: `Smirk.Effect.Effectful` exposes `RegexEngine` as a dynamic first-class effect.
   - **`polysemy`**: `Smirk.Effect.Polysemy` exposes `RegexEngine` with pure and stateful interpreters.

4. **Sequence Polymorphism via `mono-traversable`**:
   - Matches seamlessly across `Data.Text`, `Data.ByteString`, `[Char]`, `Data.Sequence.Seq Char`, `Data.Vector.Vector Char`, and `NonNull` sequences.

5. **Extended PCRE Capabilities**:
   - Positive / negative lookahead (`(?=...)`, `(?!...)`)
   - Positive / negative lookbehind (`(?<=...)`, `(?<!...)`)
   - Named and numbered capture groups (`(?<name>...)`, `(group)`)
   - Backreferences (`\1`, `\k<name>`)
   - Non-capturing groups `(?:...)`
   - Case-insensitive (`(?i)`), dotall (`(?s)`), multiline (`(?m)`) flag modes
