# Changelog for `smirk`

All notable changes to `smirk` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the Haskell Package Versioning Policy (PVP).

## [0.1.0.0] — 2026-10-24

### Added
- **`Smirk.Core.AST`**: Core abstract syntax tree supporting full PCRE constructs: character literals, character classes, escape sequences, concatenation, alternation, bounded/unbounded repetition (greedy, lazy, possessive), lookaround assertions, and named capture groups.
- **`Smirk.Core.Bytecode` & `Smirk.Core.Compiler`**: Bytecode instruction set and non-backtracking VM compilation engine.
- **`Smirk.Engine.VM`**: Resumable, step-bounded virtual machine execution model (`Fuel`, `StepBudget`, `OutOfFuel`, `MatchContinuation`). Provides ironclad Regular Expression Denial of Service (ReDoS) defense by enforcing strict upper bounds on thread state transitions.
- **`Smirk.Engine.Sequence`**: Sequence polymorphism via `mono-traversable`'s `Element seq ~ Char`, supporting `Data.Text.Text`, `Data.ByteString.ByteString`, `[Char]`, `Data.Sequence.Seq Char`, and `Data.Vector.Vector Char`.
- **`Smirk.Core.Quote`**: Compile-time QuasiQuoters:
  - `[re|...|]`: Static PCRE pattern compilation with stage-1 AST validation.
  - `[ed|s/foo/bar/g|]`: Sed-style stream editor substitution syntax.
- **`Smirk.Glob`**: Native POSIX and Git-style globbing with `compileWildcard` and `[glob|...|]` QuasiQuoter.
- Split architecture into modular packages:
  - `smirk`: Pure core engine and quasiquoters.
  - `smirk-effectful`: Algebraic effect interface via `effectful`.
  - `smirk-polysemy`: Extensible effect interface via `polysemy`.
- Canonical tooling overlay via `gachi`: Sarutahiko RFC 2822 commit attribution trailers, 5-gate physical verification runner (`bin/smirk-verify`), and comprehensive documentation corpus.
