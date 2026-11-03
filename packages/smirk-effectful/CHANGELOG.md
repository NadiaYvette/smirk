# Changelog for `smirk-effectful`

All notable changes to the `smirk-effectful` package will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the Haskell Package Versioning Policy (PVP).

## [0.1.0.0] — 2026-10-24

### Added
- **`Smirk.Effect.Effectful`**: First-class algebraic effect for Smirk's PCRE engine in the `effectful` ecosystem:
  - `SmirkRegex`: First-class effect type for regex matching, searching, and substitution.
  - `runSmirkPure`: Pure in-memory interpreter evaluating matches without side effects.
  - `runSmirkFuel`: Step-budgeted interpreter enforcing ReDoS bounds and raising `OutOfFuel` on budget exhaustion.
  - Combinators: `match`, `search`, `substitute`, `substituteAll`.
- Full Hspec test suite verifying pure and fuel-bounded execution.
