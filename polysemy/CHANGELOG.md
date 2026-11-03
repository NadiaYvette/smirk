# Changelog for `smirk-polysemy`

All notable changes to the `smirk-polysemy` package will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the Haskell Package Versioning Policy (PVP).

## [0.1.0.0] — 2026-10-24

### Added
- **`Smirk.Effect.Polysemy`**: Extensible effect bindings for Smirk's PCRE engine in the `polysemy` ecosystem:
  - `SmirkRegex`: Polysemy higher-order effect for regex matching, searching, and substitution.
  - `runSmirkPure`: Pure in-memory interpreter.
  - `runSmirkFuel`: Fuel-bounded interpreter enforcing ReDoS bounds and raising `OutOfFuel` on budget exhaustion.
  - Smart constructors: `match`, `search`, `substitute`, `substituteAll`.
- Full Hspec test suite verifying pure and fuel-bounded execution.
