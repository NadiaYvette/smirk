# smirk — State

Operational frontier. Read with [`PLAN.md`](PLAN.md); update before ending a session.

## Where we are

The multi-package workspace for `smirk` conforms to the Cellarius project constellation directory structure conventions under `packages/*`, documented to parity with Polytropos, Penelope, and Gargamelle, and prepared for Hackage distribution under GHC 9.14.1 / Cabal 3.18.
All four packages (`smirk`, `smirk-effectful`, `smirk-polysemy`, `smirk-penelope`) pass `cabal check` with 0 errors and 0 warnings, and all 5 physical verification gates pass cleanly.

### Package Architecture
- **`smirk` (`packages/smirk/`)**: Pure, native PCRE engine, Megaparsec parser, AST compiler, Pike VM with fuel-bounded step budget, sequence polymorphism via `RegexTarget`, native POSIX/Git globbing, and compile-time QuasiQuoters (`[re|...|]`, `[ed|...|]`, `[glob|...|]`).
- **`smirk-effectful` (`packages/smirk-effectful/`)**: First-class algebraic effect (`RegexEngine`) and pure/fuel-bounded interpreters for `effectful`.
- **`smirk-polysemy` (`packages/smirk-polysemy/`)**: Extensible effect (`RegexEngine`) and pure/fuel-bounded interpreters for `polysemy`.
- **`smirk-penelope` (`packages/smirk-penelope/`)**: Algebraic effect façade (`RegexEngine`) and vector-dispatched interpreters for `penelope` over flat Polytropos row vectors.

### Code Quality & Static Analysis Gates [COMPLETED]
- **Toolchain**: GHC 9.14.1 / Cabal 3.18.1.0 targeting `-Wall -Wcompat -Wredundant-constraints`.
- **HLint Cleanliness**: 0 hints across production source files and test suites under `.hlint.yaml`.
- **Physical Verification**: `./bin/smirk-verify` active with 5 verification gates (HLint, Cabal Test Suite, Tagref Cross-References, Markdown Link Integrity, CLOC Comment Density at 28.0%).
- **Git Governance**: `.githooks/commit-msg` enforcing Sarutahiko RFC 2822 commit trailer rules; `.githooks/pre-commit` guarding Tagref and link consistency.
- **Hackage Readiness**: `cabal check` clean (0 errors, 0 warnings) across all four packages (`smirk`, `smirk-effectful`, `smirk-polysemy`, `smirk-penelope`), including full licenses, per-package CHANGELOG.md, per-package README.md, source-repository head stanzas, homepage, bug-reports, and strict PVP upper bounds.

### Active Frontier
- **Cellarius Project Constellation Restructuring & Penelope Façade [COMPLETED]:**
  - Merged `campaign/smirk-scaffold` into `main`.
  - Restructured directory tree to conform to Cellarius standards (`packages/smirk`, `packages/smirk-effectful`, `packages/smirk-polysemy`, `packages/smirk-penelope`).
  - Implemented `smirk-penelope` package with algebraic effect `RegexEngine`, smart operations `matchRegex`, `matchRegexFuel`, `findAllRegex`, and handlers/runners `runRegexEnginePure`, `runRegexEngineWithFuel`, `runRegexEngineM`, `runRegexEngineWithFuelM`.
  - Fixed 100% of Haddock warnings across all packages (0 warnings, 100% coverage).
- **Testing Status:** 35/35 tests passing (25 in `smirk-test`, 2 in `smirk-effectful-test`, 2 in `smirk-polysemy-test`, 6 in `smirk-penelope-test`).
- **Verification Status:** All 5 physical verification checks (`bin/smirk-verify`) passing cleanly with 0 warnings, 0 hints, 0 dangling tagref references, 0 broken markdown links (117 checked across 25 files), and 28.0% comment density in src/.
