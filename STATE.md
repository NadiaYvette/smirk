# smirk — State

Operational frontier. Read with [`PLAN.md`](PLAN.md); update before ending a session.

## Where we are

The multi-package workspace for `smirk` is fully retrofitted via `gachi`, documented to parity with Polytropos and Penelope, and prepared for Hackage distribution under GHC 9.12.4 / Cabal 3.18.
All three packages (`smirk`, `smirk-effectful`, `smirk-polysemy`) pass `cabal check` with 0 errors and 0 warnings, and all 5 physical verification gates pass cleanly.

### Package Architecture
- **`smirk` (Root)**: Pure, native PCRE engine, Megaparsec parser, AST compiler, Pike VM with fuel-bounded step budget, sequence polymorphism via `RegexTarget`, native POSIX/Git globbing, and compile-time QuasiQuoters (`[re|...|]`, `[ed|...|]`, `[glob|...|]`).
- **`smirk-effectful` (`effectful/`)**: First-class algebraic effect (`RegexEngine`) and pure/fuel-bounded interpreters for `effectful`.
- **`smirk-polysemy` (`polysemy/`)**: Extensible effect (`RegexEngine`) and pure/fuel-bounded interpreters for `polysemy`.

### Code Quality & Static Analysis Gates [COMPLETED]
- **Toolchain**: GHC 9.12.4 / Cabal 3.18.1.0 targeting `-Wall -Wcompat -Wredundant-constraints`.
- **HLint Cleanliness**: 0 hints across production source files and test suites under `.hlint.yaml`.
- **Physical Verification**: `./bin/smirk-verify` active with 5 verification gates (HLint, Cabal Test Suite, Tagref Cross-References, Markdown Link Integrity, CLOC Comment Density at 27.3%).
- **Git Governance**: `.githooks/commit-msg` enforcing Sarutahiko RFC 2822 commit trailer rules; `.githooks/pre-commit` guarding Tagref and link consistency.
- **Hackage Readiness**: `cabal check` clean (0 errors, 0 warnings) across all three packages (`smirk`, `smirk-effectful`, `smirk-polysemy`), including full licenses, per-package CHANGELOG.md, per-package README.md, source-repository head stanzas, homepage, bug-reports, and strict PVP upper bounds.

### Active Frontier
- **Documentation Parity & Hackage Preparation [COMPLETED]:**
  - Expanded root `README.md` with emoticon grounding (;-P / ;-)), ReDoS defense motivation, 3-package architecture, worked examples across all 7 subsystems, and verification discipline.
  - Expanded `docs/INDEX.md` with complete 13-asset inventory matrix, 5 curated audience reading paths, and multi-package directory catalog.
  - Created canonical design monograph `docs/notes/SMIRK_DESIGN.md` detailing decisions D1–D10 and grounded notes architecture.
  - Expanded `docs/TUTORIAL.md` with comprehensive 8-lesson hands-on guide.
  - Expanded `docs/NEWBIE_GUIDE.md` with 3-level progressive disclosure funnel, full symbol navigation table, Pike VM mental model, and quickstart recipes.
  - Expanded `docs/MAINTAINER_GUIDE.md` with invariants I1–I8, 5-gate physical verification discipline, and triage runbooks A–E.
  - Full Hackage metadata and per-package docs in `smirk`, `effectful/`, and `polysemy/`.
- **Testing Status:** 29/29 tests passing (25 in `smirk-test`, 2 in `smirk-effectful-test`, 2 in `smirk-polysemy-test`).
- **Verification Status:** All 5 physical verification checks (`bin/smirk-verify`) passing cleanly with 0 warnings, 0 hints, 0 dangling tagref references, 0 broken markdown links (97 checked across 21 files), and 27.3% comment density in src/.
