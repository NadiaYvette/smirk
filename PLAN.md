# smirk — Strategic Roadmap (`PLAN.md`)

**Status:** ACTIVE STRATEGIC ROADMAP  
**Scope:** Ecosystem workspace for smirk  
**Documentation:** [`docs/INDEX.md`](docs/INDEX.md)  
**Operational State:** [`STATE.md`](STATE.md)  

---

## Phase 1: Core Engine Architecture & Effect Decoupling [COMPLETED]

- [x] **Milestone 1.1: Core PCRE Engine & Pike Virtual Machine**
  - Megaparsec PCRE parser supporting literals, classes, sequences, alternations, repetitions, anchors, lookarounds, and backreferences.
  - Bytecode compiler and Pike VM execution engine with explicit heap-allocated backtracking stack.
  - Step-bounded execution (`Fuel`) and pausable/resumable continuations (`MatchContinuation`).
  - Sequence polymorphism via `RegexTarget` (`Text`, `ByteString`, `String`, `Seq Char`, `Vector Char`).

- [x] **Milestone 1.2: Compile-Time QuasiQuoters & Glob Engine**
  - `[re|...|]`: Compile-time validated PCRE regular expressions.
  - `[ed|search///replace|]`: Compile-time validated stream-editor search and replace.
  - `[glob|...|]`: Native POSIX and Git-style globbing with recursive wildcard compilation.

- [x] **Milestone 1.3: Decoupled Multi-Package Effect Architecture**
  - Split into three modular packages: `smirk` (pure core), `smirk-effectful` (`effectful` algebraic effects), and `smirk-polysemy` (`polysemy` extensible effects).
  - Pure and fuel-bounded interpreters for both effect systems.

- [x] **Milestone 1.4: Gachi Tooling & Governance Retrofit**
  - Installed `.githooks/` with Sarutahiko RFC 2822 commit attribution trailer enforcement.
  - Installed 5-gate physical verification runner (`bin/smirk-verify`), Tagref harness, and CLOC audit tool.
  - Grounded canonical documentation corpus: `docs/INDEX.md`, `docs/TUTORIAL.md`, `docs/NEWBIE_GUIDE.md`, `docs/MAINTAINER_GUIDE.md`, `docs/notes/SMIRK_DESIGN.md`, `docs/notes/DOC_STRATEGY.md`, and `docs/notes/COMMIT_ATTRIBUTION.md`.

- [x] **Milestone 1.5: Hackage Upload Preparation & Parity Release**
  - Added strict PVP upper bounds to all dependencies across `smirk.cabal`, `smirk-effectful.cabal`, and `smirk-polysemy.cabal`.
  - Added complete Apache-2.0 `LICENSE`, `CHANGELOG.md`, and `README.md` to every package.
  - Configured `homepage`, `bug-reports`, `source-repository head`, and `extra-doc-files`.
  - Verified `cabal check` passes with 0 errors and 0 warnings across all 3 packages.
  - Passed all 5 physical verification gates in `./bin/smirk-verify`.

---

## Phase 2: Performance Profiling & Optimization [UPCOMING]

- [ ] **Milestone 2.1:** High-Throughput Microbenchmarks (Criterion benchmark suite comparing against libpcre2).
- [ ] **Milestone 2.2:** Instruction Vector Specialization and Unboxed Program Representation.
- [ ] **Milestone 2.3:** Substring DFA Fast-Path for Pure Literal Prefixes.
