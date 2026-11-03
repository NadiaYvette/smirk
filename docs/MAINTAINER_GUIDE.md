# Smirk Maintainer Guide

**Audience:** Repository maintainers, reviewers, and automated release engineers.  
**Corpus Inventory:** [`docs/INDEX.md`](INDEX.md)  
**Parent Strategy:** [`docs/notes/DOC_STRATEGY.md`](notes/DOC_STRATEGY.md)  
**Design Monograph:** [`docs/notes/SMIRK_DESIGN.md`](notes/SMIRK_DESIGN.md)  
**Commit Attribution:** [`docs/notes/COMMIT_ATTRIBUTION.md`](notes/COMMIT_ATTRIBUTION.md)  

---

## 1. Architectural Invariants

Every change set submitted to `smirk` must preserve eight foundational architectural invariants:

### I1: The Precedence Rule (`DOC_STRATEGY.md` §4)
- **Code wins at runtime:** When documentation and implementation diverge, the running code defines runtime reality.
- **Notes are corrected regardless:** Stale documentation, comments, or design notes must be updated in the exact same change set. Silent drift is prohibited.

### I2: Total Step-Bounding & ReDoS Immunity
- Every instruction execution in `stepVM` must consume exactly 1 unit of `Fuel`.
- When running under `StepBudget budget`, the engine must halt and yield `OutOfFuel` when steps reach the budget, preventing CPU lockup.

### I3: Resumable Continuation Completeness
- When `OutOfFuel` is generated, the captured `MatchContinuation` must retain all necessary state (`savedRegex`, `savedTarget`, `savedStartIndex`, `currentThread`, `backtrackStack`, `currentSteps`).
- Resuming execution with `resumeMatch` must produce identical match results to unbounded execution without repeating previous steps.

### I4: Container Polymorphism Purity
- The core virtual machine must operate strictly through the `RegexTarget` abstraction.
- No internal VM instruction may force conversion of target sequences to intermediate `Text` or `ByteString` unless explicitly extracting slices for results.

### I5: Zero C FFI & 100% Native Memory Safety
- The engine must remain 100% pure Haskell.
- Foreign function interface (FFI) bindings to C libraries (`libpcre`, `libpcre2`) are strictly prohibited in the core engine.

### I6: Effect Decoupling Independence
- The root `smirk` package must remain entirely free of effect system dependencies.
- `smirk-effectful` and `smirk-polysemy` must remain completely decoupled from one another: neither may depend on the other effect library.

### I7: Compile-Time Syntax Safety
- QuasiQuoters (`[re|...|]`, `[ed|...|]`, `[glob|...|]`) must validate syntax during GHC compile time and fail the build if a pattern is syntactically invalid.

### I8: Zero-Warning Compiler & Linter Cleanliness
- Every package in Smirk must compile cleanly under `-Wall -Wcompat -Wredundant-constraints`.
- HLint must pass with 0 hints across all source packages.

---

## 2. The 5-Gate Physical Verification Discipline

Before pushing commits or publishing releases, all 5 physical verification gates must pass:

```bash
./bin/smirk-verify
```

| Gate | Check | Command / Tool | Invariant |
| :--- | :--- | :--- | :--- |
| **Gate 1** | HLint Static Analysis | `hlint src effectful/src polysemy/src` | 0 hints allowed (`.hlint.yaml`) |
| **Gate 2** | Full Cabal Test Suite | `cabal test all` | 100% pass across all packages |
| **Gate 3** | Tagref Cross-References | `bin/smirk-tagref` | 0 dangling references, 0 duplicate tags |
| **Gate 4** | Markdown Link Integrity | `bin/smirk-linkcheck --quiet` | 0 broken links or anchors |
| **Gate 5** | CLOC Comment Density | `bin/smirk-cloc --quiet --check` | >= 25.0% comment density threshold |

---

## 3. Maintainer Triage Runbooks

### Runbook A: ReDoS Test Suite Failures
- **Symptom:** `test/VMSpec.hs` fails on step-bounding assertions.
- **Cause:** An instruction transition in `stepVM` bypassed the step counter increment or failed to check fuel exhaustion.
- **Fix:** Ensure that every branch of `stepVM` passes `(steps + 1)` and that `steps >= budget` is checked before evaluating opcodes.

### Runbook B: Tagref Dangling References
- **Symptom:** `bin/smirk-tagref` reports `dangling reference to [ref:<tag>]`.
- **Cause:** A documentation note was renamed, or a reference was cited without declaring the corresponding `[tag:<tag>]`.
- **Fix:** Search for citations using `git grep 'ref:<tag>'` and ensure the tag is defined in source code or design notes.

### Runbook C: Markdown Link Integrity Failure
- **Symptom:** `bin/smirk-linkcheck` reports broken link `file:///...` or relative link mismatch.
- **Cause:** A referenced file was moved or a markdown anchor does not exist.
- **Fix:** Run `bin/smirk-linkcheck` without `--quiet` to see line numbers and exact failing paths. Adjust relative paths (`../`).

### Runbook D: CLOC Comment Density Deficit
- **Symptom:** `bin/smirk-cloc` reports density below the 25.0% threshold.
- **Cause:** New code was introduced without corresponding Haddock documentation or architecture notes.
- **Fix:** Add top-level module documentation, Haddock comments on exposed functions/types, and architectural design notes.

### Runbook E: Hackage Release Preparation
When preparing packages for Hackage upload:
1. Verify `cabal check` produces 0 errors and 0 warnings across all three packages:
   ```bash
   cabal check && (cd effectful && cabal check) && (cd polysemy && cabal check)
   ```
2. Ensure all three `.cabal` files contain:
   - `synopsis`, `description`, `category`
   - `homepage: https://github.com/NadiaYvette/smirk`
   - `bug-reports: https://github.com/NadiaYvette/smirk/issues`
   - `extra-doc-files: README.md, CHANGELOG.md, LICENSE`
   - `license-file: LICENSE`
   - Strict PVP upper bounds on all dependencies
   - `source-repository head` stanza with correct `subdir`
3. Verify that each package has its own `LICENSE`, `CHANGELOG.md`, and `README.md`.
4. Run `./bin/smirk-verify` to ensure all 5 gates pass cleanly.

---

## 4. Commit Attribution & DCO Standards

Every commit must follow the Linux kernel RFC 2822 trailer format:
- Imperative summary under 72 characters.
- Body explains **why** the change was made, not just what code was edited.
- AI assistant contributions must carry an `Assisted-by:` trailer:
  ```
  Assisted-by: Antigravity (Google DeepMind / Gemini 3.8 Flash)
  ```
- AI assistants must **never** be credited via `Co-authored-by:` or `Signed-off-by:`, as only human authors certify the Developer Certificate of Origin (DCO).
- Enforced automatically by `.githooks/commit-msg`.
