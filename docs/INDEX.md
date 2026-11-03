# smirk Documentation Index & Corpus Inventory

Welcome to the `smirk` technical documentation corpus.

`smirk` (;-P / ;-) provides a pure, native Haskell PCRE regular expression engine with step-bounded ReDoS immunity, pausable/resumable continuations, sequence polymorphism, and first-class algebraic effect handlers without C dependencies, buffer allocations, or FFI overhead.

---

## 1. Corpus Inventory Matrix

| Asset | Path | Genre | Target Audience | Authority | Status |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **Project Monograph** | [`README.md`](../README.md) | Overview & Synopsis | All Engineers | Canonical Overview | Current |
| **Documentation Index** | [`docs/INDEX.md`](INDEX.md) | Reading Map & Inventory | All Audiences | Canonical Index | Current |
| **Interactive Tutorial** | [`docs/TUTORIAL.md`](TUTORIAL.md) | Hands-on Tutorial | Developers, API Consumers | Pedagogical Authority | Current |
| **Newbie Quickstart** | [`docs/NEWBIE_GUIDE.md`](NEWBIE_GUIDE.md) | Onboarding & Navigation | New Contributors, AI Agents | Quickstart Authority | Current |
| **Maintainer Guide** | [`docs/MAINTAINER_GUIDE.md`](MAINTAINER_GUIDE.md) | Governance & Operations | Maintainers, Reviewers | Operational Authority | Current |
| **Design Monograph** | [`docs/notes/SMIRK_DESIGN.md`](notes/SMIRK_DESIGN.md) | Design & Architecture | Systems Architects, Security | Invariant Authority | Current |
| **Doc Strategy Policy** | [`docs/notes/DOC_STRATEGY.md`](notes/DOC_STRATEGY.md) | Documentation Strategy | Authors, Maintainers | Policy Authority | Current |
| **Commit Attribution** | [`docs/notes/COMMIT_ATTRIBUTION.md`](notes/COMMIT_ATTRIBUTION.md) | Git & DCO Policy | Contributors, Subagents | Procedural Authority | Current |
| **AI Assistant Rules** | [`AGENTS.md`](../AGENTS.md) | Machine Instructions | AI Coding Assistants | Operational Mandate | Current |
| **Engineering Plan** | [`PLAN.md`](../PLAN.md) | Strategic Roadmap | Planners, Architects | Planning Authority | Current |
| **Operational State** | [`STATE.md`](../STATE.md) | Session Ledger | Pair Programmers | Dynamic State | Current |
| **`smirk-effectful` Package** | [`effectful/README.md`](../effectful/README.md) | Package Overview | Effectful Consumers | Package Authority | Current |
| **`smirk-polysemy` Package** | [`polysemy/README.md`](../polysemy/README.md) | Package Overview | Polysemy Consumers | Package Authority | Current |

---

## 2. Curated Audience Reading Paths

### Path A: The First 15 Minutes (Executive / Newcomer)
1. [`README.md`](../README.md) — The name grounding (;-P / ;-)), ReDoS defense, and quickstart examples.
2. [`docs/NEWBIE_GUIDE.md`](NEWBIE_GUIDE.md) — Fast symbol navigation and 15-minute quickstart recipes.
3. [`docs/TUTORIAL.md`](TUTORIAL.md) §1–§3 — Basic matching, capture groups, and QuasiQuoters.

### Path B: API Consumer & Application Engineer
1. [`docs/TUTORIAL.md`](TUTORIAL.md) §1–§4 — Basic PCRE syntax, QuasiQuoters (`[re|...|]`, `[ed|...|]`), and stream replacement.
2. [`docs/TUTORIAL.md`](TUTORIAL.md) §5 — Sequence polymorphism across `Text`, `ByteString`, and standard collections.
3. [`docs/TUTORIAL.md`](TUTORIAL.md) §7 — Native POSIX and Git-style globbing.

### Path C: Effect System Architect
1. [`effectful/README.md`](../effectful/README.md) & [`docs/TUTORIAL.md`](TUTORIAL.md) §6.1 — `Smirk.Effect.Effectful` integration.
2. [`polysemy/README.md`](../polysemy/README.md) & [`docs/TUTORIAL.md`](TUTORIAL.md) §6.2 — `Smirk.Effect.Polysemy` integration.
3. [`docs/notes/SMIRK_DESIGN.md`](notes/SMIRK_DESIGN.md) D5 — Decoupled multi-package architecture.

### Path D: Systems & Security Engineer (ReDoS Defense)
1. [`docs/notes/SMIRK_DESIGN.md`](notes/SMIRK_DESIGN.md) D1–D3 — Pike VM architecture, explicit backtracking stack, and fuel-bounded execution.
2. [`docs/TUTORIAL.md`](TUTORIAL.md) §3–§4 — Step budgeting, `OutOfFuel`, and pausable/resumable continuations.
3. [`src/Smirk/Engine/VM.hs`](../src/Smirk/Engine/VM.hs) — Core step-bounded virtual machine execution loop.

### Path E: AI Coding Assistant & Maintainer
1. [`AGENTS.md`](../AGENTS.md) — Operational instructions, token conservation, and two-file loop.
2. [`docs/MAINTAINER_GUIDE.md`](MAINTAINER_GUIDE.md) — Architectural invariants and 5-gate physical verification discipline.
3. [`docs/notes/DOC_STRATEGY.md`](notes/DOC_STRATEGY.md) — Precedence rule (code wins at runtime; notes corrected in the same change set).
4. [`bin/smirk-verify`](../bin/smirk-verify) — Automated verification test runner.

---

## 3. Multi-Package Catalog

- **`smirk` (Root)**: Core library containing the Megaparsec parser, AST compiler, Pike VM, sequence polymorphism, and QuasiQuoters.
- **[`effectful/`](../effectful/) (`smirk-effectful`)**: Algebraic effect interface exposing `RegexEngine` and pure/fuel-bounded interpreters for `effectful`.
- **[`polysemy/`](../polysemy/) (`smirk-polysemy`)**: Extensible effect interface exposing `RegexEngine` and pure/fuel-bounded interpreters for `polysemy`.

---

## 4. Physical Verification Gate Discipline

Every change set must pass all 5 physical verification gates:

```bash
./bin/smirk-verify
```

1. **Gate 1: HLint Static Analysis** (`.hlint.yaml`)
2. **Gate 2: Full Cabal Test Suite** (`cabal test all`)
3. **Gate 3: Tagref Cross-Reference Gate** (`bin/smirk-tagref`)
4. **Gate 4: Markdown Link Integrity Gate** (`bin/smirk-linkcheck`)
5. **Gate 5: CLOC Comment Density Gate** (`bin/smirk-cloc`)
