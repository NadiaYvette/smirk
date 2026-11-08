# SMIRK_DESIGN.md — Architectural Decisions & PCRE Engine Invariants

Canonical design monograph for `smirk`.

---

## 1. Design Vision & Motivation

`smirk` (;-P / ;-) provides a pure, native Haskell PCRE regular expression engine with ironclad defenses against algorithmic complexity attacks, first-class effect system integrations, and container polymorphism.

Historically, Haskell regular expression solutions presented several major drawbacks:

1. **Unsafe C Bindings (e.g. `pcre-heavy`, `regex-pcre`):**
   - Direct foreign function calls to libpcre / libpcre2 introduce C toolchain dependencies, memory safety vulnerabilities, and build complexity on cross-compilation targets.
2. **Algorithmic ReDoS Vulnerabilities (Backtracking Hell):**
   - Naive backtracking engines exhibit exponential $\mathcal{O}(2^N)$ time complexity on ambiguous grammars (such as `(a+)+$`), allowing unauthenticated attackers to lock server CPU cores indefinitely.
3. **Container Inflexibility:**
   - Libraries frequently mandate converting inputs to `ByteString` or `Text`, inducing extraneous heap allocations when processing lazy `String`, `Vector Char`, or finger-tree `Seq Char`.
4. **Effect System Coupling:**
   - Extensible effect users were forced to choose between heavy wrapper packages or dragging in monolithic dependencies.

`smirk` resolves these challenges through:
- A pure Haskell Pike-style virtual machine compiling PCRE patterns to bytecode.
- Step-bounded execution (`Fuel`) guaranteeing that matching terminates within a defined step budget.
- Pausable and resumable execution continuations (`MatchContinuation`).
- A sequence polymorphism typeclass (`RegexTarget`) operating directly on standard Haskell sequences.
- A split-package architecture providing dedicated, zero-overhead effect wrappers for `effectful` and `polysemy`.

---

## 2. Canonical Architectural Decisions

### D1: Pike-Style Virtual Machine with Explicit Backtracking Stack
- **Decision:** Regex patterns compile into a linear bytecode vector ('Program' of 'Opcode's: `OpChar`, `OpClass`, `OpSplit`, `OpJump`, `OpSaveStart`, `OpSaveEnd`, `OpAssertLookahead`, `OpAssertLookbehind`, `OpMatchBackRef`, `OpMatch`).
- **Rationale:** Avoids recursive C-style runtime call stacks that can overflow on deep inputs. Thread alternatives are maintained on an explicit heap stack (`[ThreadState]`), enabling clean stepping and continuation capture.
- **Reference:** Note [Pike VM State Machine and Backtracking Stack] `[ref:note-pike-vm-state-machine]`.

### D2: Fuel-Bounded Step Budgeting for ReDoS Defense
- **Decision:** Every instruction step in 'stepVM' consumes 1 unit of 'Fuel' (`StepBudget !Int` or `Unlimited`). If the budget is exhausted before a match or total failure is determined, execution halts safely and yields `OutOfFuel (MatchContinuation s)`.
- **Rationale:** Completely neutralizes Regular Expression Denial of Service (ReDoS) attacks by giving host applications deterministic control over execution latency.
- **Reference:** Note [Fuel-Bounded Step Budget and ReDoS Defense] `[ref:note-fuel-step-budget]`.

### D3: First-Class Pausable and Resumable Continuations
- **Decision:** When 'OutOfFuel' is returned, the 'MatchContinuation' bundles the target sequence, program counter, thread state, backtrack stack, and step count. The operation can be resumed at any time via 'resumeMatch' with a fresh fuel allocation.
- **Rationale:** Enables time-sliced, interactive, or background regex processing across long-running streams without re-executing previous steps from scratch.

### D4: Sequence Polymorphism via `RegexTarget`
- **Decision:** Instead of fixing input types to strict `Text` or `ByteString`, all VM execution primitives operate on the 'RegexTarget' typeclass, supporting `Text`, `ByteString`, `[Char]`, `Seq Char`, and `Vector Char`.
- **Rationale:** Eliminates redundant data copying and heap allocations at application boundaries.

### D5: Decoupled Multi-Package Architecture
- **Decision:** The codebase is partitioned into three independent packages:
  - `smirk`: Pure core engine, VM, compiler, and QuasiQuoters.
  - `smirk-effectful`: Dedicated bindings for the `effectful` algebraic effect library.
  - `smirk-polysemy`: Dedicated bindings for the `polysemy` extensible effect library.
- **Rationale:** Users who only require `effectful` never transitively depend on `polysemy`, and users desiring pure regexes never depend on any effect library.

### D6: Compile-Time QuasiQuoters (`[re|...|]`, `[ed|...|]`, `[glob|...|]`)
- **Decision:** Exposes QuasiQuoters validating PCRE syntax, stream-editor replacement patterns, and glob wildcards at compile time using Template Haskell.
- **Rationale:** Detects regex syntax errors during compilation rather than failing at runtime, and builds pre-validated AST structures.

### D7: Native POSIX and Git-Style Globbing Engine
- **Decision:** Implements filesystem globbing (`compileGlob`, `[glob|...|]`) and unconstrained wildcards (`compileWildcard`) by compiling glob tokens into equivalent Smirk PCRE AST representations.
- **Rationale:** Provides zero-dependency, high-performance globbing that shares the same step-bounded VM, ReDoS defense, and sequence polymorphism as standard regexes.

### D8: Zero-Warning Compiler Discipline
- **Decision:** Every package in Smirk compiles under `-Wall -Wcompat -Wredundant-constraints` with 0 HLint hints and 0 compiler warnings.
- **Rationale:** Preserves strict code hygiene and eliminates latent runtime hazards.

### D9: Forge-Tiered CI Architecture
- **Decision:** CI pipelines partition into lightweight Tier 1 static gates (Tagref, link integrity, HLint, CLOC) and Tier 2 full multi-compiler test matrices.
- **Reference:** Note [Forge-Tiered CI Resource Allocation and Static Gate Partitioning] `[ref:note-forge-tiered-ci]`.

### D10: Non-Destructive Scaffolding and Quality Gate Governance
- **Decision:** Continuous tooling, git hooks, and verification harnesses are managed via `gachi retrofit` without overwriting existing application code.
- **Rationale:** Guarantees progressive quality improvement across the multi-package workspace.

---

## 3. Grounded Notes Architecture

- **Note [Pike VM State Machine and Backtracking Stack]** (`Smirk.Engine.VM`):  
  `[ref:note-pike-vm-state-machine]`  
  Details the bytecode instruction set, explicit backtracking stack, and thread branching semantics.
- **Note [Fuel-Bounded Step Budget and ReDoS Defense]** (`Smirk.Engine.VM`):  
  `[ref:note-fuel-step-budget]`  
  Specifies the fuel step consumption model, budget exhaustion handling, and ReDoS immunity.
- **Note [Forge-Tiered CI Resource Allocation and Static Gate Partitioning]** (`docs/notes/DOC_STRATEGY.md`):  
  `[ref:note-forge-tiered-ci]`  
  Details CI execution tiering across GitHub Actions, Framagit, and Radicle.
- **Note [Pike VM Flat Capture Register Allocation]** (`Smirk.Core.Types`):  
  `[ref:note-pike-vm-flat-captures]`  
  Replaces pointer-chasing maps with flat unboxed PrimArray registers.
- **Note [Polytropos Record Representations for Named Captures]** (`Smirk.Core.Types`):  
  `[ref:note-polytropos-capture-reps]`  
  Projects named capture groups into row-polymorphic Polytropos Rep structures.
- **Note [Static AST Nullability Analysis]** (`Smirk.Core.Evacuable`):  
  `[ref:note-static-ast-nullability]`  
  Decides whether epsilon is in L(r) statically at compile time without VM allocation.
- **Note [Non-Empty Target Polymorphism]** (`Smirk.Core.Evacuable`):  
  `[ref:note-non-empty-target-polymorphism]`  
  Witnesses matchLength >= 1 at the type level across String, Text, ByteString, Seq, and Vector.
- **Note [Linear Continuation Discipline]** (`Smirk.Core.Cursor`):  
  `[ref:note-linear-continuation-discipline]`  
  Guarantees single-shot resumption under -XLinearTypes without continuation duplication.
- **Note [Graded Lookbehind Cursor]** (`Smirk.Core.Cursor`):  
  `[ref:note-graded-lookbehind-cursor]`  
  Tracks type-level lookbehind horizon bounds and safely evacuates dead prefix data.
