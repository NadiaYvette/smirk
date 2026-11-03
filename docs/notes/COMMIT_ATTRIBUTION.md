# Commit Attribution & Trailer Standards

**Project:** smirk  
**Status:** POLICY · v1.0 · 2026-10-24  
**Index:** [`../INDEX.md`](../INDEX.md)  
**Parent Strategy:** [`DOC_STRATEGY.md`](DOC_STRATEGY.md)  

---

## 1. Linux Kernel RFC 2822 Trailer Format

Every git commit in this repository must carry clear, human-auditable attribution trailers complying with RFC 2822 email header format and Linux kernel commit guidelines.

### AI Assistant Contributions

When code, documentation, or configuration is generated or refined with the assistance of an AI coding agent (e.g. Antigravity, Claude Code, Cursor, Aider):

```
Assisted-by: Antigravity (Google DeepMind / Gemini 3.8 Flash)
```

Optionally accompanied by an orchestration engine trailer:

```
Orchestrated-by: yamaarashi-exec (Milestone 4.2)
```

---

## 2. Developer Certificate of Origin (DCO) Boundaries

- **`Co-authored-by:` and `Signed-off-by:` are strictly reserved for human beings.**
- Under the Linux Foundation Developer Certificate of Origin (DCO), only legal persons can certify code authorship and copyright licensing.
- AI assistants must **never** be credited via `Co-authored-by:` or `Signed-off-by:`.
- This policy is mechanically enforced by the repository's `.githooks/commit-msg` hook.
