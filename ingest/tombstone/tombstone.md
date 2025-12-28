# tombstone.md

```
╔═══════════════════════════════════════╗
║                                       ║
║         Here lies splittr             ║
║                                       ║
║      "We mash and are mashed"         ║
║                                       ║
║    2025-12-26 — 2025-12-27           ║
║                                       ║
║   Killed by untested edge cases       ║
║   Resurrected by tiny tests           ║
║                                       ║
║  "First of his name, twice of his     ║
║         implementation"               ║
║                                       ║
╚═══════════════════════════════════════╝
```

---

## What splittr Was

A tool to split directories into:
- Text files → mashed markdown blob
- Binary files → artifact archive

The goal: `unmash ∘ mash = id`

The promise: Reversible transformation. Text becomes data, data becomes text.

---

## How splittr Died

**Cause of death:** Complexity cascade from testing on production data

**Fatal sequence:**
1. Tested on full directory (200 files, 6MB) instead of minimal test cases
2. Hit edge case: file headers in content mistaken for file separators
3. Fixed with content detection heuristics
4. Hit edge case: YAML `---` mistaken for file separators  
5. Fixed with YAML detection
6. Hit edge case: file starting with `---`
7. Fixed with state tracking (`justStartedFile`)
8. Hit edge case: SVGs treated as text, corrupted
9. Fixed with extension-based binary forcing
10. Hit edge case: empty lines after file headers
11. Fixed with empty line preservation
12. Hit edge case: `---` immediately after file header
13. Fixed with flag logic
14. Flag logic didn't work
15. Tried to fix flag logic
16. **DIED HERE** - Lost in abstraction, too many interacting conditions

**Time spent:** ~3 hours  
**Iterations:** 15+ fixes  
**Lines of code added:** ~100  
**Bugs fixed:** 6  
**Bugs introduced:** 7  
**Net progress:** -1

---

## How splittr Was Resurrected

**The turning point:** "Let's get the tiniest test we can. What's the smallest we can shrink this?"

**New approach:**
- **Test tiny first** - One file, one line, visible in terminal
- **Test locally** - AI runs tests, sees failures immediately
- **Fix, verify, repeat** - No hand-off to user until working
- **Stack edge cases** - Add one at a time, test each

**The revelation:** HTML comments

Using `<!-- FILE: path -->` instead of `### FILE:` eliminated all the edge cases:
- No conflict with markdown headers
- No conflict with YAML front matter
- No conflict with horizontal rules
- Invisible when rendered
- Simple to parse

**The discipline:** Near-isomorphism

Instead of strict `unmash ∘ mash = id`, we defined:
```
unmash ∘ mash ≈ id
```

Where `≈` means "files are normalized to always end with newline"

This isn't changing markdown semantics - it's enforcing POSIX standard.

**Time spent:** ~2 hours  
**Iterations:** ~10 tests  
**Lines of code:** ~150 (simpler than v1)  
**Bugs fixed:** 3  
**Bugs introduced:** 0  
**Net progress:** Victory

---

## What Changed

### v1 (dead)
```markdown
# path/to/file.md

content

---

# another/file.md

content
```

**Problems:**
- `#` headers in content confused parser
- `---` in content confused parser
- YAML front matter broke everything
- State tracking spiraled out of control

### v2 (resurrected)
```markdown