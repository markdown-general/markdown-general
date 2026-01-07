# card.md

**cards** ⟜ imagined recipes; routines and execution cast into markdown.
**interface** ⟜ install, test, bench, doc and do via language-specific tools
**literate** ⟜ results, costs, tests, dev, bugs, complaints; it's literally on the card.
**agnostic** ⟜ haskell, python, c, shell, english: it's all markdown meant to work together.
**processors** ⟜ haskell-api.md for Haskell; python.md for Python; language-agnostic structure

Cards are markdown files in zone/tools/ containing executable code, tests, and benchmarks. They follow coding.md structure with self-management capabilities.

## card contents

We optimize cards for your critical paths: reading, fixing, extending. Follow this order:

**Deck** ⟜ visual progress indicator for section scanning
**Statement** ⟜ what it does, why, who it's for
**Main examples** ⟜ 1-3 quick, concrete uses (the common case)
**Laws** ⟜ useful proof statements showing round-trip or invariants
**API** ⟜ inputs, outputs, configuration, contracts
**Installation** ⟜ where it lives, how to invoke, language-specific wrapper details
**Tips** ⟜ practical gotchas, edge cases, what to watch for (if needed)
**Status** ⟜ test results, known issues, performance notes (short, actionable)
**Code** ⟜ implementation with inline notes, helper functions, test assertions
**Examples** ⟜ workflow examples, specialist use cases (if needed)
**Tests** ⟜ test suite appropriate to the task
**Relations** ⟜ dependencies, related cards (optional)

All information needed to understand and fix common card mistakes lives in the card itself. No file-hopping.

All sections are optional; smallness of task means the card could be a few decks in size.

## deployment by language

Each language has its own installation and testing approach:

**Haskell cards** ⟜ see haskell-api.md
```bash
haskell-api toolname.md --install   # extract, compile, install to artifacts/bin/
haskell-api toolname.md --test      # run tests, update status
```

**Python cards** ⟜ see python.md
```bash
# Extract and use via sed
toolname() { sed -n '/^```python$/,/^```$/p' ~/markdown-general/tools/toolname.md | sed '1d;$d' | python3 - "$@"; }
# Or create wrapper script in artifacts/bin/
```

After installation, all executables are cast:

```bash
toolname [args]                     # installed executable does the work
```

## language agnostic

Cards can be implemented in any language:

**Haskell** ⟜ optparse-applicative for CLI, typed-process for wrapping, perf for benchmarks
**Python** ⟜ existing tools, simpler scripts
**Shell** ⟜ simple glue logic
**Mixed** ⟜ one language wrapping another's executable

Language detected from fenced code block info string: ```haskell, ```python, ```cpp

## language-specific processing

Each card type uses a language-specific processor:

**Haskell cards** ⟜ haskell-api.md
- Extraction ⟜ parse Haskell fenced blocks via markdown-unlit
- Compilation ⟜ orchestrate cabal build
- Testing ⟜ run doctests + executable tests, update Status section
- Benchmarking ⟜ measure performance, update Status section
- Installation ⟜ place executable in artifacts/bin/

**Python cards** ⟜ python.md
- Extraction ⟜ sed extracts python block from markdown
- No compilation ⟜ direct execution via python3
- Testing ⟜ run inline tests, update Status section
- Installation ⟜ create wrapper script in artifacts/bin/

Cards are data - specifications that language-specific tools know how to execute.

## status section

Cards maintain their own status, modified by language-specific tool operations:

```markdown
## Status

**Tests:** ✓ passed | ✗ failed: error message
**Benchmark:** 2.3ms (median over 100 runs)
**Last updated:** 2025-12-31
```

## subcommands

Cards can declare multiple functions:

```markdown
## Subcommands

**flatten** ⟜ concatenate files with delimiters
**unflatten** ⟜ extract files from concatenated output
```

Executable handles routing:

```bash
flatten-md flatten files.txt output.md
flatten-md unflatten merged.md
```

## functionalization

defunctionalization and refunctionalization is hard for the eyeballs to remember and type so we call it all casting.

**Casting** ⟜
**Defunctionalization** ⟜ conversation → markdown (capture essence)
**Refunctionalization** ⟜ markdown → executable (release essence)

Cards are defunctionalized tools that refunctionalize via language-specific processors.

The executable in artifacts/bin/ is ready to run.

## card lifecycle

**Write** ⟜ create literate tool in zone/tools/toolname.md
**Install** ⟜ language-specific tool extracts, builds, installs to artifacts/bin/
**Test** ⟜ language-specific tool verifies correctness, updates status
**Benchmark** ⟜ language-specific tool measures speed, updates status
**Use** ⟜ toolname [args] does the work
**Evolve** ⟜ modify card, --uninstall, --install again

## directory conventions

**zone/tools/** ⟜ where cards live
**work/** ⟜ where card.md lives (this file)
**artifacts/bin/** ⟜ where executables go (must be on PATH)

## verbose mode

When language-specific tools are run with --verbose:

**Prints docs** ⟜ card documentation at start
**Shows deck** ⟜ progress indicators during operations
**Diagnostic output** ⟜ compilation steps, test details, benchmark iterations

## bootstrap requirement

Cards require language-specific processors:

**Trade-off:**
- **Lost** ⟜ individual card self-sufficiency
- **Gained** ⟜ zero duplication, consistent API, focused specifications

Language-specific tools (haskell-api.md, python.md) are foundational bootstraps.
All other cards are data they process.

## relations

**coding.md** ⟜ defines code structure, cards are specific instance
**defunctionalization.md** ⟜ explains essence capture, cards materialize it
**haskell-api.md** ⟜ processor for Haskell cards
**python.md** ⟜ processor for Python cards
**sequential.md** ⟜ cards installed one at a time via language-specific tools

Cards make tools mashable - they can be installed, tested, benchmarked, and evolved like any other content.

## examples

**Haskell card (pure implementation):**
```bash
# flatten-md.md implements logic directly in Haskell
haskell-api flatten-md.md --install
flatten-md flatten files.txt output.md
flatten-md unflatten merged.md
```

**Haskell card (wrapper pattern):**
```bash
# pdf-to-md.md wraps marker-pdf executable via typed-process
haskell-api pdf-to-md.md --install    # ensures marker-pdf available
pdf-to-md input.pdf                   # wrapper validates args, calls marker-pdf
```

**Python card:**
```bash
# cache.md implements simple file operations in Python
cache flatten                         # extract from tools/cache.md, run
cache split cache-file.md             # direct execution via python3
```

**Testing and benchmarking (Haskell):**
```bash
haskell-api flatten-md.md --test       # updates Status section with results
haskell-api flatten-md.md --benchmark  # updates Status section with timings
```
