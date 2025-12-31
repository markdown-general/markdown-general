# card.md

## card

**Card** ⟜ literate tool specification processed by card-api.md

Cards are markdown files in content/tools/ containing executable code, tests, and benchmarks. They follow coding.md structure with self-management capabilities.

## structure

Cards extend coding.md with status tracking:

**Narrative** ⟜ what it does, why, how
**Code** ⟜ executable implementation in fenced blocks (any language)
**Subcommands** ⟜ optional multiple modes (flatten/unflatten, concat/split)
**Run** ⟜ how to install and run
**API** ⟜ inputs, outputs and configuration
**Examples** ⟜ usage patterns
**Tests** ⟜ expected behavior
**Status** ⟜ test results and benchmark timings (updated by card-api.md)

## universal API

All cards support the same operations via card-api.md:

```bash
card-api toolname.md --install      # extract, compile, install to artifacts/bin/
card-api toolname.md --test         # run tests, update status
card-api toolname.md --benchmark    # measure performance, update status
card-api toolname.md --docs         # display documentation
card-api toolname.md --verbose      # verbose mode for any operation
```

After installation, the executable is refunctionalized:

```bash
toolname [args]                     # installed executable does the work
```

## language agnostic

Cards can be implemented in any language:

**Haskell** ⟜ optparse-applicative for CLI, typed-process for wrapping, perf for benchmarks
**Python** ⟜ existing tools, simpler scripts
**C++** ⟜ performance-critical implementations
**Shell** ⟜ simple glue logic
**Mixed** ⟜ one language wrapping another's executable

Language detected from fenced code block info string: ```haskell, ```python, ```cpp

## card-api processing

**card-api.md** ⟜ universal processor for all cards

Handles:
- **Extraction** ⟜ parse fenced blocks by language
- **Compilation** ⟜ orchestrate stack/cabal/gcc/etc based on language
- **Testing** ⟜ run tests, parse results, update Status section
- **Benchmarking** ⟜ measure performance, update Status section
- **Installation** ⟜ place executable in artifacts/bin/

Cards become pure data - specifications that card-api knows how to execute.

## status section

Cards maintain their own status, modified by card-api operations:

```markdown
## Status

**Tests:** ✓ passed | ✗ failed: error message
**Benchmark:** 2.3ms (median over 100 runs)
**Last updated:** 2025-12-31
```

Status lives in the card itself - the markdown file is self-documenting.

## subcommands

Cards can declare multiple modes:

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

## refunctionalization

**Defunctionalization** ⟜ conversation → markdown (capture essence)
**Refunctionalization** ⟜ markdown → executable (release essence)

Cards are defunctionalized tools that refunctionalize via card-api.md --install.

The executable in artifacts/bin/ is the materialized function, ready to run.

## card lifecycle

**Write** ⟜ create literate tool in content/tools/toolname.md
**Install** ⟜ card-api toolname.md --install creates artifacts/bin/toolname
**Test** ⟜ card-api toolname.md --test verifies correctness, updates status
**Benchmark** ⟜ card-api toolname.md --benchmark measures speed, updates status
**Use** ⟜ toolname [args] does the work
**Evolve** ⟜ modify card, --install again

## directory conventions

**content/tools/** ⟜ where cards live
**content/base/** ⟜ where card.md lives (this file)
**artifacts/bin/** ⟜ where executables go (must be on PATH)

## verbose mode

When card-api.md --verbose is present:

**Prints docs** ⟜ card documentation at start
**Shows deck** ⟜ progress indicators during operations
**Diagnostic output** ⟜ compilation steps, test details, benchmark iterations

## dependency

Cards cannot operate standalone - they require card-api.md.

**Trade-off:**
- **Lost** ⟜ individual card self-sufficiency
- **Gained** ⟜ zero duplication, consistent API, focused specifications

card-api.md is the one thing you bootstrap. All other cards are data it processes.

## relation to sisyphus concepts

**coding.md** ⟜ defines code structure, cards are specific instance
**defunctionalization.md** ⟜ explains essence capture, cards materialize it
**cache.md** ⟜ cards are executable cache entries
**sequential.md** ⟜ cards installed one at a time via card-api.md

Cards make tools mashable - they can be installed, tested, benchmarked, and evolved like any other content.

## examples

**Pure implementation:**
```bash
# flatten-md.md implements logic directly in Haskell
card-api flatten-md.md --install
flatten-md flatten files.txt output.md
flatten-md unflatten merged.md
```

**Wrapper pattern:**
```bash
# pdf-to-md.md wraps marker-pdf executable via typed-process
card-api pdf-to-md.md --install    # ensures marker-pdf available
pdf-to-md input.pdf                # wrapper validates args, calls marker-pdf
```

**Testing and benchmarking:**
```bash
card-api flatten-md.md --test       # updates Status section with results
card-api flatten-md.md --benchmark  # updates Status section with timings
```
