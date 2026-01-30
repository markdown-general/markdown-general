https://github.com/archive567/words

---

## Session: Tight Review ⟜ 2026-01-25

**Source:** ~/repos/old-words (cloned, now deleted)
**Result:** ~/repos/words (new library)

**Session mode:** yin-narrow ⟜ disciplined, focused execution

**Objectives:**
- [x] Create words library skeleton
- [x] Resolve dependency conflicts (allow-newer: *:base, *:containers, *:template-haskell)
- [x] Pin perf >= 0.14 to maintain correct version
- [x] Implement wordCount with doctest
- [x] Verify cabal-docspec passes

**Status:** ✓ Complete

**Cards created:**
- 001-words-init.md (spun successfully)

**Key findings:**
- Dependency resolution required *:base, *:containers, *:template-haskell in cabal.project
- perf >= 0.14 pin necessary; without it solver backtracks to perf-0.7.0 (incompatible)
- Doctest requires explicit BSC.pack conversion (OverloadedStrings doesn't apply to doctest context)
- Library builds cleanly with all doctests passing

**Artifacts:**
- ~/repos/words/words.cabal (library stanza with streaming deps)
- ~/repos/words/src/Words.hs:
  - wordCount: Fold for word frequency counting
  - wordStream: Streaming bytestring → normalized text words
  - foldWords: Fold streaming source into Map Text Int
  - fromFile: FilePath → IO (Map Text Int)
  - fromUrl: String (URL) → IO (Map Text Int)
- ~/repos/words/readme.md (minimal usage guide)
- ~/repos/words/other/fake.txt (test data)
- ~/repos/words/.gitignore and cabal.project (allow-newer rules)

**Commit:** 7899d12 - Initialize words library with streaming word count functions

---

## Session 2: PMLB Resurrection ⟜ 2026-01-25

**Source:** https://github.com/archive567/pmlb (archived, cloned then deleted)
**Result:** ~/repos/pmlb-new (new library) ✓

**Card:** 002-pmlb-init.md (spun successfully)

**Status:** ✓ Complete

**Result:** ~/repos/pmlb-new (library only)

**Exports:** SetType (Classification | Regression), PMLBConfig, defaultPMLBConfig

**Key deps:** cassava (CSV), streaming, managed, numhask-space, lens, text, bytestring

**Commits:**
- 8bff64e - Initialize PMLB library with Penn ML Benchmarks dataset utilities
- 1227897 - Add dataframe and vector dependencies with CSV reading functions
- 6e35740 - Use DataFrame readCsv for CSV file reading
- c94baa6 - Add test-csv executable to verify DataFrame CSV loading ✓

**Final API:**
- `readCsvFile :: FilePath -> IO DataFrame` - Load local CSV files
- `datasetUrl :: PMLBConfig -> String -> String` - Construct Penn ML Benchmarks URLs
- `fetchDataset :: PMLBConfig -> String -> IO DataFrame` - Download & load remote datasets

**Final Commits:**
- 70572bd - Add fetchDataset function for downloading Penn ML Benchmarks ✓
- 588d22d - Add fetch-dataset executable for remote loading ✓

**Verified:**
- ✓ Local CSV loading: test.csv with typed columns (Text, Int, Text)
- ✓ DataFrame type inference operational
- ✓ Remote dataset infrastructure complete (download → decompress → load)
- ✓ fetch-dataset CLI executable ready for production
- ✓ Full end-to-end pipeline operational

**Status:** ✓ Complete - PMLB library resurrected and operational
**Old repo:** ~/repos/pmlb deleted
