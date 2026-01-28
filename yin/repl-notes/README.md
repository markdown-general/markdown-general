# repl-notes/ ⟜ research and design exploration

Design documents for planned features. Not yet implemented.

## Contents

**process-design.md** ⟜ file-based protocol design rationale
- Separation of concerns: spawn ⟜ query ⟜ observe
- No synchronization complexity needed
- Hyperfunction insight: all variables are local
- Composability and scaling patterns

**process-createProcess.md** ⟜ System.Process API study
- Explored GHCi via repl to understand createProcess signature
- Documents ProcessHandle, Handle, CreateProcess record structure
- Research material for implementing repl-startup in Haskell

**async-key.md** ⟜ async library patterns for repl
- Concrete rework of repl architecture using async/race/waitCatch
- Timeout handling via `race` primitive
- Single input watcher to eliminate contention
- Planned improvements over current file-based polling

**codata.md** ⟜ theoretical foundation
- Hyperfunction research and references
- Categorical properties (sheaves, coinduction)
- Grounds the design in formal theory
- Theoretical underpinning for process-design.md

## Status

These are design documents, not yet integrated into working code. They represent the path forward for:
1. Wrapping the file-based protocol in Haskell
2. Adding timeout and concurrency safety
3. Integrating async primitives for production use
