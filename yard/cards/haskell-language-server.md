# haskell-language-server ⟜ the actual type oracle

---

**hls** is not a repl wrapper. It's a **compiler service** wrapped in LSP, with a plugin system and persistent caching.

Architecture:
- **ghcide** ⟜ core IDE service (compiler, caching, type queries)
- **HLS** ⟜ LSP server that wraps ghcide + plugins
- **Plugins** ⟜ 30+ optional features (hlint, ormolu, refactoring, etc.)
- **HIE files** ⟜ persistent binary caches of typed ASTs

---

## the split: ghcide vs hls

**ghcide IS inside hls**, not separate.

- `ghcide/src/Development/IDE/*` ⟜ compiler service (typechecker, caching, file watching)
- `src/Ide/Main.hs` ⟜ HLS-specific entry point (argument parsing, plugin loading)
- `hls-plugin-api/src/Ide/Types.hs` ⟜ plugin interface (independent of LSP)

ghcide **can be used without LSP** (library, programmatic API, CLI tools).
HLS **always uses ghcide** (LSP server is the wrapper).

Flow:
```
HLS executable (exe/Main.hs)
    ↓
Main.defaultMain (src/Ide/Main.hs)
    ↓
ghcide core (Development.IDE.Main)
    ↓
Shake rules + IdeState + LSP reactor
```

## hie files: the persistence layer

**.hie files are binary artifacts** produced by GHC during compilation.

What they contain:
- Abstract Syntax Trees (ASTs) with **type annotations**
- Token mappings (identifier → location, identifier → type)
- Source locations for all symbols
- Cross-reference information for rename, refactoring

Where they live:
- Written to disk: `dist-newstyle/.hie/` or similar
- Indexed in hiedb (SQLite) for fast queries: `<project>/.hls/hiedb.sqlite`
- Loaded by HLS on startup for fast cross-file operations

How they're used in HLS:

1. **Hover/Goto** ⟜ `GetHieAst` Shake rule fetches typed AST
   ```
   User hovers: Position in file
   └─→ HLS requests: GetHieAst "file.hs"
   └─→ Shake checks: Cache? Disk? Recompile?
   └─→ Returns: HieAstResult (typed AST)
   └─→ AtPoint.atPoint extracts type at position
   └─→ Response: "fmap :: Functor f => ..."
   ```

2. **References/Rename** ⟜ hiedb indexed search
   ```
   User requests: Find all references to "foo"
   └─→ HLS queries: hiedb.sqlite (cross-file lookup)
   └─→ Returns: All positions where "foo" is used
   └─→ Can rename all occurrences atomically
   ```

3. **Startup** ⟜ HIE cache loads fast
   ```
   HLS starts → Loads existing .hie files from disk
   └─→ Builds hiedb index in memory
   └─→ Ready to serve hover/goto requests
   └─→ No need to recompile if files unchanged
   ```

**Key insight**: HIE files are **the reason HLS is fast**. Without them, every query would require recompiling the entire module. With them, type information is instant.

## the reactor pattern: single-threaded lsp

HLS uses a **reactor** to process LSP requests:

```
LSP client (editor) sends request
    ↓
Reactor receives: ReactorRequest (async)
    ↓
Request queued + response callback stored
    ↓
Reactor thread processes one at a time
    ↓
Each request: spawn work thread, runs IdeState query
    ↓
Result sent to client via callback
```

Why single-threaded?
- Prevents race conditions on IdeState
- File VFS is modified by notifications, queried by requests
- Shake rules need consistent view of file system

Trade-off: **requests are processed sequentially**, but with async callbacks so client doesn't block.

## the plugin system: extending hls

Plugins are **first-class citizens** in HLS, not afterthoughts.

Plugin structure:
```haskell
data PluginDescriptor ideState
  = pluginId          :: PluginId
  | pluginRules       :: Rules ()          -- Add Shake rules
  | pluginHandlers    :: PluginHandlers    -- LSP handlers
  | pluginCommands    :: [PluginCommand]   -- Callable functions
  | pluginConfigDesc  :: ConfigDescriptor  -- Settings schema
  | pluginPriority    :: Natural           -- Execution order
```

What plugins can do:

1. **Add Shake rules** ⟜ Custom compilation steps
   ```
   Example: Evaluate expressions in REPLs (eval plugin)
   Rule: GetEvaluatedExpression (depends on TypeCheck)
   Uses: CustomContext to hold expression results
   ```

2. **Handle LSP methods** ⟜ Respond to editor requests
   ```
   Example: HLint plugin
   Method: textDocument/codeAction
   Returns: Diagnostic fixes, refactoring suggestions
   ```

3. **Expose commands** ⟜ Editor-callable functions
   ```
   Example: Apply HLint suggestion
   Command: apply-hint
   Args: File, range, suggestion
   Executes: Modify file + return new content
   ```

4. **Modify compilation** ⟜ DynFlags adjustments
   ```
   Example: Enable RankNTypes globally, or just during parsing
   Used by: Extensions that need extra flags
   ```

Built-in 30+ plugins:
- **Formatters**: Ormolu, Fourmolu, Stylish-Haskell, Cabal
- **Linters**: HLint, Stan, Retrie
- **Refactoring**: Rename, explicit imports, code actions
- **Features**: Eval, overloaded records, GADT support, call hierarchy
- **Language**: Pragmas, completions, hover enhancements
- **Cabal**: File diagnostics, completion, hover

All executed in sequence, responses **merged** (e.g., completion items concatenated).

## beyond lsp: what hls can do outside the protocol

1. **Probe mode** ⟜ Non-LSP tools
   ```bash
   hls --probe-tools        # Check GHC version, cabal, etc.
   hls --list-plugins       # Show enabled plugins
   hls --bios               # Print cradle configuration
   hls --version            # Version info
   ```

2. **Shake rules API** ⟜ Programmatic access
   ```haskell
   import Development.IDE.Core.Actions

   getAtPoint :: IdeState → FilePath → Position → IO (Maybe TypeInfo)
   getDefinition :: IdeState → FilePath → Position → IO [Location]
   highlightAtPoint :: IdeState → FilePath → Position → IO [Range]
   refsAtPoint :: IdeState → FilePath → Position → IO [Location]
   ```
   Can use these in custom tools (not LSP clients).

3. **Direct Shake access** ⟜ Library usage
   ```haskell
   import Development.IDE

   runAction :: IdeState → Action a → IO a
   ```
   Arbitrary Shake rules can be executed programmatically.

4. **Custom command line** ⟜ Per-plugin CLI
   Each plugin can define its own arguments.
   ```bash
   hls --plugin eval --eval "length [1..100]"
   ```

**Design insight**: HLS is fundamentally a **library** (`ghcide`, `hls-plugin-api`) that happens to be packaged as an LSP server. You can use the library directly.

## the compilation pipeline: incremental shake

Every file goes through rules:

```
GetFileContents (VFS, on-disk, watch changes)
    ↓
GetParsedModule (parser, no types yet)
    ↓
GetModuleGraph (dependency resolution, .hi files)
    ↓
GhcSession (load dependencies, setup environment)
    ↓
TypeCheck (GHC compiler → TcGblEnv)
    ↓
GetHieAst (generate ASTs with types) → Write HIE to disk
    ↓ Indexed into hiedb
GetLinkable (compile to bytecode/native)
    ↓
GetDocMap (extract documentation)
```

**Incremental magic**:
- Each rule is **memoized** (results cached in IdeState)
- Each rule has **dependencies** (other rules it needs)
- When a file changes, only **affected rules** recompute
- If output is **unchanged**, downstream rules skip too (early cutoff)

Example:
```
Edit comment in function
    ↓ GetParsedModule re-runs (AST might differ)
    ↓ TypeCheck re-runs (but types same)
    ↓ GetHieAst re-runs, output unchanged
    ↓ GetLinkable skips (early cutoff)
    ↓ GetDocMap re-runs (docs might differ)
```

Result: **Fast incremental editing** even for large codebases.

## how we might use hls

Three levels of integration:

### 1. LSP client ⟜ The common way
Use HLS as your editor's language server:
```bash
hls --lsp
```
VSCode extension, Vim LSP client, Emacs lsp-mode all use this.

### 2. Library usage ⟜ Custom tools
```haskell
import Development.IDE
import Development.IDE.Span.AtPoint

-- Load a project
ideState ← initialise defaultOptions ...

-- Query types, definitions, refs
hoverInfo ← getAtPoint ideState "src/Main.hs" pos
```

Build custom tools:
- Type checkers
- Refactoring tools
- Custom linters
- Documentation extractors

### 3. Extend via plugins ⟜ New IDE features
Create a plugin (`PluginDescriptor`) that:
- Adds new Shake rules (custom analysis)
- Handles new LSP methods (new editor features)
- Exposes commands (editor-callable actions)

Compile with `-f hls-myplugin` flag, integrated into HLS.

## for our repl bridge

**HLS vs ghcid-style.py**: different approaches

**ghcid-style.py** (what we built):
- Persistent `cabal repl` session
- Queries via marker protocol
- Lightweight, fast, predictable
- Good for: rapid type queries during editing

**HLS approach**:
- Full compiler service with caching
- Plugin system for extensibility
- HIE files for persistent type info
- Good for: integrating into editor, building tools, cross-file operations

**Synthesis idea**: Could HLS replace our bridge?

Possible future work:
- Use HLS library API directly (not LSP)
- Load project via `initialise`
- Query types via `getAtPoint`
- Avoid `cabal repl` overhead, use HLS's faster compiler service
- Plugin: add new command `:query-type` callable from editor

Would need:
- HLS library in our dependencies
- Wrapper to convert file positions to HLS queries
- Integration with our marker protocol? (probably not needed)

**For now**: ghcid-style.py is simpler and faster for our use case (type queries). HLS is the right approach if we want full IDE integration (goto-def, rename, cross-file, etc.).

## remember

HLS is not magic. It's:
- GHC compiler + careful caching (Shake)
- Type information persisted in HIE files
- Plugin system that runs handlers in sequence
- LSP server as the UI layer, not the core

The **core insight**: HIE files = type annotations at the AST level = instant hover/goto without recompilation.

When you hover in your editor, HLS is:
1. Loading cached HIE file from disk
2. Walking the AST to find your position
3. Returning the type annotation already in the AST

Not recompiling, not running GHC again. Just reading.
