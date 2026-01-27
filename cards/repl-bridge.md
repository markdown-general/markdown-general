# repl-bridge ⟜ robust persistent cabal repl wrapper

---

**ghcid-style persistent wrapper** ⟜ based on how ghcid drives ghci
⟜ marker protocol: no prompt parsing, no buffering ambiguity
⟜ agent writes query to /tmp/ghci-q.txt, reads answer from /tmp/ghci-a.txt
⟜ think out loud in types, no off-by-one lags

---

## what this gives you

**type oracle** ⟜ `:t expression` → instant answer
**kind oracle** ⟜ `:k Type` → shape of type
**info oracle** ⟜ `:i Name` → instances, definition, origin
**hypothesis testing** ⟜ try before commit

## file protocol

```
write query → /tmp/ghci-q.txt
read answer ← /tmp/ghci-a.txt
```

## common queries

```haskell
:t fmap                    -- what type is this?
:t foo . bar               -- does this composition work?
:k Maybe                   -- what kind? (* -> *)
:i Functor                 -- show typeclass, instances
:browse Data.ByteString    -- what's exported?
import qualified Data.Text as T  -- bring module in scope
```

## when stuck on types

Don't guess. Ask small questions. Show the answers.

```
:t fromStrict
fromStrict :: BS.ByteString -> BL.ByteString

:t myFunction
myFunction :: BL.ByteString -> Result

-- gap visible: I have strict, need lazy
```

The types become your explanation. No excavation from bash walls.

## workflow shape

old loop:
```
edit → build (slow) → read errors → edit → build → ...
```

with repl:
```
query → think → query → sketch → edit (once, correct) → build (verify)
```

Build becomes verification, not discovery.

## tool: ghcid-style.py (recommended)

**Persistent cabal repl with ghcid-inspired marker protocol.**

Start the bridge:
```bash
cd ~/mg/artifacts/bridge
./venv/bin/python3 ghcid-style.py /path/to/haskell/project
# → mode? [w]atch files / [i]nteractive: w
```

In another terminal:
```bash
echo ":t fmap" > /tmp/ghci-q.txt
cat /tmp/ghci-a.txt
# → fmap :: Functor f => (a -> b) -> f a -> f b
```

**Why this works:**
- Injects marker functions into ghci at startup
- Sends query + marker command atomically
- Reads output until marker appears (blocking, deterministic)
- Marker is printed BY ghci itself, not parsed from outside
- No prompt matching ambiguity, no buffering races
- No off-by-one lags: each query gets its own response cleanly

## agent card pattern

```markdown
repl-ask ⟜ investigating type mismatch

query:
:t problematicFunction

result:
problematicFunction :: ByteString -> IO ()

observation:
expects strict ByteString, I'm passing lazy

next:
check if I have fromStrict available, or change upstream
```

## setup

Tools are in ~/mg/artifacts/bridge/:
- `run-ghcid-style.sh` ⟜ persistent watch mode (recommended)
- `run-ask.sh` ⟜ one-shot query fallback

```bash
cd ~/mg/artifacts/bridge
./run-ghcid-style.sh /path/to/haskell/project
```

Choose mode:
```
mode? [w]atch files / [i]nteractive: w
```

In another terminal (watch mode):
```bash
echo ":t fmap" > /tmp/ghci-q.txt
cat /tmp/ghci-a.txt
# → fmap :: Functor f => (a -> b) -> f a -> f b
```

Or use interactive mode (same process):
```
mode? [w]atch files / [i]nteractive: i
> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

## one-shot queries (fallback)

If you need a quick query without starting the bridge:

```bash
./run-ask.sh -p /path/to/project ":t myFunction"
# or pipe
echo ":i Functor" | ./run-ask.sh -p /path/to/project
```

Slower (cold start) but simpler, no process to manage.

## signal patterns

**success** ⟜ clean type signature returned
**not in scope** ⟜ module not loaded, need import
**parse error** ⟜ malformed query
**no instance** ⟜ typeclass constraint not satisfied
**kind mismatch** ⟜ type-level shape problem

## architecture: how it works (ghcid-inspired)

**Startup:**
1. Start `cabal repl`
2. Import System.IO as INTERNAL_GHCID
3. Set custom prompt to `#~GHCID-START~#`

**Per query:**
1. Send query (e.g., `:t fmap`)
2. Send marker command: `INTERNAL_GHCID.putStrLn "#~GHCID-FINISH-N~#"`
3. Read output until marker appears
4. Extract response: everything between query echo and marker
5. Clean: remove prompts, internal commands, ANSI codes

**Why this beats prompt parsing:**
- Marker is printed BY ghci, not detected from outside
- No ambiguity about prompt formats (different GHC versions vary)
- No buffering races: we read until we see the physical marker
- Incrementing counter ensures uniqueness
- Response extracted by splitting on marker boundary

**Tested:**
- Five rapid queries in sequence, all correct answers
- No off-by-one lags
- Works across GHC versions without adaptation

## remember

The repl is agent voice for type problems. Small questions, typed answers. Gaps become visible. Dead ideas die at 50ms, not 50 seconds.
