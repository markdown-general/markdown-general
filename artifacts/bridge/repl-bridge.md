# repl-bridge ⟜ agent access to ghci type oracle

---

**repl-bridge** ⟜ thin pty wrapper for ghci
⟜ agent writes query, reads typed answer
⟜ think out loud in types, not bash noise

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

## modes

**ghci-bridge.py** ⟜ persistent session, watches files
- session stays warm (loaded modules persist)
- write to /tmp/ghci-q.txt, read from /tmp/ghci-a.txt
- human runs bridge, agent queries

**ghci-ask.py** ⟜ one-shot, cold start each time
- simpler, slower (cabal repl startup each query)
- good for infrequent queries

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

## installation

```bash
pip install pexpect
chmod +x ghci-bridge.py ghci-ask.py
```

## starting the bridge

```bash
# from project directory
./ghci-bridge.py .

# or specify path
./ghci-bridge.py /path/to/haskell/project
```

## signal patterns

**success** ⟜ clean type signature returned
**not in scope** ⟜ module not loaded, need import
**parse error** ⟜ malformed query
**no instance** ⟜ typeclass constraint not satisfied
**kind mismatch** ⟜ type-level shape problem

## remember

The repl is agent voice for type problems. Small questions, typed answers. Gaps become visible. Dead ideas die at 50ms, not 50 seconds.
