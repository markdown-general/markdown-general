# ghci-bridge

thin pty wrapper ⟜ agent-ghci communication

## setup

```bash
pip install -r requirements.txt
chmod +x ghci-bridge.py ghci-ask.py
```

## usage

### persistent session (recommended)

```bash
# start bridge in your project directory
./ghci-bridge.py /path/to/project

# in another terminal, or from agent:
echo ":t fmap" > /tmp/ghci-q.txt
cat /tmp/ghci-a.txt
```

### one-shot

```bash
./ghci-ask.py ":t fmap"
echo ":t fmap" | ./ghci-ask.py
./ghci-ask.py -p /path/to/project ":t myFunction"
```

## files

- `ghci-bridge.py` — persistent session, file watcher
- `ghci-ask.py` — one-shot queries  
- `repl-bridge.md` — card for agent use
- `requirements.txt` — python dependencies

## protocol

```
agent writes → /tmp/ghci-q.txt
answer lands → /tmp/ghci-a.txt
```

## why

agents + ghci = thinking out loud in types

```
:t fromStrict
fromStrict :: BS.ByteString -> BL.ByteString
```

no bash noise. no guessing. gap visible.
