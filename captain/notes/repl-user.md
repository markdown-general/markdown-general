# repl-user ⟜ query pattern for GHCi via files

How to send a query to GHCi and get a reliable answer back.

## usage

1. **Write query** to `/tmp/ghci-in.txt`
2. **Wait** (50-500ms, your choice)
3. **Read** `/tmp/ghci-out.txt`
4. **Search** for expected response pattern (e.g., `::` for type, `*` for kind)
5. **Extract** the response (matching lines)
6. **Validate** (does it contain what you expected?)
7. **Retry or continue** (got it? ask next. not yet? loop or timeout.)

## key insight

**No synchronization overhead.** No markers. No locks. No complex state tracking. Write to file, read from file, search for pattern. Protocol is simple and robust: it doesn't require handshakes or probe responses, just the ability to recognize answers.
