# server-cleanup ⟜ Always check for old servers before testing

**Lesson learned:** When testing prettychart-serve or any server on a fixed port, ALWAYS check for zombie processes first.

Old servers will interfere, cause confusing symptoms (page flickering, mixed responses, state conflicts), and waste hours of debugging.

## Before starting any server test:

```bash
lsof -i :9160
```

Kill any old processes:

```bash
kill <PID> <PID>
```

Or nuclear option:

```bash
pkill -f "prettychart-serve"
```

Then start fresh.

## Why this matters:

- Multiple servers on same port cause unpredictable behavior
- Browser connects to whichever one responds first
- Makes it impossible to trace state changes
- Debug messages become noise from competing processes

**Make this a habit before every test run.**
