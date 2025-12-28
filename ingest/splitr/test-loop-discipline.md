## Test Loop Discipline

**When:** Building tools that process blobs  
**Why:** AI can't debug what it can't see  
**How:** Test what can be tested in AI first, hand off to humans only when you have to.

---

## The Problem

You're building a tool that processes a blob. You test it on the real blob. It fails in a weird way. Or you pass it back and get a bug report when you expected a tick. Try to fix it. It fails in a different weird way. You're now in a maze of twisty fixes, all alike.

### solution

- **shrink** the test after the first fail.
- **reviewing the path** to here is often cheaper
- **humanless loops** are less buggy and faster.

get the operator out of the loop.

---

## The Full Discipline

- can skip rules on mash

### 1. Create Minimal Test Data

**Before touching the real blob:**

```bash
# Create a mini blob
mkdir test-data
echo "# file1.md

content" > test-data/file1.md

echo "# file2.md

---
title: test
---

more content" > test-data/file2.md
```

### 2. Test Locally Within AI

**Run the tool on tiny data:**

```bash
./your-tool test-data output
diff -r test-data output
```

**Fix until diff is empty.**

**Why:** You get immediate feedback. No context switching. No explanations needed.

### 3. Only Then Ask Operators

**Once local tests pass:**
- "Here's my tool [paste code]"
- "Here's my test data [paste 10 lines]"  
- "It works on test data but fails on real blob with: [error]"

**Why:** Operators are slow and tend to slothful forgetfulness. can actually help, yes.

---

## Real Example: dir-mash/dir-unmash

**What we did:** Tested on full mash blob (200 files, 6MB)  
**What happened:** 
- File headers in content mistaken for file separators
- YAML front matter `---` mistaken for file separators
- SVGs treated as text
- 15 iterations of fixes, each introducing new abstractions
- Got stuck in "twisty fixes, all alike"

**What we should have done:**

```bash
# Test case 1: Simple files
mkdir test1
echo "content" > test1/file1.md
echo "more" > test1/file2.md

# Test case 2: File with --- in content
mkdir test2
echo "---
title: test
---
content" > test2/file.md

# Test case 3: Mixed text and binary
mkdir test3
echo "text" > test3/file.md
printf '\x89PNG' > test3/image.png

# Run tests
./dir-mash.md test1 test1.md
./dir-unmash.md test1.md test1-out
diff -r test1 test1-out || echo "FAIL: test1"

./dir-mash.md test2 test2.md  
./dir-unmash.md test2.md test2-out
diff -r test2 test2-out || echo "FAIL: test2"

# etc.
```

**Result:** Would have caught issues in 3 iterations instead of 15.

---

## The Rule

**If you can't test it on 10 lines, you can't trust it on 10,000 lines.**

Don't involve the operator until you can show it the complete test case that demonstrates the problem.

---

## When to Break This Rule

On a mash. When you have the feels.

---

## Test Data Characteristics

Good test data is:
- **Tiny:** 2-5 files max
- **Specific:** Tests exactly one edge case
- **Visible:** Fits in a terminal window
- **Explicit:** No "similar to production" - use actual production patterns
- **Dumb:** Simple as possible - no nested complexity

Bad test data is:
- "A subset of production"
- "Just the first 100 files"
- "Here's a zip file"

---

## The Cycle

```
1. Write minimal test case
2. Run tool locally
3. Fix
4. Repeat until diff clean
5. Then run on real data
```

---

## Signs You're Doing It Wrong

- "It works on most files but fails on..."
- "There's an edge case where..."
- "Let me try one more fix..."
- "The error message is confusing because..."
- "It's doing something weird with..."

**Stop. Make a test case. Test locally.**

---

## Blob Processing Checklist

Before asking Operator for help:

- [ ] Created minimal test data
- [ ] Test data demonstrates the specific issue
- [ ] Tool runs successfully on test data
- [ ] diff shows exact difference
- [ ] Can paste complete test case in one message

---

## Summary

Test loops without Operators:
1. Are faster
2. Are clearer
3. Build understanding
4. Catch issues early
5. Make Operator help actually helpful and not for mashing.

The Operator is powerful, but it can't debug what it can't understand. Give it something it can understand.

---

## Appendix: Quick Test Data Generators

```bash
# Empty file
touch test.md

# File with separator
cat > test.md << 'EOF'
---
title: test
---
content
EOF

# File starting with separator  
cat > test.md << 'EOF'
---
content
EOF

# Binary file
printf '\x89PNG\r\n\x1a\n' > test.png

# Empty directory
mkdir empty-dir

# Multiple files
for i in {1..3}; do echo "file $i" > "test$i.md"; done
```

Keep these handy. Use them first.


---

## What splittr Was

A tool to split mash/ into:
- Text files → mashed markdown ball
- Binary files → artifact archive

The goal: `unmash(mash(x)) == x`

The promise: Reversible transformation. Text becomes data, data becomes text.

---

## How splittr Died

**Cause of death:** Complexity cascade from testing on production data

**Fatal sequence:**
1. Tested on full mash/ (200 files, 6MB) instead of minimal test cases
2. Hit edge case: file headers in content mistaken for file separators
3. Fixed with content detection heuristics
4. Hit edge case: YAML `---` mistaken for file separators  
5. Fixed with YAML detection
6. Hit edge case: file starting with `---`
7. Fixed with state tracking (`justStartedFile`)
8. Hit edge case: SVGs treated as text, corrupted
9. Fixed with extension-based binary forcing
10. Hit edge case: empty lines after file headers
11. Fixed with empty line preservation
12. Hit edge case: `---` immediately after file header
13. Fixed with flag logic
14. Flag logic didn't work
15. Tried to fix flag logic
16. **DIED HERE** - Lost in abstraction, too many interacting conditions

**Time spent:** ~3 hours  
**Iterations:** 15+ fixes  
**Lines of code added:** ~100  
**Bugs fixed:** 6  
**Bugs introduced:** 7  
**Net progress:** -1

---

## What We Should Have Done

```bash
# Test case 1 (3 lines)
mkdir test1
echo "content" > test1/file.md
./dir-mash.md test1 out.md && ./dir-unmash.md out.md test1-restored
diff -r test1 test1-restored  # Pass or fail in 1 second

# Test case 2: File starting with ---
echo "---
content" > test2/file.md
# Test...

# etc.
```

**Time it would have taken:** 3 minutes  
**Bugs introduced:** 0  
**Clarity:** High  
**Confidence:** High

---

## Lessons

1. **Test on tiny data first**  
   10 lines you can see > 10,000 lines you can't

2. **One edge case at a time**  
   Fix, test, commit. Don't pile fixes.

3. **AI can't debug what it can't see**  
   Context limits mean it's guessing. Give it complete test cases.

4. **Local test loop > AI assistance**  
   Faster feedback, clearer understanding, better fixes.

5. **Provisos leak**  
   "Except when..." × 5 = unmaintainable mess.

---

## What splittr Taught Us

The mash/unmash pattern is sound. The implementation got lost in conditionals.

The idea: Split text from binary, preserve both, recombine perfectly.

The reality: Edge cases compound. State tracking spirals. Abstractions leak.

The fix: Not another conditional. **Start over with tests.**

---
