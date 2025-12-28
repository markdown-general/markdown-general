# What is a Blob?

A blob is a ball of text.

---

## Definition

**blob** (n): A collection of text files mashed together into a single file, preserving structure through delimiters.

**mashing** (v): The act of combining multiple files into a blob.

**unmashing** (v): The act of extracting files from a blob back to their original structure.

**The test:** `unmash(mash(x)) == x`

---

## Why Blobs Exist

### Problem: Too Many Files

You have 200 markdown files spread across nested directories. You want to:
- Hand them to an AI (context limits)
- Process them as a unit (scripts)
- Version them together (git)
- Move them around (transfers)
- Transform them in bulk (edits)

200 separate files are unwieldy.

### Solution: Mash Them

One file. All the content. Delimiters for structure.

```
# file1.md

content

---

# dir/file2.md

more content
```

Now you have one thing instead of many things. The AI sees all of it. Your script processes all of it. git handles one diff.

---

## Properties of Good Blobs

### 1. Readable

You can open the blob and read it. File boundaries are visible. Structure is clear.

### 2. Reversible  

`unmash(mash(x)) == x` - Perfect round-trip. No data loss. No corruption.

### 3. Transformable

You can:
- Search/replace across all files
- Apply consistent edits
- Restructure in bulk
- Feed to AI for analysis

### 4. Simple

Minimal format. Obvious delimiters. No magic. Easy to parse, easy to generate.

---

## What Blobs Are Not

**Blobs are not:**
- Archives (tar, zip) - those are binary
- Databases - those have schemas
- File systems - those have permissions
- Version control - those track changes

**Blobs are:** Just text with structure markers.

---

## The Mash/Unmash Pattern

```
Files → [mash] → Blob → [process] → Blob → [unmash] → Files
```

**Example:**
```bash
# Start: 100 markdown files
ls *.md | wc -l
# 100

# Mash
dir-mash.md . all.md

# Process (AI, scripts, whatever)
cat all.md | AI-process > all-processed.md

# Unmash  
dir-unmash.md all-processed.md ./output

# End: 100 processed files
ls output/*.md | wc -l
# 100
```

---

## Blob Formats

### Simple (V1)

```
# path/to/file.md

[content]

---

# another/file.md

[content]
```

**Pros:** Dead simple. Easy to parse. Human readable.  
**Cons:** Can't handle files with `# ` headers in content. No metadata.

### Complex (V2 - splittr's grave)

```
# metadata.json
{...}

---

### FILE: path/to/file.md

[content]

---

### FILE: another/file.md

[content]
```

Plus: artifact archives, binary detection, YAML handling, state tracking...

**Pros:** More capable. Handles edge cases.  
**Cons:** Too many edge cases. Complexity spiral. Failed at round-trip.

### Lesson

Simple blobs work. Complex blobs die in edge cases.

---

## When to Use Blobs

**Good uses:**
- Handing many files to AI for processing
- Bulk transformations (search/replace, format changes)
- Temporary aggregation for analysis
- Intermediate step in pipelines

**Bad uses:**
- Long-term storage (use proper files)
- Binary data (use archives)
- Version control (use git)
- Large files (context limits exist)

---

## The Blob Philosophy

> "We mash and are mashed."

Content flows between forms. Files become blobs. Blobs become files. Text becomes structure. Structure becomes text.

The blob is a phase state. Solid (files) → Liquid (blob) → Solid (files).

The transformation must be reversible. Otherwise you're not mashing, you're destroying.

---

## Blob Antipatterns

### 1. The Clever Blob

Tries to handle every edge case. Dies in complexity.

**Fix:** Keep it simple. Accept limitations.

### 2. The Lossy Blob

`unmash(mash(x)) ≠ x` - Loses information.

**Fix:** Test round-trip. Every time.

### 3. The Context-Aware Blob

Looks ahead, looks behind, tracks state, maintains flags.

**Fix:** Stateless parsing. Each line is what it is.

### 4. The Untested Blob

Works on test data. Dies on production.

**Fix:** Test on production. With minimal test cases first.

---

## Blob Best Practices

### 1. Test Locally

```bash
# Tiny test
echo "test" > file.md
./mash.md . blob.md
./unmash.md blob.md out
diff -r . out  # Must be identical
```

### 2. Use Obvious Delimiters

`---` is obvious. `### FILE:` is obvious. `\x00\x00\x00` is not.

### 3. One Parse Pass

Read line by line. No lookahead. No state. Each line determines its own fate.

### 4. Fail Loudly

Bad blob? Error immediately. Don't try to recover. Don't guess.

---

## The Blob Lifecycle

```
Creation:  Files → Blob       (mash)
Transport: Blob → Somewhere   (copy, send, store)
Transform: Blob → Blob        (process, edit, AI)
Restore:   Blob → Files       (unmash)
Verify:    Files == Files     (diff)
```

If any step fails, the blob is broken. Fix it before proceeding.

---

## Blob Debugging

When blob fails:
1. Extract first file: works?
2. Extract second file: works?
3. Find first failure: why?
4. Make minimal test case
5. Fix with local testing
6. Re-run full blob

**Don't:** Guess at fixes. Add conditions. Complicate parsing.  
**Do:** Understand the failure. Simplify the format. Test the fix.

---

## Summary

**Blob:** Text files mashed together with delimiters.

**Purpose:** Aggregate many files for processing.

**Test:** Perfect round-trip (`unmash(mash(x)) == x`).

**Philosophy:** We mash and are mashed.

**Wisdom:** Keep it simple. Test it thoroughly. Trust the round-trip.

---

## See Also

- `test-loop-discipline.md` - How to test blobs properly
- `tombstone.md` - What happens when blobs get too clever
- `dir-mash.md` - A blob tool (simple version)
- `dir-unmash.md` - The reverse operation
