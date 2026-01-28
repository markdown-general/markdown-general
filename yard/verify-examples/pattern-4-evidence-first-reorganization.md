# pattern: evidence-first reorganization ⟜ gather state before deciding

**What it is:** Multi-phase work where each phase has clear success criteria before proceeding to the next.

**When to use:** Large refactoring or health improvement where you need to understand the current state before making changes.

---

## Example: Haskell Repository Health Assessment

**verification** ⟜ What success looks like at each phase

---

## Phase 1: Discover ⟜ Inventory the repository

**Success criteria:**
- All .hs files found (excluding dist/, build/, .git/)
- All .cabal files found
- All test files identified
- File count matches expectation (git ls-files | wc -l ≈ inventory count)
- Inventory file generated with no errors

**How we'll verify:**
```bash
# Test 1: Find all source files
find src -name "*.hs" > /tmp/inventory-src.txt
wc -l /tmp/inventory-src.txt | awk '{print $1}' > /tmp/src-count.txt

# Test 2: Find all test files
find test -name "*.hs" 2>/dev/null > /tmp/inventory-test.txt || echo "0" > /tmp/test-count.txt

# Test 3: Find all cabal files
find . -name "*.cabal" > /tmp/inventory-cabal.txt

# Test 4: Generate inventory
cat /tmp/inventory-src.txt /tmp/inventory-test.txt > /tmp/markdown-inventory.md
echo "Generated inventory:"
wc -l /tmp/markdown-inventory.md

# Verification check
[ -f /tmp/markdown-inventory.md ] && [ -s /tmp/markdown-inventory.md ] && echo "✓ Phase 1 Complete" || echo "✗ Phase 1 Failed"
```

**Before proceeding to Phase 2:**
- Inventory file exists and is non-empty
- File count seems reasonable (not 0, not wildly inflated)
- No errors during discovery
- git status clean (no files modified)

---

## Phase 2: Extract ⟜ Structure for analysis

**Success criteria:**
- Each file analyzed for: module name, dependencies, test status
- Structured output generated (TSV or markdown table)
- No parsing errors (all files readable)
- Extraction completed without timeouts
- git status clean

**How we'll verify:**
```bash
# Test 1: Parse all source files for structure
for file in $(cat /tmp/markdown-inventory.md); do
  head -20 "$file" | grep "^module " | head -1 >> /tmp/extracted.txt
done
wc -l /tmp/extracted.txt

# Test 2: Check for extraction errors
grep -c "ERROR" /tmp/extracted.txt && echo "✗ Parse errors found" || echo "✓ No parse errors"

# Test 3: Generate structured output
cat /tmp/extracted.txt > /tmp/extracted-structure.md
[ -f /tmp/extracted-structure.md ] && [ -s /tmp/extracted-structure.md ] && echo "✓ Phase 2 Complete" || echo "✗ Phase 2 Failed"

# Test 4: No modifications
git status --short | wc -l | awk '{if ($1 == 0) print "✓ Clean"; else print "✗ Modified"}'
```

**Before proceeding to Phase 3:**
- All files parsed without errors
- Structured output generated
- Extraction completed in reasonable time (< 2 min)
- git status clean

---

## Phase 3: Analyze ⟜ Find patterns, holes, dependencies

**Success criteria:**
- Dependency graph generated (shows module imports)
- Orphan modules identified (no one imports them)
- Cyclic dependencies found (if any)
- Density metrics calculated (loc, imports per module)
- Analysis report generated
- git status clean (analysis is read-only)

**How we'll verify:**
```bash
# Test 1: Build dependency graph
grep "^import " /tmp/markdown-inventory.md | sort | uniq > /tmp/dependencies.txt
[ -f /tmp/dependencies.txt ] && [ -s /tmp/dependencies.txt ] && echo "✓ Deps found" || echo "! No imports found"

# Test 2: Identify orphans (modules no one imports)
comm -23 <(sort /tmp/extracted.txt) <(sort /tmp/dependencies.txt | cut -d' ' -f2) > /tmp/orphans.txt
echo "Orphan modules:"
wc -l /tmp/orphans.txt

# Test 3: Check for cycles (simple: A imports B, B imports A)
# (Full cycle detection is complex; this is simplified)
echo "Cycle check: (manual review needed)"
grep -E "(import.*A.*import.*B|B.*A)" /tmp/dependencies.txt || echo "✓ No obvious cycles"

# Test 4: Generate analysis report
cat > /tmp/analysis-report.md << 'EOF'
# Analysis Report

## Modules: $(wc -l < /tmp/extracted.txt)
## Orphans: $(wc -l < /tmp/orphans.txt)
## Dependencies: $(wc -l < /tmp/dependencies.txt)
EOF

[ -f /tmp/analysis-report.md ] && echo "✓ Phase 3 Complete" || echo "✗ Phase 3 Failed"

# Test 5: No modifications
git status --short | wc -l | awk '{if ($1 == 0) print "✓ Clean"; else print "✗ Modified"}'
```

**Before proceeding to Phase 4:**
- Dependency graph generated
- Orphan modules identified (number known)
- Cycles checked (documented if found)
- Analysis report written
- git status clean

---

## Phase 4: Synthesize ⟜ Write findings and next steps

**Success criteria:**
- Synthesis document written
- Identifies: patterns, holes, duplications, missing pieces
- Proposes next steps (which are separate jobs, not part of this card)
- No code changes (synthesis is decision document)
- git status clean
- Operator reviews and approves before Phase 5

**How we'll verify:**
```bash
# Test 1: Synthesis document exists and has content
[ -f /tmp/synthesis.md ] && [ -s /tmp/synthesis.md ] && echo "✓ Synthesis written" || echo "✗ No synthesis"

# Test 2: Contains required sections
for section in "Patterns" "Holes" "Duplications" "Next Steps"; do
  grep -q "## $section" /tmp/synthesis.md && echo "✓ $section documented" || echo "! Missing: $section"
done

# Test 3: No code changes
git status --short | wc -l | awk '{if ($1 == 0) print "✓ Clean"; else print "✗ Modified"}'

# Test 4: Ready for human review
echo "Synthesis ready for review:"
head -30 /tmp/synthesis.md
```

**HALT before Phase 5:**
- Synthesis document complete
- Operator reviews findings
- Operator approves (or rejects) proposed next steps
- If major reorganization needed, extract as separate jobs

---

## verification protocol

**Between each phase:**
- Verification checklist must pass 100%
- If any check fails, STOP
- Report which check failed and why
- Do not proceed until human approves fix

**Safety at each phase:**
- Phases 1-3 are READ-ONLY (no file modifications)
- Phase 4 is read-only (synthesis only)
- Phase 5+ would be separate jobs (not part of this card)

**Breathing discipline:**
- After Phase 1: pause, review inventory
- After Phase 2: pause, check structure makes sense
- After Phase 3: pause before analysis
- After Phase 4: MUST pause, human decides next

**Git safety:**
- Before each phase: git status clean
- After each phase: git status clean (no modifications)
- If any phase modifies files unexpectedly: STOP, investigate

---

## comment

This pattern is **evidence-first decision-making**:
- You gather state before deciding what to do
- Each phase produces evidence (inventory, structure, dependencies)
- Synthesis is based on evidence, not guessing
- Next steps are explicit jobs extracted from findings

The verification gates prevent blaze-through behavior. You can't claim Phase 3 done without proof.
