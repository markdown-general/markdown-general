# Org-Mode to Markdown Migration

**How ten years of org-mode notes became markdown**

**Date:** December 22, 2025
**Status:** Completed

---

## What Happened

Separated operational content (todos) from knowledge content (documentation), then flattened everything into markdown.

**Migration principle:** Todos stay in org-mode (operational, ephemeral), knowledge becomes markdown (permanent, mashed).

See todo-architecture.md for rationale.

---

## The Process

### 1. Backup

```bash
BACKUP_DATE=$(date +%Y%m%d-%H%M%S)
tar -czf "$HOME/org-migration-backup-${BACKUP_DATE}.tar.gz" org/
```

Created timestamped tar backup: `~/org-migration-backup-20251222-093005.tar.gz` (15MB)

Optional git init in ~/org/ for additional safety.

### 2. Setup Directories

```bash
mkdir -p ~/org/todos
mkdir -p ~/mash/other
```

### 3. Move Todo Files

Kept in org-mode format:
- todo.org → ~/org/todos/
- projects.org → ~/org/todos/
- projects.org_archive → ~/org/todos/
- bugz.org → ~/org/todos/

These are operational, not knowledge - don't need markdown conversion.

### 4. Convert Knowledge Files

Used pandoc to flatten org → markdown:

```bash
pandoc --from=org --to=gfm input.org -o output.md --wrap=none
```

All remaining .org files → .md files in ~/mash/

### 5. Cleanup

Converted org-mode link syntax to markdown:
```bash
# [[link][description]] → [description](link.md)
sed -i '' 's/\[\[\([^]]*\)\]\[\([^]]*\)\]\]/[\2](\1.md)/g' file.md

# .org references → .md
sed -i '' 's/\([^[:alnum:]]\)\([[:alnum:]_-]*\)\.org/\1\2.md/g' file.md
```

Removed original .org files after successful conversion.

### 6. Validation

Verified:
- Todo files in ~/org/todos/: 4
- Knowledge files in ~/mash/: converted count
- Empty files: 0
- Link issues: 0

---

## Result

```
~/org/
├── todos/
│   ├── todo.org
│   ├── projects.org
│   ├── projects.org_archive
│   └── bugz.org
└── refile.org

~/mash/
├── base/          # Created post-migration
├── self/          # Created post-migration
├── projects/      # Created post-migration
├── artifacts/     # Created post-migration
└── *.md          # Converted files
```

Backup: `~/org-migration-backup-20251222-093005.tar.gz` (15MB)

---

## Post-Migration Organization

After flat conversion, content was organized into:
- **base/** - Static, stable knowledge
- **self/** - Experimental, evolving thoughts
- **projects/** - Active project documentation
- **artifacts/** - Non-markdown supporting files

This structure supports the mashing machine principle - content undergoes continuous refinement through AI collaboration.

---

## Tools Used

- **Pandoc** - Org → GFM conversion
- **sed** - Link syntax conversion (macOS compatible)
- **tar** - Backup
- **bash** - Automation

---

## Key Decisions

### 1. Separation of Concerns

Operational (todos) vs Knowledge (documentation) - different formats for different purposes.

### 2. Todos Stay in Org-Mode

Preserve org-capture, org-refile, org-agenda functionality. Operational data doesn't need markdown.

### 3. Knowledge to Markdown

Better for:
- AI collaboration
- Version control
- GitHub-friendly
- Mashing process

### 4. Progressive Organization

Easier to:
1. Flatten everything first (migration)
2. Organize into base/self/projects/ (post-migration)
3. Refine through mashing (ongoing)

Rather than planning perfect structure upfront.

---

## What Was Lost and Recovered

### Lost: Org-Babel

Code execution in org files:
```org
#+begin_src bash
echo "hello"
#+end_src
```

### Recovered: Executable Markdown

publish.md pattern - literate programming in markdown:
- Script embedded in markdown
- Self-documenting
- Portable (no Emacs required)
- Version controlled

See publish.md and publication-workflow.md for examples.

---

## The $65/3-Hour Session

This migration enabled the high-productivity session where:
- Content could be previewed in Hugo immediately
- AI could mash aggressively without org-mode constraints
- Communication compressed to "mash?" "we mash"
- Ten years of notes transformed in one session

The Hugo preview system closed the feedback loop - instant visualization of what was being mashed.

---

## Related Documentation

- todo-architecture.md - Why todos stay in org-mode
- content-guidelines.md - How mashing works
- publication-workflow.md - Preview and publish flow

---

**Migration complete. Content now flows through the mashing machine.**

# Org-Markdown Migration Analysis Project

**Status:** Active analysis project
**Type:** Historical document extraction
**Workflow Example:** base → projects (when content needs mashing)

---

## Why This Became a Project

The org-markdown-migration.md file was living in base/ as a historical document about a completed December 2024 migration. While the migration itself is complete, the document contains valuable insights that need extraction and redistribution.

**This is an example of the base → projects workflow pattern:**
- Content in base/ should be current, stable, and actionable
- Historical documents with embedded wisdom need "mashing" (analysis and extraction)
- When content requires work, it becomes a project
- Extracted useful material flows back to base/, self/, or other projects

---

## Project Goal

Analyze org-markdown-migration.md and extract reusable material that has current value.

---

## Potential Outcomes

Material from this document may be extracted to:

### → base/
- **Separation of concerns principle** (operational vs knowledge content)
- **Progressive organization approach** (flatten first, organize later)
- **Pandoc conversion commands** (reference material)

### → self/
- **The $65/3-hour session story** (productivity insight, workflow compression pattern)
- **"Mashing machine" concept** (thinking about AI collaboration)

### → Other projects/
- **Migration patterns** if similar conversions are needed
- **Link conversion sed commands** as reusable recipes

---

## What Gets Removed

- Historical timestamps and "completed" status markers
- Step-by-step migration process (no longer actionable)
- Specific backup file names and paths
- Redundant validation steps

---

## Analysis Questions

1. What principles from this migration apply to current work?
2. Which commands/patterns are reusable?
3. What insights about AI collaboration are worth preserving?
4. Does any material belong in existing base/ documents?

---

## Next Steps

1. Read org-markdown-migration.md thoroughly
2. Identify extractable material
3. Create/update target documents in base/ or self/
4. Archive or remove this project once extraction complete

---

**This project demonstrates the mash workflow: content continuously flows through the system, getting refined and redistributed where it's most valuable.**

