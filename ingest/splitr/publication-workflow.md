# Publication Workflow

**How content leaves mash/ and becomes a blog**

---

## Overview

1. **Author** - Write anywhere in ~/mash/ (self/, base/, projects/)
2. **Preview** - See it rendered via Hugo locally (all content, all artifacts)
3. **Publish** - Select specific posts and artifacts via publish.md
4. **Deploy** - Push to GitHub Pages

---

## The Literate Programming Approach

**publish.md is executable markdown** - simultaneously:
- Documentation explaining the workflow
- Publication lists (what to publish)
- Bash script that copies files
- Complete in one file

This is "we mash" applied to deployment: code, docs, and data unified.

Run with: `cd ~/mash/base && ./publish.md`

---

## Authoring

Write markdown anywhere in ~/mash/:

```bash
vim ~/mash/base/my-post.md      # Knowledge base
vim ~/mash/self/ideas.md        # Experimental
vim ~/mash/projects/updates.md  # Active work
```

Add artifacts:
```bash
cp diagram.svg ~/mash/artifacts/
```

Reference in markdown:
```markdown
![Diagram](/artifacts/diagram.svg)
```

---

## Live Preview

Hugo mounts ~/mash/ directories for preview:

```bash
cd ~/site
hugo server --buildDrafts
```

**Visit:**
- http://localhost:1313/preview/base/my-post/
- http://localhost:1313/preview/self/ideas/
- http://localhost:1313/preview/projects/updates/

**All artifacts visible** - instant feedback loop. Edit in ~/mash/, browser updates automatically.

This closes the loop: content gets mashed (defunctionalized to markdown) → Hugo renders (refunctionalizes to HTML) → you see results → adjust → repeat.

---

## Publishing

Edit `~/mash/base/publish.md`:

```markdown
## Posts to Publish
- my-post.md
- another-post.md

## Artifacts to Publish
- diagram.svg
- chart.png
```

Run the script:
```bash
cd ~/mash/base
./publish.md
```

This copies:
- Selected markdown → `~/site/content/posts/`
- Selected artifacts → `~/site/static/artifacts/`
- Infrastructure → `~/site/` (config, CSS, layouts)

---

## Building

```bash
cd ~/site
hugo
```

Generates `~/site/public/` with selected content.

---

## Deployment

```bash
cd ~/site/public
git add -A
git commit -m "Publish: description"
git push origin main
```

Site updates at https://tonyday567.github.io/

---

## Key Concepts

### Preview vs Published

**Preview (local only):**
- All markdown in ~/mash/ visible
- All artifacts available
- For development and experimentation

**Published (production):**
- Only files listed in publish.md
- Only selected artifacts
- Your actual blog

### Single Source of Truth

```
~/mash/          ← Author here (single source)
    ↓ (publish.md)
~/site/              ← Staging (selected content)
    ↓ (hugo)
~/site/public/       ← Built site
    ↓ (git push)
GitHub Pages         ← Live
```

### Why Literate Programming

Traditional:
- publish.sh (script)
- publish-list.txt (data)  
- README.md (docs)

**publish.md approach:**
- All three unified
- Documentation can't get out of sync
- Lists are self-documenting
- Version control tracks publication history

This is mashing applied to deployment.

---

## File Locations

### Source (~/mash/)

**Content:**
- base/, self/, projects/ - Markdown content
- artifacts/ - Images, PDFs, supporting files
- artifacts/site-infrastructure/ - Hugo config, CSS, layouts

**Control:**
- base/publish.md - Executable publication script

### Build (~/site/)

**Staging:**
- content/posts/ - Published posts (copied)
- static/artifacts/ - Published artifacts (copied)
- config/, assets/, layouts/ - Infrastructure (deployed)

**Output:**
- public/ - Built site (deploy to GitHub)

---

## Common Tasks

### Publish New Post

1. Write: `vim ~/mash/base/new-article.md`
2. Preview: `hugo server --buildDrafts`
3. List in publish.md
4. Run: `./publish.md`
5. Deploy: `hugo && cd public && git add -A && git commit && git push`

### Update Post

1. Edit in ~/mash/base/
2. Run `./publish.md` (re-copies)
3. Deploy

### Unpublish

1. Remove from publish.md (comment out)
2. Delete from ~/site/content/posts/
3. Rebuild and deploy

---

## Troubleshooting

**Preview not showing?**
- Check frontmatter exists
- Restart Hugo server
- Verify URL: `/preview/base/post/`

**Artifacts not in production?**
- Must be listed in publish.md
- Run `./publish.md` to copy
- Rebuild with `hugo`

**Script not executable?**
```bash
chmod +x ~/mash/base/publish.md
```

---

## The Feedback Loop

The Hugo preview system enables the mashing process:

1. Content mashed into markdown (defunctionalized)
2. Hugo renders to HTML (refunctionalized)  
3. Preview shows result immediately
4. Adjust and iterate
5. Repeat

This closed loop enables rapid transformation. The $65/3hr productivity came from removing preview friction - you could see what was being mashed in real-time.

---

**Single source in ~/mash/, selective publication via literate programming, instant preview for feedback.**
