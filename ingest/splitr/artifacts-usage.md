# Using Artifacts in Hugo Content

**Date:** 2025-12-23
**Purpose:** Guide for referencing shared artifacts from Hugo content

---

## Setup Complete ✅

Hugo is now configured to mount `~/markdown/artifacts/` as `/artifacts/` in your site.

**Configuration:** `~/site/config/_default/hugo.toml`
```toml
[[module.mounts]]
  source = "/Users/tonyday567/markdown/artifacts"
  target = "static/artifacts"
```

---

## How to Use

### Link Syntax in Markdown

Reference artifacts using the `/artifacts/` path:

```markdown
# Images
![Description](/artifacts/image-name.png)

# PDFs
[Download PDF](/artifacts/document.pdf)

# SVG Charts
![Chart](/artifacts/chart.svg)

# Inline images with sizing
![Alt text](/artifacts/diagram.png "Title")
```

### Current Artifacts Available

Located in `~/markdown/artifacts/`:
- `DaoFP.pdf` (932KB) - Data-Oriented Application For Problems PDF
- `Loopless_Functional_Algorithms.pdf` (654KB)
- `backpermute.png` (18KB)
- `keys.png` (93KB)
- `recovery-codes.txt` (208B)

### Example Usage

**In your markdown:**
```markdown
---
title: "My Blog Post"
date: 2025-12-23
---

Here's a diagram explaining the concept:

![Backpermute visualization](/artifacts/backpermute.png)

For more details, see the [complete paper](/artifacts/DaoFP.pdf).
```

**Hugo renders as:**
```html
<p>Here's a diagram explaining the concept:</p>
<img src="/artifacts/backpermute.png" alt="Backpermute visualization">

<p>For more details, see the <a href="/artifacts/DaoFP.pdf">complete paper</a>.</p>
```

**In browser:**
- Image loads from: `https://tonyday567.github.io/artifacts/backpermute.png`
- PDF loads from: `https://tonyday567.github.io/artifacts/DaoFP.pdf`

---

## Benefits

1. **Single source of truth:** Artifacts live in `~/markdown/artifacts/`
2. **No duplication:** Hugo reads directly from artifacts directory
3. **Consistent references:** Always use `/artifacts/` path
4. **Easy management:** Add/update artifacts in one location
5. **Git-friendly:** Artifacts versioned separately from content

---

## Workflow

### Adding New Artifacts

1. **Copy to artifacts directory:**
   ```bash
   cp ~/Downloads/new-diagram.svg ~/markdown/artifacts/
   ```

2. **Reference in markdown:**
   ```markdown
   ![New diagram](/artifacts/new-diagram.svg)
   ```

3. **Build and test:**
   ```bash
   cd ~/site
   hugo server --buildDrafts
   # Visit http://localhost:1313
   ```

4. **Deploy:**
   ```bash
   hugo
   cd public
   git add -A
   git commit -m "Add new diagram"
   git push
   ```

### Organizing Artifacts

For larger projects, consider subdirectories:

```bash
~/markdown/artifacts/
├── diagrams/
│   ├── flowchart.svg
│   └── architecture.svg
├── papers/
│   ├── DaoFP.pdf
│   └── algorithms.pdf
└── screenshots/
    └── demo.png
```

Reference with subdirectory:
```markdown
![Flowchart](/artifacts/diagrams/flowchart.svg)
[Read paper](/artifacts/papers/DaoFP.pdf)
```

---

## Hugo Shortcodes with Artifacts

### Figure Shortcode

```markdown
{{< figure src="/artifacts/backpermute.png"
           caption="Backpermute operation visualization"
           width="400" >}}
```

### Multiple Formats

```markdown
# Responsive image with fallbacks
<picture>
  <source srcset="/artifacts/diagram.webp" type="image/webp">
  <img src="/artifacts/diagram.png" alt="Diagram">
</picture>
```

---

## Important Notes

### Paths Must Start with `/artifacts/`

✅ **Correct:**
```markdown
![Image](/artifacts/image.png)
```

❌ **Wrong:**
```markdown
![Image](~/markdown/artifacts/image.png)
![Image](artifacts/image.png)  # Missing leading slash
![Image](../artifacts/image.png)
```

### Deployment Behavior

**Local development (`hugo server`):**
- Hugo reads directly from `~/markdown/artifacts/`
- Changes to artifacts reflected immediately

**Production build (`hugo`):**
- Artifacts copied to `public/artifacts/`
- Deployed to GitHub Pages as static files
- Accessible at `https://tonyday567.github.io/artifacts/`

### .gitignore Considerations

The `~/site/public/` directory is gitignored in the source repo but IS the deployment repo. Artifacts will be deployed to GitHub Pages.

If artifacts contain sensitive data:
1. Don't put them in `~/markdown/artifacts/`
2. Use a private artifacts directory
3. Or add to `.gitignore` in deployment repo

---

## Troubleshooting

### Artifact not loading?

1. **Check path:** Must start with `/artifacts/`
2. **Check file exists:** `ls ~/markdown/artifacts/filename`
3. **Rebuild:** `hugo --quiet` to regenerate
4. **Check case:** Filenames are case-sensitive

### 404 on deployed site?

1. **Verify in public:** `ls ~/site/public/artifacts/`
2. **Check deployment:** `cd ~/site/public && git status`
3. **Redeploy:** Build and push again

### Large artifacts?

Consider:
- Compressing images (PNG → WebP, optimize SVG)
- Using CDN for large files
- Git LFS for very large binaries
- External hosting (S3, CDN) for massive assets

---

## Advanced: Content Mounts

You can also mount content directories. Currently only artifacts are mounted.

To mount content from `~/markdown/base/`:
```toml
[[module.mounts]]
  source = "/Users/tonyday567/markdown/base/site-content"
  target = "content/posts"
```

This would allow authoring directly in `~/markdown/base/` without copying to `~/site/content/`.

---

## Summary

✅ Artifacts in `~/markdown/artifacts/` are automatically available at `/artifacts/`
✅ Use `/artifacts/filename` in all markdown links
✅ No copying or symlinking required
✅ Single source of truth maintained
✅ Works in development and production

Your content authoring and artifact management are now cleanly separated!
