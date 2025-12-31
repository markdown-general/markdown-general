# Hugo Site Customization Strategy

**Site:** ~/site/
**Theme:** Congo v2.12.2
**Last Updated:** 2025-12-23

## Core Principle: Override, Don't Modify

Keep the theme pristine. Make customizations in your site directory, not in the theme.

---

## Directory Structure

```
~/site/
├── config/               # Your configuration (overrides theme)
├── layouts/              # Your layout overrides (overrides theme)
│   └── partials/
│       ├── logo.html                 # SVG logo sizing fix
│       └── extend-footer.html        # Custom footer
├── assets/               # Your assets
│   └── css/
│       └── custom.css               # Solarized color scheme
├── content/              # Your content
└── themes/congo/         # Theme (keep pristine)
    ├── layouts/          # Theme defaults (don't modify)
    └── config/           # Theme defaults (don't modify)
```

---

## Hugo's Override Hierarchy

Hugo searches in this order (first match wins):

1. **~/site/layouts/** ← Your customizations (highest priority)
2. **~/site/themes/congo/layouts/** ← Theme defaults (fallback)

Same applies to:
- Configuration: `config/` overrides `themes/congo/config/`
- Assets: `assets/` overrides `themes/congo/assets/`
- Static files: `static/` overrides `themes/congo/static/`

---

## How to Customize

### For Layout/Template Changes

1. **Find the file** in the theme:
   ```bash
   ls ~/site/themes/congo/layouts/
   ```

2. **Copy to your site** (preserve path):
   ```bash
   # Example: customize logo partial
   cp ~/site/themes/congo/layouts/_partials/logo.html \
      ~/site/layouts/partials/logo.html
   ```

3. **Edit YOUR copy**:
   ```bash
   # Edit ~/site/layouts/partials/logo.html
   ```

4. **Commit to your repo**:
   ```bash
   cd ~/site
   git add layouts/partials/logo.html
   git commit -m "Override logo partial for custom behavior"
   ```

### For Configuration Changes

Always edit files in **~/site/config/**, never in the theme:

```bash
# ✅ Correct
~/site/config/_default/params.toml

# ❌ Wrong
~/site/themes/congo/config/_default/params.toml
```

### For CSS/Styling Changes

Add custom CSS in **~/site/assets/css/custom.css**:

```bash
# Your custom styles
~/site/assets/css/custom.css
```

Hugo automatically includes files in `assets/css/` after theme CSS.

---

## Current Customizations

### Layout Overrides

| File | Purpose | Status |
|---|---|---|
|------|---------|--------|
| `layouts/partials/logo.html` | Fix SVG sizing (remove hardcoded dimensions) | Active |
| `layouts/partials/extend-footer.html` | "Made with Haskell & Hugo & congo" | Active |
| `layouts/partials/recent-articles.html` | Exclude preview section from homepage recents | Active |
### Configuration Overrides

All files in `~/site/config/_default/`:
- `hugo.toml` - Site settings, theme selection
- `params.toml` - Theme parameters, features
- `languages.en.toml` - Site title, author, links
- `menus.en.toml` - Navigation
- `markup.toml` - Markdown processing
- `taxonomies.toml` - Tags

### Asset Overrides

- `assets/css/custom.css` - Solarized color scheme (60 CSS variables)

### Recent Articles Override Details

The `recent-articles.html` override filters out the `preview` section from the homepage's recent articles list. This is necessary because `hugo.toml` mounts `~/mash/` directories as preview content at `content/preview/mash/*`. Without filtering, Hugo would include these preview pages in the recents section, pushing actual site posts off the list.

**What it does:**
- Adds `"Section" "!=" "preview"` filter to the recent articles query
- Only shows pages from the main site content, not preview mounts
- Keeps preview pages available locally at `http://localhost:1313/preview/*` for testing

---

## Build Output Configuration

### `.gitignore` in `~/site/public/`

The public directory (GitHub Pages repo) has a `.gitignore` that prevents publishing of locally-generated preview and pagination files:

```
/preview/
/page/
```

**Why this is needed:**
- Preview mounts generate `/preview/mash/*` pages during local builds
- Large number of preview pages causes pagination (`/page/2`, `/page/3`, etc.)
- These files clutter the published site and should only exist locally
- The `.gitignore` ensures they're not deployed to GitHub Pages

---

## Theme Updates

To update the Congo theme while preserving customizations:

```bash
cd ~/site/themes/congo

# Check current version
git log --oneline -1
# 53f7e89d 🔨 Preparing release v2.12.2

# Update from upstream
git fetch origin
git checkout dev
git pull origin dev

# Your overrides in ~/site/layouts/ are preserved automatically
```

---

## Verification

After making changes, verify Hugo uses your overrides:

```bash
cd ~/site

# Build and preview
hugo server --buildDrafts

# Check which template is used (in Hugo output)
# Look for: "layouts/partials/logo.html" (yours)
# Not: "themes/congo/layouts/_partials/logo.html" (theme)
```

---

## Git Workflow

### Main Site Repository

```bash
cd ~/site
git status              # Should be clean
git add <files>         # Stage customizations
git commit -m "msg"     # Commit YOUR changes
```

### Theme Repository (Keep Clean)

```bash
cd ~/site/themes/congo
git status              # Should be clean
# Don't commit changes here
# If accidentally modified, reset:
git reset --hard HEAD
```

### Deployment Repository

```bash
cd ~/site/public
# This is your GitHub Pages repo
# Usually updated by Hugo build process
# Separate from main site repo
```

---

## Best Practices

1. **Never modify theme files directly**
   - Theme updates will overwrite your changes
   - Hard to track what you changed

2. **Always use overrides**
   - Copy file to your site directory
   - Hugo automatically uses your version
   - Theme stays pristine

3. **Document customizations**
   - Keep this file updated
   - Note why each override exists

4. **Test after theme updates**
   - Check that overrides still work
   - Theme structure may change

5. **Commit often**
   - Small, focused commits
   - Clear commit messages
   - Easy to revert if needed

---

## Common Customization Patterns

### Override a Partial

```bash
# Pattern
cp themes/congo/layouts/_partials/FILENAME.html \
   layouts/partials/FILENAME.html

# Edit layouts/partials/FILENAME.html
```

### Extend a Partial (Hook System)

Congo provides extension points:
- `extend-head.html` - Add to `<head>`
- `extend-footer.html` - Add to footer
- `extend-article-link.html` - Add to article links

Just create the file in your `layouts/partials/`:

```bash
# Example: extend footer
echo '<p>Custom footer text</p>' > layouts/partials/extend-footer.html
```

### Override a Layout

```bash
# Single pages
layouts/_default/single.html

# List pages
layouts/_default/list.html

# Homepage
layouts/index.html
```

### Add Custom Shortcodes

```bash
# Create in your site
layouts/shortcodes/custom.html

# Use in content
{{</* custom param="value" */>}}
```

---

## Troubleshooting

### Hugo not using my override?

1. Check file path matches exactly (including `_partials` vs `partials`)
2. Restart `hugo server`
3. Clear browser cache
4. Check Hugo output for which template it's using

### Theme seems broken after update?

1. Check if theme changed file structure
2. Review your overrides for compatibility
3. Test by temporarily removing override
4. Adjust your override to match new theme structure

### Can't find file to override?

1. Check theme documentation
2. Search theme source:
   ```bash
   cd ~/site/themes/congo
   find . -name "*.html" | grep KEYWORD
   ```
3. Check if feature is in a partial or main layout

---

## References

- [Hugo Template Lookup Order](https://gohugo.io/templates/lookup-order/)
- [Congo Theme Documentation](https://jpanther.github.io/congo/)
- [Hugo Partials](https://gohugo.io/templates/partials/)

---

## Example: Adding a New Override

Let's say you want to customize the header:

```bash
# 1. Find the file
ls ~/site/themes/congo/layouts/_default/baseof.html
# or
ls ~/site/themes/congo/layouts/partials/header.html

# 2. Copy to your site
mkdir -p ~/site/layouts/partials
cp ~/site/themes/congo/layouts/partials/header.html \
   ~/site/layouts/partials/header.html

# 3. Edit your copy
# Make changes to ~/site/layouts/partials/header.html

# 4. Test
cd ~/site
hugo server --buildDrafts

# 5. Commit
git add layouts/partials/header.html
git commit -m "Customize header layout"

# 6. Document
# Add entry to "Current Customizations" section above
```

Done! Your customization is now permanent and survives theme updates.
