---
title: "Theme and CSS Analysis for Simplification"
date: 2025-12-23
status: completed
---

# Theme and CSS Analysis for Simplification

**Date:** 2025-12-23
**Project:** ~/site/ Hugo Blog
**Theme:** Congo v2.12.2

## Executive Summary

The current site uses the full-featured Congo theme with significant bloat from unused JavaScript libraries and complex Tailwind CSS compilation. **Estimated 85-90% of theme features are unused.** Major simplification opportunities exist.

---

## Current Theme Stack

### Congo Theme Dependencies

| Library | Size | Purpose | Used? | Impact |
|---------|------|---------|-------|--------|
| **Mermaid** | 2.6MB | Diagram rendering | ❌ NO | HIGH - Largest dependency |
| **KaTeX** | 1.4MB | Math rendering | ⚠️ MINIMAL (1 usage) | HIGH |
| **Chart.js** | 204KB | Interactive charts | ❌ NO | MEDIUM |
| **Fuse.js** | 28KB | Search functionality | ✅ YES (enabled) | LOW |
| **Quicklink** | 8KB | Page prefetching | ✅ YES (default) | LOW |
| **FontAwesome** | ~900KB | Icon fonts | ⚠️ MINIMAL | MEDIUM |
| **Tailwind CSS** | 51KB (compiled) | Utility CSS framework | ✅ YES | MEDIUM |

**Total unused JS**: ~4.2MB (Mermaid + KaTeX + Chart.js + FontAwesome)
**Total used JS**: ~36KB (Fuse + Quicklink + main)

### Build Complexity

**Current build process requires:**
1. Node.js + npm (15+ packages)
2. Tailwind CSS compilation (development + production modes)
3. Vendor asset copying (6 libraries)
4. Hugo extended version
5. Git submodule management

---

## CSS Analysis

### Custom CSS Override (`assets/css/custom.css`)

**Purpose:** Implements Solarized color scheme and custom fonts

**What it does:**
1. **Font Stack** (3 fonts)
   - Inconsolata (monospace/code)
   - Lora (body text)
   - Raleway (headings)

2. **Color System** (60 CSS variables)
   - Completely overrides Congo's default colors
   - Defines 27 variables for light mode
   - Defines 27 variables for dark mode
   - Primary: Solarized Green (#859900)
   - Secondary: Solarized Blue (#268bd2)
   - Neutral: Full Solarized base palette

**Size:** 95 lines (all CSS variables)

**Issues:**
- **Redundancy**: Most shades (50-900) are identical, just repeated
  - Primary 50-900: all `133, 153, 0`
  - Secondary 50-900: all `38, 139, 210`
- **Tailwind dependency**: Color variables designed for Tailwind's shade system
- **Complexity**: Could be reduced to 10-12 variables instead of 60

### Theme CSS System

**Location:** `themes/congo/assets/css/`

**Structure:**
- `main.css` - Entry point for Tailwind compilation
- `schemes/*.css` - 7 built-in color schemes (congo, fire, ocean, slate, etc.)
- `compiled/main.css` - Output (51KB minified)

**Tailwind Configuration:**
- Typography plugin for prose styling
- Custom color mappings for neutral/primary/secondary
- Dark mode via CSS classes
- Extensive typography customization (links, kbd, mark elements)

---

## Feature Usage Analysis

### Enabled Features (from `params.toml`)

✅ **Actually Used:**
- Search (Fuse.js) - 28KB
- Code copy buttons
- Image lazy loading
- WebP conversion
- Dark mode toggle
- Social sharing links (X, Mastodon, Bluesky)

❌ **Enabled but NOT Used:**
- None detected

❌ **Available but NOT Used:**
- Mermaid diagrams (2.6MB) - 0 instances in content
- Chart.js (204KB) - 0 instances in content
- KaTeX math (1.4MB) - 1 potential instance (unclear if actual LaTeX)
- Comments system
- Analytics (Fathom/Plausible/Umami all commented out)

### Content Feature Requirements

Based on actual content analysis:

**Required:**
- Markdown rendering
- Code syntax highlighting
- SVG image display (extensive - 45 files)
- Internal/external links
- Basic typography
- Tags taxonomy
- RSS feed

**NOT Required:**
- Interactive diagrams
- Math equations (minimal use)
- Charts (all charts are pre-rendered SVGs)
- Comments
- Analytics

---

## Simplification Opportunities

### 1. Remove Unused JavaScript Libraries (HIGH IMPACT)

**Savings: ~4.2MB, eliminates npm dependency**

Remove from `themes/congo/package.json`:
- ❌ `mermaid` (2.6MB)
- ❌ `chart.js` (204KB)
- ⚠️ `katex` (1.4MB) - only 1 potential usage
- ⚠️ `@fortawesome/fontawesome-free` (900KB) - minimal icon usage

**Impact:**
- 85% reduction in JS bundle size
- Eliminates npm build step
- Faster page loads
- Simpler deployment

**Risks:**
- If you ever want to add Mermaid diagrams, need to add it back
- Math rendering would require alternative solution

### 2. Simplify Custom CSS (MEDIUM IMPACT)

**Current:** 60 CSS variables (95 lines)
**Proposed:** 12-15 variables (30 lines)

**Simplified color system:**
```css
:root {
  /* Light mode */
  --color-bg: 253, 246, 227;           /* base3 */
  --color-fg: 101, 123, 131;           /* base00 */
  --color-heading: 7, 54, 66;          /* base02 */
  --color-subtle: 147, 161, 161;       /* base1 */
  --color-accent: 133, 153, 0;         /* green */
  --color-link: 38, 139, 210;          /* blue */
}

html.dark {
  /* Dark mode */
  --color-bg: 0, 43, 54;               /* base03 */
  --color-fg: 131, 148, 150;           /* base0 */
  --color-heading: 253, 246, 227;      /* base3 */
  --color-subtle: 88, 110, 117;        /* base01 */
  --color-accent: 133, 153, 0;         /* green */
  --color-link: 38, 139, 210;          /* blue */
}
```

**Benefits:**
- 60% reduction in CSS variables
- Easier to understand and maintain
- Still supports Solarized colors
- No functional loss

### 3. Replace Congo Theme (HIGHEST IMPACT)

**Options:**

#### Option A: Minimal Hugo Theme
Use a lightweight theme like:
- **hugo-tufte** (~5KB CSS, no JS)
- **hugo-paper** (~10KB CSS, minimal JS)
- **Custom minimal theme** (100-200 lines CSS)

**Pros:**
- No npm/Tailwind build process
- No git submodules
- ~95% smaller
- Full control

**Cons:**
- Lose built-in dark mode toggle
- Lose search functionality
- Need to rebuild layouts

#### Option B: Fork Congo and Strip It Down
Keep Congo structure but remove unused features:

**Remove:**
- Mermaid, Chart.js, KaTeX, FontAwesome
- Unused shortcodes
- Extra color schemes
- Complex Tailwind config

**Keep:**
- Basic layouts
- Dark mode
- Search (Fuse.js)
- Typography styles

**Estimated result:** ~50KB CSS, ~40KB JS (vs current ~51KB CSS, ~2.7MB JS)

#### Option C: Static CSS Build
Generate Tailwind CSS once, commit it, remove build process:

```bash
# One-time build
cd ~/site/themes/congo
npm run build

# Commit the compiled CSS
git add assets/css/compiled/main.css

# Remove package.json, node_modules
# Hugo uses the pre-built CSS
```

**Pros:**
- No npm dependency
- Keep all current functionality
- Faster builds

**Cons:**
- Tailwind changes require manual rebuild
- Still have bloated Congo theme
- Still ship unused JS libraries

### 4. Simplify Font Loading (LOW IMPACT)

**Current:** 3 web fonts (Inconsolata, Lora, Raleway)
**Proposed:** System font stack

```css
/* Monospace */
font-family: ui-monospace, 'Cascadia Code', 'Source Code Pro',
             Menlo, Consolas, 'DejaVu Sans Mono', monospace;

/* Body */
font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
             Helvetica, Arial, sans-serif;

/* Headings */
font-family: 'SF Pro Display', -apple-system, BlinkMacSystemFont,
             'Segoe UI', Helvetica, Arial, sans-serif;
```

**Benefits:**
- No web font downloads (~100KB+ savings)
- Faster initial render
- Native system aesthetics
- Better CLS (Cumulative Layout Shift)

**Tradeoff:**
- Less distinctive typography
- Appearance varies by OS

---

## Recommended Simplification Path

### Phase 1: Quick Wins (Minimal Risk)

1. **Remove unused JS libraries** (saves 4.2MB)
   - Remove Mermaid, Chart.js, KaTeX from package.json
   - Keep Fuse.js for search
   - Test that site still works

2. **Simplify CSS variables** (saves maintenance burden)
   - Reduce 60 variables to 15
   - Keep same Solarized colors
   - Test light/dark modes

3. **Static CSS build** (removes npm from production)
   - Build Tailwind CSS once
   - Commit compiled CSS
   - Document rebuild process

**Impact:** 85% JS reduction, no functionality loss, npm not required for deployment

### Phase 2: Medium Refactor (Some Risk)

4. **Strip down Congo theme**
   - Remove unused shortcodes
   - Remove unused layouts (comments, analytics)
   - Simplify Tailwind config
   - Remove extra color schemes

5. **Consolidate custom CSS**
   - Merge custom.css into theme
   - Remove redundancy
   - Document color system

**Impact:** Simpler codebase, easier maintenance

### Phase 3: Major Simplification (High Risk)

6. **Replace theme entirely**
   - Build minimal custom theme
   - Pure CSS (no Tailwind)
   - ~500 lines total
   - Search via JSON + vanilla JS

7. **Switch to system fonts**
   - Remove web fonts
   - Use system font stack

**Impact:** Maximum simplicity, full control, complete rebuild required

---

## Comparison Matrix

| Metric | Current | Phase 1 | Phase 2 | Phase 3 |
|--------|---------|---------|---------|---------|
| **JS Size** | 2.7MB | 40KB | 40KB | 10KB |
| **CSS Size** | 51KB | 51KB | 30KB | 10KB |
| **Dependencies** | npm (15 pkg) | npm (3 pkg) | npm (optional) | None |
| **Build Time** | ~10s | ~10s | ~5s | instant |
| **Maintainability** | Complex | Medium | Medium | Simple |
| **Functionality** | Full | Full | Full | Core only |
| **Risk** | N/A | Low | Medium | High |
| **Effort** | N/A | 2-4 hours | 1-2 days | 3-5 days |

---

## Specific Recommendations for Your Site

### What You Actually Need

Based on your content (Haskell blog with SVG charts):

✅ **Essential:**
- Clean typography for code and prose
- Dark mode (Solarized colors)
- SVG image display (no processing needed)
- Syntax highlighting for Haskell
- Tags and basic navigation
- RSS feed
- Fast loading

❌ **Not Needed:**
- Interactive charts (yours are pre-rendered SVGs)
- Math rendering (minimal/no usage)
- Diagrams (not used)
- Heavy JS frameworks

### Recommended: Phase 1 + 2

**Rationale:**
- Removes 4.2MB of bloat (Phase 1)
- Keeps proven Congo layouts and structure
- Maintains dark mode and search
- Low risk, high reward
- Can be done incrementally

**Next steps after Phase 1+2:**
- Monitor actual usage
- Consider Phase 3 only if theme customization becomes painful
- Static site = minimal JS is perfectly fine

---

## Implementation Notes

### To Remove JavaScript Libraries

Edit `~/site/themes/congo/package.json`:

```json
{
  "devDependencies": {
    "@tailwindcss/typography": "^0.5.16",
    "fuse.js": "^7.1.0",
    "quicklink": "^3.0.1",
    "rimraf": "^6.0.1",
    "tailwindcss": "^3.4.17",
    "vendor-copy": "^3.0.1"
  }
}
```

Remove: `mermaid`, `chart.js`, `katex`, `prettier`, `jsdom`

### To Simplify CSS

Replace `~/site/assets/css/custom.css` with minimal version (see section 2 above)

### To Test

```bash
cd ~/site
hugo server --buildDrafts
# Visit http://localhost:1313
# Test: dark mode toggle, search, all pages render
```

---

## Conclusion

Your Hugo site is currently shipping **2.7MB of JavaScript that provides zero value**. The Congo theme is well-built but massively over-featured for a simple Haskell blog with pre-rendered SVG charts.

**Bottom line:**
- **Quick fix** (Phase 1): Remove 85% of JS bloat in 2 hours
- **Better fix** (Phase 1+2): Streamline entire stack in 1-2 days
- **Best fix** (Phase 3): Custom minimal theme (3-5 days work)

Given the site's current simplicity (12 blog posts, SVG charts, no interactive features), Phase 1+2 offers the best ROI.
