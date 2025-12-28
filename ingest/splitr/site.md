---
title: "Hugo Blog Project Summary - ~/site/"
date: 2025-12-23
status: completed
---

# Hugo Blog Project Summary - ~/site/

## Core Configuration
- **Theme:** Congo v2.12.2 (MIT licensed, git submodule on dev branch)
- **Base URL:** https://tonyday567.github.io/
- **Site Title:** "en-code-d patterns"
- **Tagline:** "A blog; mostly Haskell"
- **Author:** Tony Day

## Directory Structure
```
~/site/
├── config/_default/       # 7 configuration files (hugo, params, languages, etc.)
├── content/              # Blog posts and pages
│   ├── posts/           # Main blog content
│   │   ├── Leaf bundles (6): chart-svg-changelog, color-adjust, ellipse,
│   │   │                     gallery, harpie, secondlazy
│   │   └── Single posts (6): attack.md, active.md, learning.md, etc.
│   ├── about.md
│   └── _index.md
├── assets/
│   ├── css/custom.css   # Solarized color scheme, font definitions
│   ├── icons/           # Custom Haskell icon
│   └── image/           # Avatar, logo, banner
├── static/              # Favicons and manifests
├── layouts/partials/    # Custom footer extension
├── themes/congo/        # Full-featured theme with Tailwind CSS
└── public/              # Built output (gitignored)
```

## Styling Approach
- **Framework:** Tailwind CSS v3.4.17 (via Congo theme)
- **Custom Colors:** Solarized color scheme (green #859900 primary, blue #268bd2 secondary)
- **Fonts:** Inconsolata (code), Lora (body), Raleway (headings)
- **Dark Mode:** CSS class-based with user toggle

## Content Profile
- **Format:** Markdown with TOML frontmatter
- **Organization:** Tags taxonomy (chart-svg, harpie, haskell, learning, math, numhask, climate, color)
- **Special Features:** Leaf bundles with embedded assets (SVGs, images), draft posts
- **Focus:** Haskell programming, chart visualization, mathematical content

## Theme Complexity
The Congo theme is feature-rich with:
- Chart.js, KaTeX, Mermaid, Fuse.js integration
- Complex Tailwind configuration
- Multiple JS bundles (2.6MB Mermaid alone)
- Extensive npm dependencies (FontAwesome, typography plugins)

## Customizations
- Custom CSS overriding theme colors
- Custom footer partial ("Made with Haskell & Hugo & congo")
- Custom Haskell icon
- Privacy-focused analytics options (Fathom/Plausible/Umami commented out)

## Overall Directory Structure

The Hugo site at `~/site/` is a well-organized personal blog using the Congo theme. The project follows standard Hugo conventions:

```
~/site/
├── .git/                      # Git repository
├── .gitignore                 # Ignores: /public/, /themes/, .hugo_build.lock
├── .hugo_build.lock          # Hugo build lock file
├── archetypes/               # Content templates
├── assets/                   # Site-specific assets
├── config/                   # Configuration files
├── content/                  # Blog content (markdown)
├── data/                     # Data files (currently empty)
├── i18n/                     # Internationalization (currently empty)
├── layouts/                  # Site-specific layout overrides
├── public/                   # Built output (gitignored)
├── resources/                # Hugo resource cache (_gen/)
├── static/                   # Static files (favicons, manifests)
└── themes/                   # Theme directory
    └── congo/               # Congo theme (git submodule)
```

## Hugo Configuration Files

Located in `/Users/tonyday567/site/config/_default/`:

1. **hugo.toml** - Core Hugo settings:
   - Theme: "congo"
   - Base URL: "https://tonyday567.github.io/"
   - Default language: English
   - Pagination: 10 items per page
   - Outputs: HTML, RSS, JSON
   - Privacy settings for Vimeo, X (Twitter), YouTube with DNT enabled
   - Code fingerprinting: SHA256 algorithm

2. **params.toml** - Theme parameters:
   - Color scheme: "congo" with dark mode as default
   - Features enabled: Search, code copy, image lazy loading, WebP conversion
   - Header: Basic layout with SVG logo
   - Footer: Appearance switcher, scroll-to-top button
   - Article display: Shows date, date updated, reading time, breadcrumbs, taxonomies, sharing links (X, Mastodon, Bluesky)
   - List display: Shows breadcrumbs, summaries, taxonomies
   - Analytics: Fathom, Plausible, Umami (commented out)

3. **languages.en.toml** - Language configuration:
   - Site title: "en-code-d patterns"
   - Author: Tony Day with profile image, headline "A blog; mostly Haskell"
   - Author links: GitHub, Reddit, X Twitter, Bluesky, Haskell Discourse

4. **markup.toml** - Markdown processing:
   - Goldmark renderer with unsafe HTML enabled
   - Syntax highlighting without CSS classes
   - Table of contents: Levels 2-4

5. **menus.en.toml** - Main navigation:
   - Posts, Tags, About, GitHub repo link, RSS feed
   - Integrated search action

6. **taxonomies.toml** - Content organization:
   - Single taxonomy: tags

7. **module.toml** - Hugo requirements:
   - Extended Hugo required (version >= 0.87.0)

## Theme: Congo

**Theme Details:**
- Name: Congo (https://github.com/jpanther/congo)
- Version: 2.12.2
- Location: `/Users/tonyday567/site/themes/congo/`
- Status: Git submodule on `dev` branch
- License: MIT

**Congo Features:**
- Built with Tailwind CSS (v3.4.17)
- Dark mode support via CSS classes
- Responsive design
- Extensive customization options
- Support for charts, KaTeX math, Mermaid diagrams

## Content Structure

Located in `/Users/tonyday567/site/content/`:

**Content Organization:**
- `_index.md` - Home page with "Encoded patterns" tagline
- `about.md` - Author profile page (simple layout, no TOC/metadata shown)
- `posts/` - Blog posts directory with:
  - **Leaf bundles** (directories with `_index.md`):
    - `chart-svg-changelog/` - SVG chart examples with changelog
    - `color-adjust/` - Color-related content
    - `ellipse/` - Math/geometry content
    - `gallery/` - Visual gallery with 25+ SVG examples
    - `harpie/` - Haskell library discussion (draft)
    - `secondlazy/` - Content bundle
  - **Single posts** (markdown files):
    - `attack.md` - "Haskell and the elegant attack"
    - `active.md`
    - `learning.md` - Learning-related content
    - `lowercase.md` - Typography/case-related
    - `diverse.md` - Diversity content
    - `coderclimate.md` - Code/climate related

**Content Frontmatter:**
- Uses TOML format (+++...+++)
- Common fields: title, author, date, lastmod, draft, tags
- Theme-specific: showSummary, showTableOfContents, showTaxonomies, etc.

**Tags Used:** chart-svg, harpie, haskell, learning, math, numhask, climate, color

## Static Assets Organization

Located in `/Users/tonyday567/site/static/`:
- `favicon.ico` - Main favicon
- `favicon-16x16.png`, `favicon-32x32.png` - Favicon variants
- `apple-touch-icon.png` - iOS home screen icon
- `android-chrome-192x192.png`, `android-chrome-512x512.png` - Android icons
- `site.webmanifest` - PWA manifest with theme colors

## Assets Directory

Located in `/Users/tonyday567/site/assets/`:

**CSS:**
- `css/custom.css` - Custom styling:
  - Font definitions: Inconsolata (code), Lora (body), Raleway (headings)
  - Solarized color scheme customization
  - CSS variables for light/dark themes
  - Primary color: Solarized Green (#859900)
  - Secondary color: Solarized Blue (#268bd2)
  - Full light/dark mode color palette

**Icons:**
- `icons/haskell.svg` - Custom Haskell icon

**Images:**
- `image/avatar.png` - Profile avatar
- `image/avatar2.jpg` - Alternative avatar
- `image/logo.svg` - Site logo
- `image/banner.svg` - Banner graphic
- `image/Two_rays_and_one_vertex.png` - Geometric image

## Custom Layouts and Templates

Located in `/Users/tonyday567/site/layouts/`:

- `partials/extend-footer.html` - Custom footer extension:
  - Adds attribution: "Made with Haskell & Hugo & congo"
  - Links to Haskell.org, GoHugo.io, Congo documentation

## CSS/Styling Approach

**Framework:** Tailwind CSS (via Congo theme)

**Styling Features:**
1. **Custom CSS Variables** (in `assets/css/custom.css`):
   - Neutral, primary, secondary color palettes
   - Light mode: Solarized light base (light backgrounds)
   - Dark mode: Solarized dark base (dark backgrounds)
   - Consistent accent colors across modes

2. **Theme Integration:**
   - Congo's tailwind.config.js defines color mappings
   - Typography plugin for prose styling
   - Custom link/kbd/mark element styling
   - Responsive utilities

3. **Build Process:**
   - Tailwind CSS compilation during build
   - Fingerprinting for cache busting
   - CSS output: `main.bundle.min.97019b81c770be34708009f14f1692ac4a3c6b98e13bff9f42f33a327a5991d4.css` (51KB)

## Build Process and Scripts

**Congo Theme Build:**
Located in `/Users/tonyday567/site/themes/congo/`:

**npm Scripts:**
- `npm run build` - Production Tailwind CSS compilation
- `npm run dev` - Development with watch mode
- `npm run assets` - Vendor asset copying
- `npm run example` - Run example site locally

**Dependencies (from package.json):**
- **Dev dependencies:**
  - Tailwind CSS v3.4.17
  - @tailwindcss/typography v0.5.16
  - KaTeX v0.16.22 (math rendering)
  - Mermaid v11.8.1 (diagram support)
  - Chart.js v4.5.0 (charting)
  - Fuse.js v7.1.0 (search)
  - Quicklink v3.0.1 (prefetching)
  - Prettier v3.6.2 (code formatting)
  - FontAwesome v6.7.2

**Asset Vendoring:**
- Mermaid, Chart.js, KaTeX, Fuse.js, Quicklink copied to `assets/lib/`
- Auto-compiled via `vendor-copy` post-install

## Hugo Features Being Used

1. **Markdown Processing:**
   - Goldmark renderer with unsafe HTML
   - Table of contents generation
   - Code syntax highlighting

2. **Content Organization:**
   - Leaf bundles for image galleries
   - Taxonomies (tags)
   - Cascading front matter for defaults

3. **Output Formats:**
   - HTML (main)
   - RSS feed (at `/index.xml`)
   - JSON index (for search)

4. **Asset Pipeline:**
   - Image processing with WebP conversion
   - Resource cache in `resources/_gen/`
   - Asset fingerprinting

5. **Performance Features:**
   - Image lazy loading
   - Minified CSS/JS bundles
   - RSS generation for content distribution

6. **Dark Mode:**
   - Automatic light/dark theme switching
   - CSS class-based dark mode
   - User appearance preference toggle

7. **Templating:**
   - Custom shortcodes ({{</* lead */>}}, {{</* profile */>}}, {{</* figure */>}})
   - Partial overrides for theme customization
   - Layout cascading for flexible content

## Generated Output

The `public/` directory contains the built site with:
- Minified CSS: 51KB (single bundled file)
- JavaScript bundles: Main (34KB), Appearance (1.8KB), Mermaid (2.6MB)
- Paginated HTML archives
- Post directories with individual pages
- Tag-based category pages
- Static image assets with WebP variants

## Key Project Characteristics

- **Purpose:** Personal blog focused on Haskell programming
- **Author:** Tony Day (tonyday567)
- **Theme:** Professional, minimal Congo theme with Solarized colors
- **Content Focus:** Haskell, chart visualization, programming patterns
- **Deployment:** GitHub Pages (tonyday567.github.io)
- **Build:** Hugo static site generation with Tailwind CSS
- **Version Control:** Git-based with theme as submodule

## Potential Simplification Opportunities

1. **Heavy theme dependencies:** 2.6MB+ JS libraries for features you may not use
2. **Complex Tailwind compilation process:** Requires npm build step
3. **Custom CSS duplication:** Overriding theme settings could be simplified
4. **Theme submodule management:** Adds complexity to git workflow
5. **Unused features:** Many Congo features may not be utilized (Chart.js, Mermaid, KaTeX)
6. **Build complexity:** npm dependency chain in theme directory

This is a well-structured, professionally configured Hugo site with extensive customization through custom CSS, theme parameter overrides, and layout partials, all organized for easy maintenance and future expansion.
