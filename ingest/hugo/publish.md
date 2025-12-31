#!/bin/bash
# Executable markdown for Hugo blog publication
# Run with: ./publish.md

sed -n '/^# SCRIPT_START/,/^# SCRIPT_END/p' "$0" | grep -v "^# SCRIPT" | bash -s "$0"
exit $?

# SCRIPT_START

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

MARKDOWN_BASE="$HOME/mash/base"
MARKDOWN_ARTIFACTS="$HOME/mash/artifacts"
HUGO_POSTS="$HOME/site/content/posts"
HUGO_ARTIFACTS="$HOME/site/static/artifacts"
SCRIPT_FILE="$1"

echo "========================================="
echo -e "${BLUE}Hugo Blog Publication${NC}"
echo "========================================="
echo ""

mkdir -p "$HUGO_ARTIFACTS"

echo -e "${BLUE}Reading publication lists from $SCRIPT_FILE${NC}"
posts=$(sed -n '/^## Posts to Publish$/,/^## /p' "$SCRIPT_FILE" | grep '^- ' | sed 's/^- //')
artifacts=$(sed -n '/^## Artifacts to Publish$/,/^## /p' "$SCRIPT_FILE" | grep '^- ' | sed 's/^- //')

post_count=$(echo "$posts" | grep -v '^$' | wc -l | tr -d ' ')
artifact_count=$(echo "$artifacts" | grep -v '^$' | wc -l | tr -d ' ')

echo "Posts to publish: $post_count"
echo "Artifacts to publish: $artifact_count"
echo ""

# Publish posts
if [ "$post_count" -gt 0 ]; then
  echo -e "${BLUE}Publishing posts...${NC}"
  published=0
  skipped=0

  while IFS= read -r post; do
    [ -z "$post" ] && continue

    if [ -f "$MARKDOWN_BASE/$post" ]; then
      echo -e "  ${GREEN}✓${NC} $post"
      cp "$MARKDOWN_BASE/$post" "$HUGO_POSTS/"
      ((published++))
    else
      echo -e "  ${RED}✗${NC} $post ${YELLOW}(not found)${NC}"
      ((skipped++))
    fi
  done <<< "$posts"

  echo ""
  echo -e "${GREEN}Posts published: $published${NC}"
  [ $skipped -gt 0 ] && echo -e "${RED}Posts skipped: $skipped${NC}"
  echo ""
else
  echo -e "${YELLOW}No posts to publish${NC}"
  echo ""
fi

# Publish artifacts
if [ "$artifact_count" -gt 0 ]; then
  echo -e "${BLUE}Publishing artifacts...${NC}"
  published=0
  skipped=0

  while IFS= read -r artifact; do
    [ -z "$artifact" ] && continue

    if [ -f "$MARKDOWN_ARTIFACTS/$artifact" ]; then
      echo -e "  ${GREEN}✓${NC} $artifact"
      cp "$MARKDOWN_ARTIFACTS/$artifact" "$HUGO_ARTIFACTS/"
      ((published++))
    else
      echo -e "  ${RED}✗${NC} $artifact ${YELLOW}(not found)${NC}"
      ((skipped++))
    fi
  done <<< "$artifacts"

  echo ""
  echo -e "${GREEN}Artifacts published: $published${NC}"
  [ $skipped -gt 0 ] && echo -e "${RED}Artifacts skipped: $skipped${NC}"
  echo ""
else
  echo -e "${YELLOW}No artifacts to publish${NC}"
  echo ""
fi

# Deploy infrastructure
echo -e "${BLUE}Deploying site infrastructure...${NC}"
INFRA_SOURCE="$MARKDOWN_ARTIFACTS/site-infrastructure"

if [ -f "$INFRA_SOURCE/config/hugo.toml" ]; then
  mkdir -p "$HOME/site/config/_default"
  cp "$INFRA_SOURCE/config/hugo.toml" "$HOME/site/config/_default/"
  echo -e "  ${GREEN}✓${NC} hugo.toml"
fi

if [ -f "$INFRA_SOURCE/css/custom.css" ]; then
  mkdir -p "$HOME/site/assets/css"
  cp "$INFRA_SOURCE/css/custom.css" "$HOME/site/assets/css/"
  echo -e "  ${GREEN}✓${NC} custom.css"
fi

if [ -d "$INFRA_SOURCE/favicons" ]; then
  mkdir -p "$HOME/site/static"
  cp "$INFRA_SOURCE/favicons"/* "$HOME/site/static/"
  echo -e "  ${GREEN}✓${NC} favicons"
fi

if [ -d "$INFRA_SOURCE/layouts/partials" ]; then
  mkdir -p "$HOME/site/layouts/partials"
  cp "$INFRA_SOURCE/layouts/partials"/* "$HOME/site/layouts/partials/"
  echo -e "  ${GREEN}✓${NC} layout overrides"
fi

echo ""
echo "========================================="
echo -e "${GREEN}Publication complete!${NC}"
echo "========================================="
echo ""
echo "Published to:"
echo "  Posts:          $HUGO_POSTS"
echo "  Artifacts:      $HUGO_ARTIFACTS"
echo "  Infrastructure: $HOME/site/"
echo ""
echo "Next steps:"
echo "  hugo server --buildDrafts  # Preview"
echo "  hugo                       # Build"
echo "  cd public && git add -A && git commit -m 'Publish' && git push"
echo ""

# SCRIPT_END

---

# Hugo Blog Publication

**Executable markdown - both script and documentation**

> we mash

---

## What This Is

Literate programming applied to deployment. The bash script above extracts publication lists from this file and copies selected content to ~/site/.

**This is one file:**
- Executable script
- Publication lists (below)
- Complete documentation

---

## Posts to Publish

Add posts to publish (uncomment or add lines):

- attack.md
- learning.md
- lowercase.md

**Archived (unpublished 2025-12-23):**
<!-- active.md -->
<!-- coderclimate.md -->
<!-- diverse.md -->

---

## Artifacts to Publish

Add artifacts needed by published posts:

<!-- List artifacts here as needed -->
<!-- - diagram.svg -->
<!-- - chart.png -->

---

## How It Works

**Lists embedded as markdown sections** - the script extracts lines starting with `- ` from the sections above.

**Infrastructure deployed automatically** - config, CSS, favicons, layouts always copied.

**Run it:**
```bash
cd ~/mash/base
./publish.md
```

---

## Workflow

1. **Author** in ~/mash/ (anywhere)
2. **Preview** via `hugo server --buildDrafts`
3. **List** in this file (add to sections above)
4. **Run** `./publish.md`
5. **Deploy** `hugo && cd public && git add -A && git commit && git push`

See publication-workflow.md for details.

---

## What Gets Published

**From lists:**
- Selected posts → ~/site/content/posts/
- Selected artifacts → ~/site/static/artifacts/

**Always:**
- Infrastructure → ~/site/ (config, CSS, layouts)

**Preview vs Production:**
- Preview: All content visible via Hugo mounts
- Production: Only listed content

---

## Literate Programming Recovery

This pattern recovers org-babel functionality lost in migration to markdown:

**Traditional approach:**
- publish.sh (script)
- publish-list.txt (data)
- README.md (docs)

**This approach:**
- All three unified
- Can't get out of sync
- Self-documenting
- Version controlled

This is "we mash" - code, docs, data as one.

---

**Make your changes to the lists above, then run ./publish.md**
