# cache profiles

Caches are handoff structures between chat, agentics, and humans. Learn by example rather than specification.

## cache-260105.md

Created: 2026-01-05
Location: ~/markdown-general-cache/cache-260105.md
Files: 92 markdown files, 9,715 lines
Cost: $0.23

**What**: Complete markdown-general/ snapshot for tidy-up.

**Included**: All .md files in markdown-general/

**Excluded**:
- artifacts/
- content/self/
- content/blog/
- cache-*.md files

**Non-markdown files excluded from included directories**:
- ./.claude/settings.local.json
- ./.gitignore
- ./.projectile
- ./zone/tools/.claude/settings.local.json
- ./zone/tools/discover-executables.hs
- ./crypto_poller.py
- ./org/refile.org

**Method**: Simple bash script with find + exclusions, wrapped content with `<!-- FILE: path -->` markers.

**Tips for next time**:
- Trust simple bash over complex tooling for one-offs
- Verify file count matches expectations
- Cache represents what you're handing off - scope by use case not theory
- Named by date (YYMMDD) for temporal organization

## priority-zero (content/ only)

Created: 2026-01-05
Location: ~/markdown-general-cache/cache-priority-zero.md
Files: 35 markdown files, 1,894 lines

**What**: Clean content cache excluding experiments.

**Included**: content/ directory only (work/ + zone/upgrades/)

**Excluded**:
- tools/
- upstream/
- artifacts/
- self/
- blog/

**Use**: Default handoff cache with just curated content, no tools or experiments.

