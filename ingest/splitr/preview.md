# Repository Maintenance

**Check repos, commit configs, generate status snapshot**

> we mash

---

## What This Does

Scans all git repositories for uncommitted changes and current branches. Auto-commits configuration repos. Generates status snapshot showing work-in-progress across repos.

This is a morning ritual - helps you resume yesterday's context.

---

## Commands

### Check All Repos

```bash
cd ~/repos
find . -maxdepth 1 -mindepth 1 -type d -exec sh -c '(echo {} && cd {} && git status -s && echo)' \;
```

### Check Branches

```bash
cd ~/repos
find . -maxdepth 1 -mindepth 1 -type d -exec sh -c '(echo {} && cd {} && git rev-parse --abbrev-ref HEAD && echo)' \;
```

---

## Auto-Commit Configs

### Doom Emacs
```bash
cd ~/.config/doom && git status -s
git commit -a -m "regular commit" && git push
```

### Dotfiles
```bash
cd ~/repos/dotfiles && git status -s
git commit -a -m "regular commit" && git push
```

### Org Files
```bash
cd ~/mash/org && git status -s
git add * && git commit -a -m "regular commit" && git push
```

**Note:** org uses `git add *` to catch new files

---

## Check Specific Repos

### Site
```bash
cd ~/site && git status -s
cd ~/site/public && git status -s
```



---

## Generate Status Summary

Create snapshot of current work-in-progress:

**Include:**
- Repos with uncommitted changes
- Repos on feature branches
- Brief description of work stream
- Cross-repo dependencies

**Example format:**
```markdown
## Repo Status - 2025-12-24

1. chart-svg:pie - Pie chart implementation + bug45
2. chart-svg-dev - Contains animate, bug45, pie work
3. prettychart - Animate feature
4. unsafesnatsfix - Harpie bug #1

Config repos committed and pushed.
```

---

## For AI Executing This

1. Run commands and capture output
2. Auto-commit doom, dotfiles, org if changes exist
3. Flag project repos with changes for review
4. Generate status summary showing:
   - What was found
   - What was committed
   - What needs attention
   - Current work-in-progress

---

## Notes

### For Execution (Agent Decision Logic)
- **Read-mostly**: Only auto-commit specified config repos
- **Safety**: When unsure, flag for human review
- **Standard commits**: Generic message for configs only
- **New files**: `git add *` for org, `git commit -a` elsewhere

### For Interpretation (Pattern Recognition)
- **Multi-repo activity**: chart-svg ecosystem changes often related
- **Branch awareness**: Feature branches = active experiments
- **Config changes**: Usually follow tool updates
- **Org frequency**: Daily notes = active planning

### For Documentation (Status Summary Guidance)
- **Connect dots**: Multiple repo changes = unified work stream
- **Capture branches**: Note feature branches and their purpose
- **Flag blockers**: Conflicts or unusual states
- **Timeline**: Morning = resume yesterday; weekly = broader snapshot

### For Future Reference (Human Context)
- Create breadcrumbs for context-switching
- Help future-me understand past-me's focus
- Snapshot clarity over completeness
- Success = confidence in current state

---

**This is a morning ritual. Run it, see what's uncommitted, auto-commit configs, know where you are.**

