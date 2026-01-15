# card: action-git-tag ⟜ create annotated version tag

**type** ⟜ action / git

**execution** ⟜ bounded worker / 5s timeout

**input** ⟜ repo directory (must be on main or release branch), version string (e.g., v0.12.0), optional message

**task** ⟜ git tag -a v[version] -m "[message]"; verify tag created

**output** ⟜ ✓/✗ + [tag-name] + [commit-hash]

**idempotence** ⟜ NOT idempotent if tag exists; check and error explicitly

**effects** ⟜ reads: commit hash, writes: git tag (annotated), spawns: none, network: no

---

## Pre-Check

Verify we're in a good state:
```bash
git status --porcelain  # must be empty (no dirty files)
CURRENT=$(git rev-parse HEAD)
echo "Current commit: $CURRENT"
```

---

## Tag Creation

```bash
TAG_NAME="v${version}"
MESSAGE="${message:-Release $TAG_NAME}"

# Check if tag already exists
if git rev-parse --verify "$TAG_NAME" > /dev/null 2>&1; then
  echo "✗ TAG ALREADY EXISTS: $TAG_NAME"
  exit 1
fi

# Create annotated tag
git tag -a "$TAG_NAME" -m "$MESSAGE"
RESULT=$?

if [ $RESULT -eq 0 ]; then
  COMMIT=$(git rev-list -n 1 "$TAG_NAME")
  echo "✓ TAG CREATED: $TAG_NAME"
  echo "Commit: $COMMIT"
else
  echo "✗ TAG CREATION FAILED"
  echo "Return code: $RESULT"
fi
```

---

## Output Format

Success:
```
action-git-tag | [repo] | [tag-name] | ✓ | commit: [hash]
```

Error (tag exists):
```
action-git-tag | [repo] | [tag-name] | ✗ | already-exists
```

Error (working tree dirty):
```
action-git-tag | [repo] | [tag-name] | ✗ | working-tree-dirty
```

---

## Notes

- Uses annotated tags (`-a`) not lightweight tags; annotated tags are recommended for releases
- Tag message should be brief but meaningful
- Tag naming: `v[major].[minor].[patch]` (e.g., `v0.12.0`, `v1.0.0-rc1`)
- Fails explicitly if tag already exists; does not overwrite
- Tag creation is local; must push explicitly if publishing to remote

Example message:
```
"Release v0.12.0: Upgrade to GHC 9.14, bounds refresh"
```

