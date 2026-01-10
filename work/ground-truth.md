## ground truth

markdown-general ⟜ canonical source
self ⟜ dependent workspace

### the symlink relationship

```
self/work   → symlink to markdown-general/work
self/zone   → symlink to markdown-general/zone
self/artifacts/ → real directory (independent)
```

When you're in ~/self/ and you access work/ or zone/, you're actually reading from and writing to markdown-general. The symlinks mean there's no duplication, no drift, no version mismatch.

### why this matters

markdown-general is changing rapidly and is the ground truth. It's the operating system that self depends on. Major pattern work, tool development, and structural decisions happen there first.

self is a subordinate workspace that layers on top of markdown-general's infrastructure. It maintains its own independent directories (artifacts, blog, self, intake) but delegates work/ and zone/ to the canonical source.

### what you can do

**In self:**
- Read from work/ and zone/ freely (you're reading markdown-general)
- Execute, reference, and build on work/ and zone/ content
- Make modifications to work/ and zone/ if needed (they flow back to markdown-general)
- Maintain independent directories: artifacts/, blog/, self/, intake/
- Use the full structure as a workspace

**For major changes:**
- If you're substantially reworking something in work/ or zone/, recognize you're working in the ground truth
- Consider whether changes should propagate to all users of markdown-general
- Commit changes - they affect the canonical source

### permissions

self/ needs:
- Read access to markdown-general/ (via the symlinks)
- Full read/write access to its independent directories
- Ability to modify work/ and zone/ (since they're writable via symlinks)

This means changes made in ~/self/work/ or ~/self/zone/ are actually changes to ~/markdown-general/work/ and ~/markdown-general/zone/.

### the constraint

You cannot break the symlinks or create a forked version of work/ and zone/ in self/. That would create drift and conflict with the ground truth.

The symlinks are the architecture. They're not a workaround—they're the design.
