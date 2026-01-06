# tools/cache.md

**cache** ⟜ create reversible markdown caches with smart defaults

**what it does** ⟜ concatenate files with HTML comment delimiters for reversible flattening

**why** ⟜ token-efficient handoff structures between chat, agentics, and humans

## code

```python
#!/usr/bin/env python3
"""
Create reversible markdown caches with pattern-based file selection.

Uses HTML comment delimiters for reversible concatenation.
"""

import argparse
from pathlib import Path
import glob
import fnmatch
from datetime import datetime
import sys

def find_files(patterns, base_dir, exclude_patterns=None, recursive=True):
    """Find files matching include patterns, excluding exclude patterns."""
    if exclude_patterns is None:
        exclude_patterns = []
    
    matched_files = set()
    
    for pattern in patterns:
        # Convert glob pattern to find-compatible pattern
        find_pattern = pattern.replace("**", "*")
        
        if recursive:
            # Use glob for recursive patterns
            for file_path in glob.glob(str(base_dir / pattern), recursive=True):
                file_path = Path(file_path)
                if file_path.is_file():
                    # Check exclude patterns
                    if not any(fnmatch.fnmatch(str(file_path), str(base_dir / excl)) 
                             for excl in exclude_patterns):
                        matched_files.add(file_path)
        else:
            # Non-recursive - only immediate directory
            if "**" in pattern:
                pattern = pattern.replace("**", "*")
            
            for file_path in glob.glob(str(base_dir / pattern)):
                file_path = Path(file_path)
                if file_path.is_file() and file_path.parent == base_dir:
                    if not any(fnmatch.fnmatch(str(file_path), str(base_dir / excl)) 
                             for excl in exclude_patterns):
                        matched_files.add(file_path)
    
    # Sort for consistent ordering
    return sorted(matched_files)

def apply_smart_defaults(options):
    """Apply smart defaults when options are not explicitly set."""
    if not options.dir:
        options.dir = Path.cwd()
    
    if not options.include:
        options.include = ["**/*"]
    
    if not options.exclude:
        # Common cache exclusions
        options.exclude = [
            "cache-*.md",
            "artifacts/**",
            "content/self/**", 
            "content/blog/**",
            "content/upstream/**",
            "org/**",
            "intake/**"
        ]
    
    if not options.cache_dir:
        options.cache_dir = Path.home() / "sisyphus-cache"
    
    if not options.output:
        options.output = generate_auto_name(options.dir, options.cache_dir)
    
    return options

def generate_auto_name(dir_path, cache_dir):
    """Generate auto filename based on directory name and date."""
    dir_name = dir_path.name if dir_path != Path.cwd() else "current"
    date_suffix = datetime.now().strftime("%y%m%d")
    return cache_dir / f"cache-{dir_name}-{date_suffix}.md"

def concat_files(files, output, base_dir):
    """Concatenate files with HTML comment delimiters."""
    print(f"Concatenating {len(files)} files...")
    
    # Create output directory if needed
    output.parent.mkdir(parents=True, exist_ok=True)
    
    with open(output, 'w') as out_file:
        for file_path in files:
            try:
                # Get relative path from base_dir
                try:
                    rel_path = file_path.relative_to(base_dir)
                except ValueError:
                    rel_path = file_path
                
                # Write delimiter
                out_file.write(f"<!-- FILE: {rel_path} -->\n")
                
                # Read and clean file content
                with open(file_path, 'r') as in_file:
                    lines = in_file.readlines()
                    
                    # Strip existing FILE delimiters
                    filtered_lines = []
                    for line in lines:
                        if not (line.startswith("<!-- FILE: ") and line.rstrip().endswith(" -->")):
                            filtered_lines.append(line)
                    
                    # Ensure single trailing newline
                    content = ''.join(filtered_lines)
                    content = content.rstrip('\n') + '\n'
                    out_file.write(content)
                    
            except Exception as e:
                print(f"Warning: Could not process {file_path}: {e}")
                continue
    
    print(f"Created: {output}")
    return output

def split_file(concat_file, output_dir):
    """Split concatenated file back to individual files."""
    print(f"Splitting {concat_file.name}...")
    
    if not concat_file.exists():
        raise FileNotFoundError(f"Concatenated file not found: {concat_file}")
    
    current_file = None
    current_path = None
    content_lines = []
    files_created = []
    
    with open(concat_file) as f:
        for line in f:
            # Check for file delimiter
            if line.startswith("<!-- FILE: ") and line.rstrip().endswith(" -->"):
                # Save previous file if exists
                if current_file is not None:
                    current_file.parent.mkdir(parents=True, exist_ok=True)
                    with open(current_file, 'w') as out:
                        out.write(''.join(content_lines))
                    files_created.append(current_file)
                
                # Extract new path
                path_str = line[11:-4].strip()  # Remove <!-- FILE: and -->
                current_path = path_str
                current_file = output_dir / path_str
                content_lines = []
            else:
                # Accumulate content
                if current_file is not None:
                    content_lines.append(line)
        
        # Save last file
        if current_file is not None:
            current_file.parent.mkdir(parents=True, exist_ok=True)
            with open(current_file, 'w') as out:
                out.write(''.join(content_lines))
            files_created.append(current_file)
    
    print(f"Extracted {len(files_created)} files to: {output_dir}/")
    for f in files_created:
        print(f"  {f.relative_to(output_dir)}")
    
    return files_created

def main():
    parser = argparse.ArgumentParser(
        description="Create or split markdown caches with pattern-based file selection"
    )
    subparsers = parser.add_subparsers(dest='command', help='Command to run')
    
    # Flatten subcommand
    flatten_parser = subparsers.add_parser('flatten', help='Create cache from file patterns')
    flatten_parser.add_argument(
        '--include', 
        nargs='*', 
        default=[],
        help='File patterns to include (default: "**/*")'
    )
    flatten_parser.add_argument(
        '--exclude', 
        nargs='*', 
        default=[],
        help='File patterns to exclude'
    )
    flatten_parser.add_argument(
        '--dir', 
        type=Path, 
        help='Base directory for pattern matching (default: current)'
    )
    flatten_parser.add_argument(
        '--cache-dir', 
        type=Path, 
        help='Cache directory (default: $HOME/sisyphus-cache/)'
    )
    flatten_parser.add_argument(
        '--output', 
        type=Path, 
        help='Output file (default: auto-generated)'
    )
    flatten_parser.add_argument(
        '--recursive', 
        action='store_true', 
        default=True,
        help='Include files in subdirectories (default: true)'
    )
    flatten_parser.add_argument(
        '--nonrecursive', 
        action='store_true', 
        help='Don\'t include subdirectories'
    )
    flatten_parser.add_argument(
        '--dry-run', 
        action='store_true', 
        help='Show what would be included without creating output'
    )
    
    # Split subcommand
    split_parser = subparsers.add_parser('split', help='Split cache back to individual files')
    split_parser.add_argument(
        'concat_file', 
        type=Path, 
        help='Concatenated file to split'
    )
    split_parser.add_argument(
        '--output-dir', 
        type=Path, 
        default=Path.cwd(), 
        help='Output directory for extracted files (default: current dir)'
    )
    
    args = parser.parse_args()
    
    if args.command == 'flatten':
        # Apply smart defaults
        options = args
        options = apply_smart_defaults(options)
        
        if options.nonrecursive:
            options.recursive = False
        
        # Find files
        files = find_files(options.include, options.dir, options.exclude, options.recursive)
        
        if not files:
            print("No files found matching the specified patterns")
            return 1
        
        # Dry run or execute
        if args.dry_run:
            print("Would include these files:")
            for f in files:
                print(f"  {f.relative_to(options.dir)}")
            print(f"\nWould create: {options.output}")
        else:
            concat_files(files, options.output, options.dir)
    
    elif args.command == 'split':
        split_file(args.concat_file, args.output_dir)
    
    else:
        parser.print_help()
        return 1
    
    return 0

if __name__ == "__main__":
    exit(main())
```

## run

### extract and use

```bash
# Define helper function
cache() {
  sed -n '/^```python$/,/^```$/p' ~/sisyphus/tools/cache.md | sed '1d;$d' | python - "$@"
}

# Create cache with smart defaults
cache flatten

# Create cache with explicit patterns
cache flatten --include "**/*.md" --exclude "test/**" --output my-cache.md

# Split cache back to individual files
cache split my-cache.md --output-dir restored/

# Dry run to see what would be included
cache flatten --dry-run
```

## api

### flatten command

**Smart Defaults:**
- `--include "**/*"` (all files)
- `--exclude "cache-*.md" "artifacts/**" "content/self/**" "content/blog/**" "content/upstream/**" "org/**" "intake/**"`
- `--dir .` (current directory)
- `--recursive` (default true)
- `--cache-dir ~/sisyphus-cache/`
- `--output` auto-generated as `cache-<dir>-YYMMDD.md`

**Options:**
- `--include PATTERN [PATTERN ...]` - File patterns to include
- `--exclude PATTERN [PATTERN ...]` - File patterns to exclude
- `--dir DIR` - Base directory for pattern matching
- `--cache-dir DIR` - Cache directory (default: ~/sisyphus-cache/)
- `--output FILE` - Output file (default: auto-generated)
- `--recursive` - Include files in subdirectories (default: true)
- `--nonrecursive` - Don't include subdirectories
- `--dry-run` - Show what would be included without creating output

### split command

**Inputs:**
- `concat_file` - Path to concatenated markdown file
- `--output-dir DIR` - Directory for extracted files (default: current directory)

**Outputs:**
- Individual markdown files restored to original paths
- Directory structure created as needed
- Single trailing newline preserved per POSIX

## examples

### Create content cache

```bash
# From content/ directory - simplest usage
cd ~/sisyphus/content
cache flatten
# Creates: ~/sisyphus-cache/cache-content-200106.md
```

### Create tools cache

```bash
# From tools directory
cd ~/sisyphus/tools  
cache flatten
# Creates: ~/sisyphus-cache/cache-tools-200106.md
```

### Create specialist cache

```bash
# Haskell development cache
cache flatten \
  --include "**/*.hs" \
  --include "**/*.cabal" \
  --include "content/base/haskell.md" \
  --exclude "test/**" \
  --exclude "temp/**" \
  --output cache-haskell-specialist.md
```

### Verify round-trip

```bash
# Create cache
cache flatten --output test-cache.md

# Split it back
cache split test-cache.md --output-dir restored/

# Compare (should be identical)
diff -r . restored/
```

## tests

### round-trip identity

```bash
# Create test structure
mkdir -p test/a/b
echo "# File A" > test/a/file.md
echo "# File B" > test/a/b/file.md

# Create cache
cache flatten --dir test --output test-cache.md

# Split
cache split test-cache.md --output-dir restored/

# Verify (should show no differences)
diff -r test/ restored/test/
```

### html comment edge cases

```bash
# File with existing HTML comments (should be preserved in content, overwritten in delimiter)
cat > test-edge.md << 'EOF'
# Test Document

<!-- This is a comment, not a delimiter -->
Some content here.

More content.
<!-- FILE: something.md -->
This looks like a delimiter but isn't.
EOF

# Create cache including this file
cache flatten --include "test-edge.md" --output edge-cache.md

# Split and verify the content is preserved correctly
cache split edge-cache.md --output-dir restored/
cat restored/test-edge.md
```

### header edge cases

```bash
# File with YAML frontmatter and headers
cat > test-yaml.md << 'EOF'


---


title: Test Document
author: Test Author


---


# Header 1

Some content.

## Header 2

More content.
EOF

# Test that YAML and headers are preserved
cache flatten --include "test-yaml.md" --output yaml-cache.md
cache split yaml-cache.md --output-dir restored/
diff test-yaml.md restored/test-yaml.md
```

### card creation pattern test

```bash
# Test the pattern used for new cards
echo "## content/base/newcard.md" > test-card.md

# Include in cache and verify it's treated as content
cache flatten --include "test-card.md" --output card-cache.md
cache split card-cache.md --output-dir restored/
cat restored/test-card.md
```

## relations

**python.md** ⟜ Python card development and deployment guide (includes sed extraction pattern used here)
**card.md** ⟜ general card structure and concepts

## status

**Installation Cost:**
  - Tokens: unknown
  - Cache: 0 read, 0 created
  - Time: 7s
  - Model: claude-haiku-4-5-20251001
  - Installed: 2026-01-05 21:59 UTC

**Installation Cost:**
  - Tokens: unknown
  - Cache: 0 read, 0 created
  - Time: 7s
  - Model: claude-haiku-4-5-20251001
  - Installed: 2026-01-05 21:59 UTC

**Tests:** ✓ round-trip identity, HTML comment edge cases, YAML/header preservation, card creation pattern
**Last updated:** 2026-01-06
