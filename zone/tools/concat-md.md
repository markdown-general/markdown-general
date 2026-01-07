# concat-md

Concatenate markdown files from a directory into a single file with reversibility.

Flattens directory structure for processing while preserving the ability to split back into original structure.

## what it does

Takes a list of markdown files, concatenates them with HTML comment delimiters, outputs to specified location.

Reverses the process: reads concatenated file, extracts individual files back to original paths.

## why html comments

**Invisible delimiters** ⟜ render cleanly in markdown viewers
**Unambiguous parsing** ⟜ `<!-- FILE: path -->` markers are unique
**No escaping needed** ⟜ markdown content can contain anything
**Preserves structure** ⟜ full relative paths in delimiters
**Simple boundaries** ⟜ next FILE marker or EOF ends content

## code

```python
#!/usr/bin/env python3
"""
Concatenate and split markdown files with reversibility.
Uses HTML comment delimiters to preserve directory structure.
"""

import argparse
from pathlib import Path
import sys

def concat_files(file_list: Path, output: Path, base_dir: Path = Path(".")):
    """
    Concatenate markdown files into single file with delimiters.
    
    Args:
        file_list: Path to file containing ordered list of files (one per line)
        output: Path for concatenated output
        base_dir: Base directory for relative paths (default: current dir)
    """
    
    if not file_list.exists():
        raise FileNotFoundError(f"File list not found: {file_list}")
    
    # Read file list
    files = []
    with open(file_list) as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith('#'):  # Skip empty and comments
                files.append(Path(line))
    
    if not files:
        raise ValueError(f"No files found in {file_list}")
    
    print(f"Concatenating {len(files)} files...")
    
    # Create output directory if needed
    output.parent.mkdir(parents=True, exist_ok=True)
    
    # Concatenate
    with open(output, 'w') as out:
        for filepath in files:
            full_path = base_dir / filepath
            
            if not full_path.exists():
                print(f"Warning: {full_path} not found, skipping")
                continue
            
            # Get relative path from base_dir
            try:
                rel_path = full_path.relative_to(base_dir)
            except ValueError:
                rel_path = filepath
            
            # Write delimiter
            out.write(f"<!-- FILE: {rel_path} -->\n")
            
            # Write file content
            with open(full_path) as f:
                lines = f.readlines()
                
                # Strip existing FILE delimiters (from previous concat runs)
                filtered_lines = []
                for line in lines:
                    if not (line.startswith("<!-- FILE: ") and line.rstrip().endswith(" -->")):
                        filtered_lines.append(line)
                
                content = ''.join(filtered_lines)
                # Ensure single trailing newline
                content = content.rstrip('\n') + '\n'
                out.write(content)
    
    print(f"Created: {output}")
    return output


def split_file(concat_file: Path, output_dir: Path = Path(".")):
    """
    Split concatenated file back into individual files.
    
    Args:
        concat_file: Path to concatenated file
        output_dir: Base directory for extracted files (default: current dir)
    """
    
    if not concat_file.exists():
        raise FileNotFoundError(f"Concatenated file not found: {concat_file}")
    
    print(f"Splitting {concat_file.name}...")
    
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
        description="Concatenate or split markdown files with reversibility"
    )
    subparsers = parser.add_subparsers(dest='command', help='Command to run')
    
    # Concat subcommand
    concat_parser = subparsers.add_parser('concat', help='Concatenate files')
    concat_parser.add_argument(
        'file_list',
        type=Path,
        help='File containing list of markdown files (one per line)'
    )
    concat_parser.add_argument(
        'output',
        type=Path,
        help='Output path for concatenated file'
    )
    concat_parser.add_argument(
        '--base-dir',
        type=Path,
        default=Path('.'),
        help='Base directory for relative paths (default: current dir)'
    )
    
    # Split subcommand
    split_parser = subparsers.add_parser('split', help='Split concatenated file')
    split_parser.add_argument(
        'concat_file',
        type=Path,
        help='Concatenated file to split'
    )
    split_parser.add_argument(
        '--output-dir',
        type=Path,
        default=Path('.'),
        help='Output directory for extracted files (default: current dir)'
    )
    
    args = parser.parse_args()
    
    if args.command == 'concat':
        concat_files(args.file_list, args.output, args.base_dir)
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
# Define helper functions
concat-md() {
  sed -n '/^```python$/,/^```$/p' ~/markdown-general/work/concat-md.md | sed '1d;$d' | python - "$@"
}

# Concatenate files
concat-md concat files.txt output.md --base-dir content/

# Split concatenated file
concat-md split merged.md --output-dir restored/
```

### create file list

Order matters - list files by importance:

```bash
# Create ordered list
cat > files.txt << 'EOF'
work/cache.md
work/defunctionalization.md
work/sisyphus.md
work/coding.md
work/deck.md
EOF
```

Or generate programmatically:

```bash
# List all markdown files recursively
find content/ -name "*.md" > files.txt

# Then manually reorder by importance
```

## api

### concat command

**Inputs:**
- `file_list` - path to file containing ordered list of markdown files (one per line)
- `output` - path for concatenated output file
- `--base-dir` - base directory for relative paths (default: current directory)

**Outputs:**
- Single markdown file with HTML comment delimiters
- Each file's content preserved with `<!-- FILE: relative/path.md -->` marker

**File list format:**
```
work/cache.md
work/coding.md
# Comments starting with # are ignored
work/deck.md
```

### split command

**Inputs:**
- `concat_file` - path to concatenated markdown file
- `--output-dir` - directory for extracted files (default: current directory)

**Outputs:**
- Individual markdown files restored to original paths
- Directory structure created as needed
- Single trailing newline preserved per POSIX

## examples

### flatten work/

```bash
# Create ordered file list
cat > base-files.txt << 'EOF'
work/sisyphus.md
work/cache.md
work/defunctionalization.md
work/coding.md
work/deck.md
work/lattice.md
EOF

# Concatenate
concat-md concat base-files.txt /tmp/base-flattened.md --base-dir .

# Result: /tmp/base-flattened.md contains all files with delimiters
```

### split back to original structure

```bash
# Split concatenated file
concat-md split /tmp/base-flattened.md --output-dir restored/

# Verify round-trip
diff -r work/ restored/work/
```

### recursive directory concat

```bash
# Generate file list for entire content/ tree
find content/ -name "*.md" | sort > all-content.txt

# Manually reorder by importance in editor
vim all-content.txt

# Concatenate
concat-md concat all-content.txt /tmp/all-content.md
```

## tests

### round-trip identity

```bash
# Create test structure
mkdir -p test/a/b
echo "# File A" > test/a/file.md
echo "# File B" > test/a/b/file.md

# List files
cat > test-files.txt << 'EOF'
test/a/file.md
test/a/b/file.md
EOF

# Concat
concat-md concat test-files.txt test-concat.md

# Split
concat-md split test-concat.md --output-dir restored/

# Verify
diff -r test/ restored/test/
```

Expected: no differences

### delimiter edge cases

```bash
# Create file with HTML comments already present
cat > test-edge.md << 'EOF'
# Test

<!-- FILE: something.md -->
This should be overwritten.

More content.
EOF

# This file will have its existing delimiter overwritten during concat
```

Expected: tool overwrites existing `<!-- FILE: -->` comments

### empty files

```bash
# Create empty file (bug per sisyphus conventions)
touch test-empty.md

# List it
echo "test-empty.md" > files.txt

# Concat
concat-md concat files.txt output.md
```

Expected: delimiter written, only trailing newline for content

### trailing newline normalization

```bash
# File with no trailing newline
printf "# Test\nNo newline" > no-newline.md

# File with multiple trailing newlines
printf "# Test\n\n\n" > multi-newline.md

# Concat normalizes both to single trailing newline
```

Expected: both files get exactly one trailing newline in output

## upstream problems

**Order specification** ⟜ manual file list creation is tedious
- Could add auto-ordering heuristics (alphabetical, by size, by date)
- But "importance" is semantic, requires human judgment

**Existing delimiter handling** ⟜ currently overwrites without warning
- Could detect and warn user
- But assumption is delimiters only exist from previous runs

**Empty file handling** ⟜ treated as bugs but processed anyway
- Could error on empty files
- But silent processing is more robust

Added to `org/`: decide if auto-ordering heuristics are worth the complexity
