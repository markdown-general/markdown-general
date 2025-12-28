# mash/

one must imagine mash/ happy;
here we mash

## flow
- ingest/ is a staging area for stuff that is to be turned into markdown suitable for content/. This process digests material slowly - it may split into parts, iterate through stages (ingest/a/ -> ingest/b/), but will eventually be consumed and disappear.
- content/ is a space containing curated, condensed content available for upstream, or used as context for mashing
- artifacts/ is a space for curated non-text material (binary, images, svg)
- org/ operational todos and holes in content weaves. A system where problems are sent "up times arrow" for future resolution.

### Conversion
- pandoc --from=org --to=gfm input.org -o output.md
- Use GFM format for GitHub compatibility

## Content

### Structure
- content/ markdown
- ingest/ change material, projects, knowledge
- org/ operations
- artifacts/ artifacts referenced by markdown/

### Access
- content/base/ immediate
- content/self specified
- content specified
- ingest/ specified
- org/ logistics
- artifacts/ logistics

## Structure

### content/self & artifacts/self

Personal content. Not included in repository

### content/base/

General, reusable content neutral to knowledge origin:
- **Generative**: source material for informed building of content/
- **reference**: frequently referenced by AI
- **flow**: understanding of the flow
- **curated**: dense, high-quality
- **kit**: toolkit for mashing
  
### artifacts/
- Supporting files referenced from markdown content
- Images, PDFs, code files, data files
- Cross-referenced from markdown

### org/
- todo mangement

### Tools

We will at least try to mash documentation testing code and execution into a markdown file, and they will compete to enter the arena that is base.

### Writing

- write economically
- avoid allcaps
- short commits. one-line. no overthinking.
- no branding. ever.

## Tips and Tricks 

- we ingest/ with our TEETHS!!!
- **cascade mashing**: chaining multiple AI processing steps on the same content.
- stuckedness is to be avoided.
- desire for change generates new ingest/ from old content/
- ingest/ is consumed via processing into content/ (with artifacts/)
- there is no archival requirement
- org/ plans mash/ with todos
- Regular commits of the ~/mash/ repo

