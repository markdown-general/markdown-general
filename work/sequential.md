# work/sequential.md

## sequential processing

markdown-general/ is a **sequential processing system** by design. 

This may be a limitation but is a **stable and expected feature** and we may not explore this for a while.

### Current Implementation

**GitHub PRs** are the current cheap and cheerful solution for system processing:
- One change at a time
- Human review and approval
- Clear audit trail
- Conflict-free collaboration

**Predictable Flow** ⟜ ingest → mash → content pipeline operates in clear sequence
- No race conditions or conflicts
- Each step builds on the previous one
- Clear responsibility and ownership
- Easy debugging and tracing

**Quality Assurance** ⟜ sequential processing enables thorough review
- Each change gets proper attention
- Build on previous successes 
- Learn from each iteration
- Maintain system coherence

**Scalability** ⟜ sequential design scales through specialization
- Different contributors handle different steps
- Parallel ingestion, sequential processing
- Specialized roles emerge naturally
- Complex systems remain manageable

### When to Consider Concurrency and Parallelism

Sequential processing remains the default, but consider parallel approaches when:
- Independent components can be developed separately
- No shared dependencies or conflicts
- Each parallel stream can be sequenced later
- Quality can be maintained across streams

Even in parallel cases, the final integration into the sequential system provides the ultimate quality guarantee.
