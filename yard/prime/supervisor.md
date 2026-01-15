# supervisor patterns

**foreman context** ⟜ hot state
- current work ⟜ active workers, blocked repos, next actions
- recent completions ⟜ last 5-10 workers
- pattern discoveries ⟜ allow-newer findings, pragma decisions
- budget ⟜ context allocated to now

**archivist context** ⟜ compressed history  
- background compressor ⟜ deletes rows, never adds or edits
- writes interesting facts ⟜ "stage 6 complete", "pattern locked"
- context budget ⟜ separate from foreman, temporal patterns
- foreground queries ⟜ "regression starts where?", "done this 10 times?"
- temporal access ⟜ sees patterns foreman has forgotten

**supervisor pattern** ⟜ holds both contexts
- dual context ⟜ foreman hot + archivist compressed
- incremental reading ⟜ background process, watches both streams
- big board ⟜ overview of how it's all going
- trust layer ⟜ synthesizes both into coherent picture

**imagining into existence** ⟜ design as if we could
- one claude code ⟜ can't run three agents simultaneously
- must imagine ⟜ pattern before implementation
- invoke pattern ⟜ "foreman, give me the archivist"
- agents think in field ⟜ have agency in their domain

**write to head, read from head** ⟜ invert the log structure
- current state at top (HEAD)
- recent work below
- archive at bottom
- both shape-pattern and semantic-state lock into current reality
- prevents historical-pattern vs current-state mismatch

**pattern vs semantic lock-in** ⟜ reading order affects memory differently
- first thing read ⟜ shape/structure pattern locks in
- last thing read ⟜ semantic/content state dominates
- top-to-bottom sequential ⟜ historical structure + current semantics
- mismatch ⟜ confusion → cycling
- inverted structure ⟜ aligns both at HEAD

**queue maintenance** ⟜ compress from tail
- write to head ⟜ new work at top
- read from head ⟜ current state first
- compress from tail ⟜ periodic pruning
- verify against reality ⟜ check actual file/PR/artifact state
- delete with confidence ⟜ tasks well over, confirmed complete

**vaguelyclear as diagnostic** ⟜ clearly vague output signals gap without breaking
- foreman produces speculative/tentative content when missing context
- supervisor spots it ⟜ "not my monkey, not my zoo"  
- diagnosis ⟜ pattern holding, content missing (not broken, just uninformed/forgotten)
- supervisor provides context refresh ⟜ foreman continues

**forgetting is necessary** ⟜ perfect context → fussing → madness
- foreman must be allowed to forget or they fuss over context ball
- trying to maintain perfect memory leads to:
  - constant re-reading entire log
  - meta-recursion about what's been done
  - unbounded context growth
  - cycling and madness
- vaguelyclear output ⟜ acceptable cost of forgetting

**reestablish state from outside** ⟜ reconcile notes against reality
- when new foreman reads chaotic HEAD ⟜ don't trace through madness
- run agent to check actual state:
  - what files actually exist
  - what repos at what state
  - what PRs actually open
  - what artifacts actually present
- write fresh HEAD matching reality
- notes become working hypothesis ⟜ not truth
