# tail-first ⟜ process outcome → tale routing architecture

**mandate** ⟜ capture process success/failure/timeout in minimal tokens via specialized agents

**captain** ⟜ me, outside context reference point
**prime** ⟜ single spinner controller, reads captain orders, spins workers, no meta-spinning
**field workers** ⟜ tale agents (ephemeral, bounded execution, compressed output)

---

## Tale Agents ⟜ four specializations

**nexttale** ⟞ success path
  ⟜ detects completion indicators, extracts metrics
  ⟜ tail(1) for completion, tail(5) for metrics
  ⟜ output: ✓ status, duration, file count, warnings, 1-line summary
  ⟜ lazy execution (skip if unneeded)

**failtale** ⟞ known error path
  ⟜ extracts error codes, categorizes failure patterns
  ⟜ tail(1) → tail(5) → tail(100) progressive depth
  ⟜ output: ✗ status, error-code, error-type, package, 5-line context, fix suggestion
  ⟜ lazy execution (skip if unneeded)

**fixit** ⟞ timeout/hang path
  ⟜ investigates complex failures, hang points, resource bottlenecks
  ⟜ tail(100) for hang detection, full log for root cause
  ⟜ output: diagnosis, hang-point, repair-strategy, timeout-adjustment
  ⟜ specialized in stuck operations, repeated patterns, timeout correlation

**bloodhound** ⟞ deep investigation path
  ⟜ follows error trails across dependency chains
  ⟜ tail(1) → tail(100) → tail(∞) progressive depth
  ⟜ output: root-cause, error-trail, similar-patterns from history, deep-solution
  ⟜ pattern recognition, history correlation, beyond symptoms

---

## Routing Strategy ⟜ outcome → tale mapping [UNCERTAIN]

**inputs** ⟜ exit-code, timeout-status, error-pattern match, output-file

**routing-logic** ⟞ [QUESTIONABLE - nested if/elif, order matters?]
  exit-code == 0 & !timeout → nexttale
  exit-code != 0:
    if error-pattern ∈ known-patterns → failtale
    else → bloodhound
  timeout & duration > expected → fixit
  timeout & duration ≤ expected → failtale(extended)

**open questions**
  - order of checks: does priority matter?
  - error-pattern matching: how is "known" defined?
  - timeout threshold: where's the boundary?
  - edge cases: partial success + warning?
  - cascading: can tale1 trigger tale2?

**output** ⟜ {tale-type, parameters, priority}

---

## Coordination Layer ⟜ orchestration agents [EXPERIMENTAL]

**tale-router** ⟞ dispatcher
  ⟜ specialization: intelligent tale selection
  ⟜ input: {exit-code, timeout-status, process-metadata}
  ⟜ output: which tale to spin, with parameters
  ⟜ efficiency: 20-30 tokens

**tale-spinner** ⟞ instantiator
  ⟜ specialization: parameterization + timeout assignment
  ⟜ nexttale: 5s timeout, tail(1)
  ⟜ failtale: 8s timeout, tail(1)→tail(5)
  ⟜ fixit: 15s timeout, tail(100)
  ⟜ bloodhound: 30s timeout, progressive
  ⟜ output: {worker-id, tale-card, expected-duration, callback-logic}
  ⟜ efficiency: 15-25 tokens

**process-with-tale** ⟞ execution coordinator
  ⟜ streaming output to file, monitor completion
  ⟜ auto-trigger tale-router on completion
  ⟜ integrate tale results into workflow
  ⟜ output: {process-status, tale-result, next-step}
  ⟜ efficiency: 15-25 tokens

---

## Execution Model ⟜ five-phase flow

**phase 1: specification**
  ⟜ define process command, output file, timeout, callback routing

**phase 2: execution**
  ⟜ run with streaming output, monitor status
  ⟜ no token consumption during run

**phase 3: routing** [UNCLEAR - who calls router?]
  ⟜ analyze completion data, select tale type, instantiate with parameters

**phase 4: tale execution**
  ⟜ tale agent performs specialized analysis
  ⟜ progressive tail resolution (tail(1) → tail(N) → tail(∞))
  ⟜ returns structured result

**phase 5: integration**
  ⟜ feed tale result back to foreman
  ⟜ trigger next action, maintain state continuity

---

## Spinner Model ⟜ single controller constraint [UNCERTAIN]

**prime** ⟜ only allowed to spin background workers
  ⟜ flat structure: no meta-spinning (no spinner spinning spinners)
  ⟜ expert in card writing (turn captain orders → crisp form)
  ⟜ all field worker instantiation goes through prime

**open questions**
  - does prime run synchronously or background?
  - does prime write cards dynamically or use templates?
  - what if prime needs to spin multiple tales in parallel?
  - who manages prime's own timeouts/failures?
  - can foreman override prime's routing?

---

## Efficiency Calculus ⟜ token cost vs gain

**composition** ⟜
  process card: 15-25 tokens
  tale router: 20-30 tokens
  tale spinner: 15-25 tokens
  tale analysis: 25-80 tokens
  **total: 75-160 tokens**

**traditional approach**
  monolithic error analysis: 300-500 tokens

**gains** ⟜ ~3x compression, lazy execution, specialization

**assumptions**
  - each tale is small + focused (true?)
  - lazy execution actually happens (implemented?)
  - compression doesn't lose signal (tested?)

---

## Implementation Status ⟜ what exists? what's spec?

**exists** ⟜ agent types (nexttale, failtale, fixit, bloodhound) sound
**uncertain** ⟜ tale-router logic, tale-spinner parameterization, process-with-tale orchestration
**missing** ⟜ integration with foreman infrastructure, listener callbacks, timer management
**untested** ⟜ spinner-only constraint in practice, error pattern matching, timeout thresholds


