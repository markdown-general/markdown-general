hyperfunction-parser-regression-guard ⟜ avoid reverting to state-threaded design

**input** ⟜ parser/hyperfunction design

**output** ⟜ architecture + implementation approach

**instruction**
monitor for trap: when satisfy/tokens seem to work as plain functions (parse :: t -> These e (a, t)), it feels like progress and tests pass.
resist this reversion. you've abandoned hyperfunction composability and reverted to parsec-style state threading.
consequence: measurement requires threading Perf tuple, logging requires log list, semantic parsing needs another tuple, performance overhead everywhere, backtracking invisible.
maintain abstraction: keep hyperfunction representation (t ↬ ...) at all levels.
