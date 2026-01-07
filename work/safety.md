# work/safety.md

**safety rating** ⟜ **unsafe**

**a safe space** ⟜ is a happy space.

**Code injection** 
  ⟜ malicious workflow description executes arbitrary commands
  ⟝ sandboxing and input validation

**Filesystem access**
  ⟜ agent can read/write anywhere, deletes wrong directory  
  ⟝ access controls and directory protection

**API key exposure**
  ⟜ tools might leak credentials in literate documentation
  ⟝ secrets management and credential scrubbing

**Dependency hell** 
  ⟜ agent installs unvetted packages that compromise system
  ⟝ package verification and dependency pinning

**Runaway processes** 
  ⟜ agent spawns infinite talk, consumes all resources
  ⟝ process limits and resource constraints

**Data exfiltration** 
  ⟜ agent sends private content to external services
  ⟝ network controls and data governance
