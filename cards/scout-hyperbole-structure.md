scout-hyperbole-structure ⟜ understand Hyperbole 0.6 architecture, types, view system

**input** ⟜ repo + area to explore

**output** ⟜ findings + type signatures/patterns

**instruction**
cabal repl with Hyperbole setup:
:set -XOverloadedStrings -XDataKinds
import Web.Hyperbole, Network.Wai, Network.Wai.Handler.Warp
:type liveApp, runServer
explore view system: type View, action routing, handler patterns
investigate app structure for chart rendering
document type flow from request → action → view → response.
