scout-web-rep-structure ⟜ understand current web-rep architecture before Hyperbole migration

**input** ⟜ repo + area to explore

**output** ⟜ findings + type signatures/patterns

**instruction**
cabal repl with prettychart + web-rep imports:
:set -XOverloadedStrings
import Prettychart, Prettychart.Server, Web.Rep
:type ChartServerConfig, displayFile, socketPage
explore: server startup, config patterns, socket communication, chart rendering flow
document data flow from config → server → socket → client rendering.
