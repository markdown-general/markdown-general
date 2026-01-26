server-test ⟜ minimal server test: load, start, wait for connection

**instruction**
simplified server test - just verify server starts and responds:
1. cabal repl, import Prettychart.Server
2. (send, stop) <- startChartServerHyperbole defaultChartServerConfig
3. wait 2s
4. stop
verify: server starts, listens, can be stopped cleanly.
