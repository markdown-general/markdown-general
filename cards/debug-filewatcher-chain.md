card-debug-filewatcher-chain ⟜ diagnose prettychart-serve --watch empty response issue

**instruction**
trace execution chain: SVG file created → watchDir detects → svgEvent filters → callback triggered → BL.readFile → TE.decodeUtf8 → send text → chartRef updated → handler renders HTML → browser response
add logging at each phase:
  1. putStrLn "EVENT: " + show e (callback triggered?)
  2. putStrLn "SVG detected: " + svgPath (file reading?)
  3. putStrLn "Read " + show bytes (decoding?)
  4. putStrLn "send returned: " (response?)
  5. curl -i http://localhost:9161 (HTTP valid?)
suspected root cause: forever $ threadDelay loop blocking watchDir callback execution; solution: run watchDir + forever in separate async threads

**comment** ⟜ this is solid: diagnosis teaching + hypothesis = fix / diagnostic method (trace → log → test) informs the solution (async split) / stays coherent as one card / good example of "observe then fix"
