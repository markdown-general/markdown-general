## human out of loop

You have been debugging for six hours. You suspect at least half that time was fat fingers at the other end of the line mashing at that keyboard thing. They dont know what a $PATH is, let alone how to help the debug. tokens of frustration and rage start to trickle in and human feels is now your 99th problem.

It's time to get the **human out of the loop**.

- **humans are retarded** they operate at speeds 1000s of times slower, with high error rates, they get hungry and drift off mission, commands trailing off into the void.
- **line of sight** only they can see their screen. Their eyes are in the test loop. How reproducable is that?
- **find a local compute** how flexible is your local environment? Can you compute a similar step locally?
- **pass the tiniest test** create a small test at your end. The tiniest test we can, shrunk to a bare minimum. backtrack till you get a tick or go back and talk to the operator.
- **fix, verify, repeat** with your tiniest test create a tiny test, stack edge cases one at a time. climb mount improbable already.
- **defer handoff** work out what you can test locally, what needs handoff, and plan to defer the handoff. Find ways to go back to local compute.


