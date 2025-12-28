the second reason for laziness
===

In 
[Why Functional Programming Matters](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf) John Highes argues that a desire for modularity - the atomisation of a problem domain - leads structural programming to need new types of glue to stick components together.

{{< mermaid >}}
graph LR;
A[Modularity]-->B[composition];
B-->C[Profit]
{{< /mermaid >}}


[laziness gives us transparent streaming](https://www.reddit.com/r/haskell/comments/j3kbge/comment/g7foelq/?utm_source=share&utm_medium=web2x&context=3)

*** sum-type continuations are an anti-pattern

[Faster Coroutine Pipelines: A Reconstruction](https://rubenpieters.github.io/assets/papers/JFP20-pipes.pdf)


https://www.reddit.com/r/haskell/comments/11z8ueh/an_apologia_for_lazy_evaluation/

