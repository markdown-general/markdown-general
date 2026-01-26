hyperfunction-parser-implementation ⟜ core parser type and instance chain

**input** ⟜ parser/hyperfunction design

**output** ⟜ architecture + implementation approach

**instruction**
define core type: Parser t e a = Parser { parse :: t ↬ These e (a, t) }
where t ↬ ... is hyperfunction (takes continuation), These e (a, t) is result with error, value, remainder.
implement instance chain in order: Functor (map over a, preserve stream/error), Applicative (chain parsers, thread t), Profunctor (map input/output types), Alternative (backtracking with These recovery).
primitive: satisfy (p :: Token -> Bool) to parse individual tokens with condition.
