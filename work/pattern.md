# pattern.md

**pattern** ⟜ structure seen, made, compressed, cast, empty

**grammar** ⟜ curating and cataloguing. patterns recognized.
**cast** ⟜ encoding, transform, cross structure
**recognition** ⟜ seeing and searching; compare, select, merge
**generate** ⟜ extend from positive space; replicate, reuse, extend
**compress** ⟜ densify, compact, retreat.
**absence** ⟜ the hidden, quotiented, residue, negative space

## grammar
**decking** ⟜ a grammar for markdown
⟜ permissive by default, relaxed & flexible
⟜ [grammar](grammar) for design, [decking](decking) for usage

lead ⟜ a few tokens
     ⟜ slug
dash ⟜ the type of elab
elab ⟜ an elaboration of the lead
slug ⟜ lead (dash elab)*
deck ⟜ a few slugs
card ⟜ a few decks
     ⟜ a markdown file

## grammar

**decking** ⟜ a grammar for markdown

lead ⟜ a few tokens
     ⟜ slug
dash ⟜ the type of elab
elab ⟜ an elaboration of the lead
slug ⟜ lead (dash elab)*
deck ⟜ a few slugs

permissive by default
relaxed & flexible

[grammar](grammar) for design.
[decking](decking) for usage.
[]


## grammar
**decking** ⟜ a grammar for markdown
⟜ permissive by default, relaxed & flexible
⟜ [grammar](grammar) for design, [decking](decking) for usage

lead ⟜ a few tokens
     ⟜ slug
dash ⟜ the type of elab
elab ⟜ an elaboration of the lead
slug ⟜ lead (dash elab)*
deck ⟜ a few slugs










lead ⟜ a few tokens
     ⟜ slug
dash ⟜ the type of elab
elab ⟜ an elaboration of the lead
slug ⟜ lead (dash elab)*
deck ⟜ a few slugs

decking vs BNF

· facet: ⟜ ⟜ "is an elaboration of" not "is defined as"
  - support: creates space for multiple interpretations
  - support: suggests relationship rather than rule
  - support: bidirectional (prose ↔ deck) vs unidirectional (definition → production)

· facet: meaning ⟜ inferred from context
  - support: token, line, lead remain open for elaboration
  ~ contrast: BNF uses <non-terminals> to corral semantics
  ~ contrast: requires explicit definition before use

· facet: stance ⟜ decking is interpretive; BNF is prescriptive
  - support: decking adapts to thinking
  ~ contrast: BNF constrains to specification
  + commonality: both borrow *, +, ? for quantity notation

· facet: structure ⟜ intermediate leads are expendable in decking
  - support: facet labels can dissolve into prose
  - support: elaborations carry the actual content
  ~ contrast: BNF non-terminals are load-bearing, can't be dropped
  
  
###  decking vs BNF

decking vs BNF ⟜ "is an elaboration of" not "is defined as"
  - bidirectional (prose ↔ deck) suggesting relationship rather than rigid definition
  ~
  +

meaning ⟜ inferred from context 
  - token, line, lead remain open for elaboration
  ~ BNF uses <non-terminals> to corral semantics and requires explicit definition
  +

stance ⟜ decking is interpretive; BNF is prescriptive
  - decking adapts to thinking
  ~ BNF constrains to specification
  + both borrow *, +, ? for quantity notation

structure ⟜ intermediate leads are expendable in decking
  - elaborations carry actual content while facet labels can dissolve
  ~ BNF non-terminals are load-bearing, can't be dropped
  +

lead ⟜ a few tokens ⟜ lead (dash elab)
dash ⟜ the type of elab
elab ⟜ an elaboration of the lead
line ⟜ lead (dash elab)*
deck ⟜ a few lines

lead ⟜ a few tokens; or a line.
**bolded** ⟜ bolded lead
dash ⟜ a symbol to represent the type of elab
elab ⟜ an elaboration of the lead
line ⟜ lead [dash elab]*
deck ⟜ [line] 3+
head ⟜ first line of a deck, where the tail is an elab of the first line.
tail ⟜ if the first line is a header, the rest of the deck; otherwise all the lines.
lead deck ⟜ a deck with a head
list deck ⟜ not a head deck
filled deck ⟜ a deck with everything filled in
unfilled deck ⟜ a deck with not everything filled in
range deck ⟜ a deck whwre the leads have a wide semantic range. For a lead deck, the tail represents a semantic range of the head.
branch ⟜ a lead with multiple follows
lattice ⟜ 2+ branches
full lattice ⟜ a lattice where branches have follows with the same dashes and a follow key
follow key ⟜ a list of dash ◊ content
content ⟜ a lead or a line

### follow types

⟜ ⟜ simple continuation elaboration

others are abstracted local meanings
