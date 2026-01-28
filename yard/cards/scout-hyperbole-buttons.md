scout-hyperbole-buttons ⟜ explore button, action, and HTMX attributes in Hyperbole

**input** ⟜ repo + area to explore

**output** ⟜ findings + type signatures/patterns

**instruction**
cabal repl with Hyperbole imports
:type button_, hxPost, hxGet
explore button construction: button_ [hxPost "/api/action"] (text "Click me")
test HTMX attributes: understand attribute pattern
investigate action type and how to route HTTP actions back to Hyperbole handlers
document button ↔ action ↔ HTTP request flow.
