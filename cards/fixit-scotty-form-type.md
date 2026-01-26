fixit-scotty-form-type ⟜ resolve HashMap/Map type mismatch in scotty HTTP form handling

**instruction**
1. identify root cause: http-api-data 0.7 changed Form from HashMap to Map (HashDOS mitigation)
2. locate definition: Form = Form { unForm :: Map Text [Text] }
3. fix paramListToForm: replace HM.alter HM.empty with Map.alter Map.empty in Web/Scotty/Action.hs:459
4. verify: scotty-0.30 builds, only unrelated Typeable warnings remain
5. test: chart-svg-dev can use fixed scotty from local repo

**comment** ⟜ this is a complete fix arc: cause → locate → fix → verify → test / steps flow together and each depends on the previous / stays as one coherent card / good template for targeted library fixes
