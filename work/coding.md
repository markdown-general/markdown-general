## coding guide

- content/ is only markdown.
- literate programming is our coding style
- the language preference is english.
- all our code is in content/
- composition is encouraged

## What is code?

Code then, given this context, is a markdown document typically containing:

- **Narrative** ⟜ what it does, why, how
- **Code** ⟜ executable implementation in fenced blocks
- **Run** ⟜ how to install and run.
- **API** ⟜ inputs, outputs and configuration
- **Examples** ⟜ usage patterns
- **Tests** ⟜ expected behavior

You can mash a pdf-to-md.md and put it in base/ using this recipe:

- put the important stuff up front. Without context the important stuff might be:

``` markdown
PDF is for printing not parsing, so converting PDF into markdown can get messy.

- marker-pdf, a python module, is best at handling this compexity.
- marker-pdf uses multiple ML models (layout detection, OCR) and initial loading is expensive.
```

