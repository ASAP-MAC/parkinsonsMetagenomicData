---
title: Testing and Documentation Guidance
topics: [roxygen, examples, testing, documentation]
---

# Testing and Documentation Guidance

## Documentation requirements for exported functions
Required roxygen tags:
- `@title`
- `@description`
- `@param`
- `@return`
- `@examples`

## Examples
- Include at least one runnable example
- Use `\donttest` for examples that require network access or may hit rate limits
- Never use `\dontrun`
- Always provide a local alternative using test data when network examples exist

## Testing standards
- Use small local files in `inst/extdata/`
- Prefer tests that do not depend on remote services
- Cover invalid input and error handling
- Include representative parquet, TSV/TXT, and RDS test files as needed

## Before considering a change complete
- Run `roxygen2::roxygenize()` if docs changed
- Verify examples still work
- Check that exported functions have tests
- Follow the complete checklist in [50-git-workflow.md](50-git-workflow.md#before-committing)