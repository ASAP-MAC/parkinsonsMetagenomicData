---
title: Development Guidance
topics: [coding-standards, parquet-structure, validation, api-design]
---

# Development Guidance

## Coding standards
- Use `@noRd` for internal helper functions
- Export user-facing functions and document them fully (see [30-testing-and-docs.md](30-testing-and-docs.md))
- Validation helpers should start with `confirm_*`
- Return `TreeSummarizedExperiment` when appropriate
- Use `parquet_colinfo(data_type)` to confirm column structure before coding against a data type

## Column roles in parquet files
- `cname`: column names, typically sample IDs / `uuid`
- `cdata`: column metadata such as versions or commands
- `rname`: row names, feature IDs
- `rdata`: row metadata, feature annotations
- `assay`: measurement values

## When to suggest package changes
Good candidates:
- simplifying common analysis patterns
- better validation and error messages
- performance improvements for large files
- reference data that benefits multiple users

Avoid suggesting:
- visualization functions
- analysis functions better suited to other packages
- study-specific filtering logic
- breaking API changes