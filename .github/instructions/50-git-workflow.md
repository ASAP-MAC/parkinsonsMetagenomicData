---
title: Git Workflow Guidance
topics: [git, commits, branches, checks, bioconductor]
---

# Git Workflow Guidance

## Branch naming
Use descriptive branch names such as:
- `enhance-vignettes`
- `fix-uuid-validation`

## Commit messages
Use conventional commit style:
```text
type: brief description

- Detailed point 1
- Detailed point 2

Co-Authored-By: [AI Agent Name and Version] <noreply@[provider].com>
```

Types:
- `feat`
- `fix`
- `docs`
- `test`
- `refactor`
- `chore`

### AI Agent Acknowledgements
Always include a `Co-Authored-By` line acknowledging AI assistance. Use the appropriate format for your agent:
- **Claude Code**: `Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>`
- **GitHub Copilot**: `Co-Authored-By: GitHub Copilot <noreply@github.com>`
- **Cursor**: `Co-Authored-By: Cursor AI <noreply@cursor.sh>`
- **Other agents**: Use the agent name, version if known, and appropriate email

## Before committing
- Run `R CMD check` on the built tarball
- Verify vignettes knit successfully
- Confirm examples in modified functions run
- Regenerate docs with `roxygen2::roxygenize()` if needed

## Quick commands
```bash
R CMD build .
R CMD check parkinsonsMetagenomicData_*.tar.gz
R -e "BiocCheck::BiocCheck('parkinsonsMetagenomicData_*.tar.gz')"
R -e "roxygen2::roxygenize()"
R -e "rmarkdown::render('vignettes/codebook.Rmd')"
R CMD INSTALL .
```