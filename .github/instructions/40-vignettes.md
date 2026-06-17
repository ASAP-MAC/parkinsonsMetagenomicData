# Vignette Guide

## Available Vignettes

1. `first-15-minutes.Rmd` - Quick Start
2. `codebook.Rmd` - Data Codebook
3. `full-workflow.Rmd` - Comprehensive data retrieval
4. `piecewise-workflow.Rmd` - Advanced database control
5. `working-with-large-parquet-files.Rmd` - Large data handling
6. `full-data-preparation.Rmd` - Background on data creation
7. `file-types.Rmd` - Overview of file formats

## Recommended Reading Order

first-15-minutes, codebook, full-workflow, piecewise-workflow, working-with-large-parquet-files, full-data-preparation, file-types.

## Updating Vignettes

Guidelines for maintaining vignettes:
- Ensure all code chunks execute successfully
- Update examples if function signatures change
- Test: `devtools::build_vignettes()`