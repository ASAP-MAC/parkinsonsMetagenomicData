# Development Patterns


## Package-Specific Patterns

### Function Organization

Modular R files like parseFiles.R, readParquet.R, getMetagenomicData.R, utils.R

### Naming Conventions

Mixed camelCase (e.g., returnSamples) and snake_case (e.g., biobakery_files).

## S4 Classes and Methods

None defined internally. Uses TreeSummarizedExperiment from Bioconductor.

## Key Dependencies

curatedCore, duckdb, arrow, httr2, TreeSummarizedExperiment.

## Code Style Notes

Uses tidyverse conventions (dplyr, stringr, purrr) and dbplyr.