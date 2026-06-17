# Data Access Patterns

## Overview

Uses curatedCore as the data-access layer. DuckDB connects to remote parquet files stored on Hugging Face or GCP, allowing incremental queries and filtering before downloading into R.

## Primary Data Access Functions

### High-Level Functions

- `returnSamples()`: Create TreeSummarizedExperiment objects
- `loadParquetData()`: Return filtered tibbles/data frames

### Low-Level Functions

- `accessParquetData()`: Open duckdb connection to remote datasets

## Data Sources

Remote repositories hosted on Hugging Face (waldronlab/metagenomics_mac) and GCP (gs://metagenomics-mac).

## Access Patterns

### Basic Retrieval

```r
con <- accessParquetData("huggingface", "metaphlan_relative_abundance")
res <- loadParquetData(con)
```

### Filtered Retrieval

```r
con <- accessParquetData("huggingface", "metaphlan_relative_abundance")
tse <- returnSamples(con, 
                     dataType = "metaphlan_relative_abundance",
                     samples = c("sample1", "sample2"))
```

### Advanced Queries

Use dplyr verbs on DuckDB connections before calling collect().

## Large File Handling

DuckDB enables fetching only requested columns or filtering rows via SQL without downloading full parquet files.

## Testing with Data

- **Production data**: Hugging Face parquet files accessed via curatedCore::parquetRepoSource.
- **Test/example data**: inst/extdata/
- **Running tests**: Local dummy parquet files, text files, and .Rds files used for unit tests to avoid network dependence.