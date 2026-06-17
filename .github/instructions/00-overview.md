# parkinsonsMetagenomicData Overview

## Classification
- **Type**: Data Package
- **Version**: 0.99.0

## Purpose

Provides functions to retrieve raw tabular data from Google Cloud Bucket or Hugging Face, starting from a pre-built sample metadata file. Data types include output from FastQC, KneadData, MetaPhlAn, and HUMAnN. Outputs a TreeSummarizedExperiment object as the final result.

## Key Functions

**Data Access Functions**:
- `returnSamples()`: Main high-level data retrieval function
- `loadParquetData()`: Load filtered data from DuckDB connection
- `accessParquetData()`: Access a dataset on Hugging Face or GCP

**Discovery Functions**:
- `parquet_colinfo()`: Inspect column structure
- `biobakery_files()`: List available data types
- `data_dict()`: View data dictionary
- `get_hf_parquet_urls()`: Get Hugging Face parquet URLs
- `get_repo_info()`: Get repository info
- `get_ref_info()`: Get reference information
- `load_ref()`: Load reference tables

## Quick Start

```r
library(parkinsonsMetagenomicData)
# View sample metadata
head(sampleMetadata)

# Create a DuckDB connection to parquet files
con <- accessParquetData("huggingface", "metaphlan_relative_abundance")

# Load data into a TreeSummarizedExperiment
tse <- returnSamples(con, dataType = "metaphlan_relative_abundance")
```

## Key Concepts

TreeSummarizedExperiment, DuckDB, Parquet, Microbiome profiles, Hugging Face data hosting.

## Data Sources

Hugging Face (waldronlab/metagenomics_mac) and Google Cloud Storage (gs://metagenomics-mac).