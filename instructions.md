# AI Agent Instructions for parkinsonsMetagenomicData

This document provides guidance for AI agents assisting with code review, data analysis, and development of the `parkinsonsMetagenomicData` package.

## Package Overview

**Purpose**: Provides access to uniformly processed gut microbiome data from Parkinson's Disease studies.

**Key Architecture**:
- Data stored as parquet files on Hugging Face
- Accessed via DuckDB for efficient remote querying
- Returns TreeSummarizedExperiment objects for downstream analysis
- Sample metadata in `sampleMetadata` data frame (included in package)
- Microbiome data accessed remotely or from local files

**Primary Repositories**:
- `waldronlab/metagenomics_mac` - Full dataset (all samples)
- `waldronlab/metagenomics_mac_examples` - Small examples (10 samples per file)

## Key Concepts

### Data Types Organization

**Taxonomic** (MetaPhlAn):
- `relative_abundance` - Primary taxonomic profiling
- `viral_clusters` - Viral community composition
- `marker_abundance`, `marker_presence` - Species markers

**Functional** (HUMAnN):
- `genefamilies_*` - Gene family abundances
- `pathabundance_*` - Metabolic pathway abundances
- `pathcoverage_*` - Pathway completeness
- Variants: `*_stratified` (by species) vs `*_unstratified` (community total)
- Normalizations: raw (RPK), `*_relab` (relative %), `*_cpm` (copies per million)

**Quality Control**:
- `fastqc` - Sequencing quality
- `kneaddata_log` - Preprocessing stats

### Sample Identifiers

- **Primary key**: `uuid` (links all data types)
- UUIDs connect microbiome data to `sampleMetadata`
- Each sample has metadata fields: age, sex, disease, study_name, etc.

## User Workflow Guidance

### For Simple Data Retrieval

**Use `returnSamples()`** when users want:
- Quick data access without complex filtering
- To filter samples by metadata first
- TreeSummarizedExperiment output immediately

```r
# Filter metadata first
my_samples <- sampleMetadata %>% filter(age >= 18)

# Retrieve data
tse <- returnSamples(sample_data = my_samples,
                     data_type = "relative_abundance")
```

### For Advanced/Filtered Retrieval

**Use `accessParquetData()` + `loadParquetData()`** when users need:
- Feature-level filtering before data retrieval
- Direct DuckDB queries
- Custom SQL-like operations
- Memory-efficient loading of large files

```r
# Connect and filter by features
con <- accessParquetData(data_types = "relative_abundance")
tse <- loadParquetData(con,
                       data_type = "relative_abundance",
                       filter_values = list(
                         clade_name_species = c("s__Escherichia_coli")
                       ))
```

### Large File Strategy

For `genefamilies_stratified` and other large files:
1. **Always filter on sorted columns** (`uuid`, `gene_family_uniref`, `pathway`, etc.)
2. Use two-stage filtering: remote (sorted cols) → local (other cols)
3. Consider downloading files locally for repeated queries
4. See `vignettes/working-with-large-parquet-files.Rmd` for details

## Code Review Guidelines

### Function Documentation

**Required for exported functions**:
- `@title`, `@description`, `@param`, `@return`, `@examples`
- At least one runnable example (not in `\donttest`)
- For functions with network access, provide both remote and local examples

**Use `\donttest` when**:
- Examples require network access to Hugging Face
- Examples might trigger rate limits
- Examples depend on external API availability
- **Always provide alternative runnable examples using local test files**

**Never use `\dontrun`** - Bioconductor discourages it

### Testing Standards

**Test files location**: `inst/extdata/`
- Small parquet files for testing (~20KB)
- Sample TSV/TXT files from bioBakery tools
- RDS objects for TreeSummarizedExperiment testing

**All exported functions should have**:
- Tests using local `inst/extdata/` files
- Examples demonstrating typical usage
- Error handling for invalid inputs

### Coding Standards

**Function design**:
- Internal helper functions: use `@noRd` (not exported)
- User-facing functions: full documentation + export
- Validation functions: start with `confirm_*`
- Return TreeSummarizedExperiment when appropriate

**Column roles in parquet files**:
- `cname`: Column names (sample IDs, typically `uuid`)
- `cdata`: Column metadata (versions, commands)
- `rname`: Row names (feature IDs)
- `rdata`: Row metadata (feature annotations)
- `assay`: Measurement values

**Use `parquet_colinfo(data_type)` to check column structure**

## Common Analysis Patterns

### Pattern 1: Case-Control Study

```r
# Filter samples by metadata
data("sampleMetadata")
samples <- sampleMetadata %>%
  filter(control %in% c("Case", "Study Control"),
         age >= 18,
         !is.na(sex))

# Retrieve taxonomic data
tse <- returnSamples(sample_data = samples,
                     data_type = "relative_abundance",
                     taxa_level = "species")
```

### Pattern 2: Specific Taxa Analysis

```r
# Connect and filter for specific bacteria
con <- accessParquetData(data_types = "relative_abundance")
tse <- loadParquetData(
  con,
  data_type = "relative_abundance",
  filter_values = list(
    clade_name_genus = "g__Bacteroides"
  )
)
```

### Pattern 3: Functional Profiling

```r
# For functional analysis, choose normalization based on goal:
# - *_relab for relative comparisons within samples
# - *_cpm when comparing across different sequencing depths
# - *_stratified to see species contributions
# - *_unstratified for community-level measurements

tse <- returnSamples(
  sample_data = my_samples,
  data_type = "pathabundance_relab_unstratified"
)
```

### Pattern 4: Multiple Data Types

```r
# Load different data types separately
rel_abund <- returnSamples(sample_data, "relative_abundance")
pathways <- returnSamples(sample_data, "pathabundance_relab")

# UUIDs in colData allow integration
```

## Common Issues and Solutions

### Issue: Rate Limiting (HTTP 429 errors)

**Cause**: Too many requests to Hugging Face API or scanning unsorted columns

**Solutions**:
- Filter on sorted columns (check with `parquet_colinfo()`)
- Use `metagenomics_mac_examples` repo for testing
- Download files locally for repeated queries
- For large files, follow two-stage filtering approach

### Issue: Memory Errors

**Cause**: Loading too much data at once

**Solutions**:
- Filter more aggressively before `collect()`
- Use `loadParquetData()` with specific UUIDs
- For large files, use persistent DuckDB database: `db_connect(dbdir = "path.duckdb")`
- Consider stratified vs unstratified versions (unstratified are smaller)

### Issue: No Data Returned

**Cause**: Feature names don't match parquet file format

**Solutions**:
- Check exact feature names: `load_ref("clade_name_ref")` for taxa
- Taxa names format: `"s__Escherichia_coli"` (not `"Escherichia coli"`)
- Gene families: `"UniRef90_A0A000"` format
- Use `filter_values` with exact matches

### Issue: UUID Mismatches

**Cause**: Sample not in requested data type or typo in UUID

**Solutions**:
- Verify UUIDs exist: `uuid %in% sampleMetadata$uuid`
- Check if data type has those samples: `get_hf_parquet_urls()`
- UUIDs are lowercase hex with hyphens: `"c3eb1e35-9a43-413d-8078-6a0a7ac064ba"`

## Development Workflow

### Adding New Functions

1. Write function with full roxygen2 documentation
2. Add `@export` if user-facing, `@noRd` if internal
3. Create examples using `inst/extdata/` files
4. Add network examples in `\donttest` if applicable
5. Run `roxygen2::roxygenize()` to update NAMESPACE and man pages
6. Build and check: `R CMD build .` then `R CMD check *.tar.gz`

### Adding Test Data

Test data should be:
- Small (<50KB per file)
- Representative of real data structure
- Located in `inst/extdata/`
- Documented in examples

### Vignette Guidelines

Existing vignettes serve different audiences:
- `Data Codebook` - Reference for all variables and structures
- `First 15 Minutes` - Quick start for new users
- `Full Workflow` - Comprehensive tutorial with sample + feature filtering
- `Piecewise Workflow` - Advanced direct database control
- `Working with Large Parquet Files` - Performance optimization

**When suggesting vignette updates**:
- Keep each focused on its specific audience
- Use consistent code style across vignettes
- Always test code chunks before committing
- Include the vignette guide table for navigation

## Helper Functions Reference

### Data Discovery
- `data_dict()` - Sample metadata field definitions
- `biobakery_files()` - Available data types with descriptions
- `parquet_colinfo(data_type)` - Column structure for a data type
- `get_repo_info()` - Available Hugging Face repositories
- `get_ref_info()` - Available reference files
- `get_hf_parquet_urls()` - Direct parquet file URLs

### Data Access
- `returnSamples()` - High-level: metadata → TreeSummarizedExperiment
- `accessParquetData()` - Mid-level: setup DuckDB connection with views
- `loadParquetData()` - Mid-level: load filtered data from connection
- `db_connect()` - Low-level: create DuckDB connection
- `load_ref()` - Load reference lookup tables

### Utilities
- `output_file_types()` - Map data types to tool/file info
- Internal validation: `confirm_*()` functions for input checking

## When to Suggest Package Changes

**Good candidates for new features**:
- Simplifying common analysis patterns
- Adding validation for better error messages
- Performance improvements for large files
- Additional reference data that benefits multiple users

**Avoid suggesting**:
- Analysis functions (this is a data package)
- Visualization functions (use microbiome analysis packages)
- Study-specific metadata filtering (users handle this)
- Breaking changes to existing APIs

## Git Workflow Notes

**Branch naming**: Descriptive names (e.g., `enhance-vignettes`, `fix-uuid-validation`)

**Commit messages**: Follow conventional commits format
```
type: brief description

- Detailed point 1
- Detailed point 2

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
```

**Types**: `feat`, `fix`, `docs`, `test`, `refactor`, `chore`

**Before committing**:
- Run `R CMD check` on built tarball
- Verify all vignettes knit successfully
- Check that examples in modified functions run
- Update documentation with `roxygen2::roxygenize()` if needed

## Quick Reference Commands

```bash
# Build and check
R CMD build .
R CMD check parkinsonsMetagenomicData_*.tar.gz

# Build and run BiocCheck
R CMD build .
R -e "BiocCheck::BiocCheck('parkinsonsMetagenomicData_*.tar.gz')"

# Regenerate documentation
R -e "roxygen2::roxygenize()"

# Render specific vignette
R -e "rmarkdown::render('vignettes/codebook.Rmd')"

# Install locally
R CMD INSTALL .
```

## Additional Resources

- [Bioconductor Package Guidelines](https://contributions.bioconductor.org/)
- [DuckDB R Documentation](https://duckdb.org/docs/stable/clients/r.html)
- [TreeSummarizedExperiment](https://bioconductor.org/packages/TreeSummarizedExperiment)
- [MetaPhlAn](https://github.com/biobakery/MetaPhlAn)
- [HUMAnN](https://github.com/biobakery/humann)

---

**Last Updated**: March 2026
**Package Version**: 0.99.0
