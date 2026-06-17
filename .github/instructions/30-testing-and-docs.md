# Testing and Documentation

For complete standards, see:
- [Core Bioconductor standards](../../templates/bioconductor-development.md)
- [Waldronlab conventions](../../templates/waldronlab-standards.md)

## Development and Checking Commands

```bash
# Build and check
R CMD build .
R CMD check parkinsonsMetagenomicData_*.tar.gz

# Documentation
R -e "roxygen2::roxygenize()"

# Tests
R -e "devtools::test()"
```

## Package-Specific Considerations

[To be documented]

## Package-Specific Testing

### Test Organization

Standard testthat setup with tests separated by functionality (e.g., test-readParquet.R, test-utils.R).

### Test Data

- **Location**: inst/extdata/
- **File types**: Parquet, .Rds, .csv, and compressed text files.
- **Purpose**: Validate reading, parsing, and data retrieval functions locally.

### Remote Data Testing

Primarily tested against local versions of files in inst/extdata to ensure tests pass offline.

### Running Tests

devtools::test()

## Package-Specific Documentation Patterns

### Function Categories

roxygen2 standard documentation

### Common Parameters

con (duckdb connection), dataType (character), repository (character)

## Common Testing Patterns

testthat edition 3