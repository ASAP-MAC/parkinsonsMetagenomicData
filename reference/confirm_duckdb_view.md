# Validate DuckDB view/table argument

'confirm_duckdb_view' checks that an object is a valid DuckDB table
connection object

## Usage

``` r
confirm_duckdb_view(view)
```

## Arguments

- view:

  Object to validate

## Details

This function is intended to be used within another function as input
validation. If the input is valid, nothing will happen. If it is not,
the function will throw a 'stop()' error.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 con <- accessParquetData(data_types = "pathcoverage_unstratified")
 view <- tbl(con, "pathcoverage_unstratified_uuid")
 confirm_duckdb_view(view)
 }
} # }
```
