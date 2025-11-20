# Validate DuckDB connection argument

'confirm_duckdb_con' checks that an object is a valid DuckDB connection
object.

## Usage

``` r
confirm_duckdb_con(con)
```

## Arguments

- con:

  Object to validate

## Details

This function is intended to be used within another function as input
validation. If the input is valid, nothing will happen. If it is not,
the function will throw a 'stop()' error.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 con <- db_connect()
 confirm_duckdb_con(con)
 confirm_duckdb_con("horse")
 }
} # }
```
