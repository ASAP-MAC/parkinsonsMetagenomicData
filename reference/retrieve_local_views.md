# Create database views from local parquet files

'retrieve_local_views' creates database views for each of the files
provided. If view names are not also provided, they are imputed from the
file names.

## Usage

``` r
retrieve_local_views(con, local_files, view_names = NULL)
```

## Arguments

- con:

  DuckDB connection object of class 'duckdb_connection'

- local_files:

  String or vector of strings: path(s) to parquet file(s)

- view_names:

  String or vector of strings (optional): names to use for the created
  views instead of imputing from file names, Default: NULL

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 con <- db_connect(dbdir = ":memory:")
 fpath <- file.path(system.file("extdata",
                                package = "parkinsonsMetagenomicData"),
                    "sample_table.parquet")
 retrieve_local_views(con, fpath)
 DBI::dbListTables(con)
 }
} # }
```
