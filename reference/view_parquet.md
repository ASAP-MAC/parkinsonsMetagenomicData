# Create a database view of a specific parquet file

'view_parquet' creates a database view with the provided DuckDB
connection object. The view is created from a parquet file hosted at a
repo of interest (see inst/extdata/parquet_repos.csv) or stored locally.
The specific file is specified via the httpfs-compatible URL or local
file path.

## Usage

``` r
view_parquet(con, httpfs_url = NULL, file_path = NULL, view_name = NULL)
```

## Arguments

- con:

  DuckDB connection object of class 'duckdb_connection'

- httpfs_url:

  String (optional): httpfs-compatible URL referencing a specific
  parquet file hosted in a repo of interest. Default: NULL

- file_path:

  String (optional): path to locally stored parquet file. Default: NULL

- view_name:

  String (optional): name of the database view to be created. If not
  provided, it will be generated from the name of the file indicated by
  'httpfs_url'. Default: NULL

## Details

See [DuckDB
Docs](https://duckdb.org/docs/stable/core_extensions/httpfs/hugging_face.html)
for more information on httpfs-compatible URLs.

## See also

[`dbExecute`](https://dbi.r-dbi.org/reference/dbExecute.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 con <- db_connect()
 view_parquet(con,
              "hf://datasets/waldronlab/metagenomics_mac/relative_abundance_uuid.parquet",
              "relative_abundance_uuid")

 DBI::dbListTables(con)
 }
} # }
```
