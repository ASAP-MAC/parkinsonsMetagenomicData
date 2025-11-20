# Set up DuckDB connection with views for available data types

'accessParquetData' is a wrapper function for 'db_connect' and
'retrieve_views'. A DuckDB connection is established and views are
created for either all provided local files or all data types available
in a repo of interest (see inst/extdata/parquet_repos.csv). When using a
remote repo, a vector of specific data types can be supplied as doing
this for all data types can take longer.

## Usage

``` r
accessParquetData(
  dbdir = ":memory:",
  repo = NULL,
  local_files = NULL,
  data_types = NULL
)
```

## Arguments

- dbdir:

  Location for database files. Should be a path to an existing directory
  in the file system or the value ':memory:' to keep data in RAM.
  Default: ':memory:'

- repo:

  String (optional): Hugging Face repo where the parquet files are
  stored. If NULL and local_files is also NULL, the repo listed as the
  default in get_repo_info() will be selected. Default: NULL

- local_files:

  String or vector of strings (optional): path(s) to parquet file(s).
  Default: NULL

- data_types:

  Character vector (optional): when using a remote repo, a list of data
  types to establish database views for. If NULL, views will be created
  for all available data types. Default: NULL

## Value

DuckDB connection object of class 'duckdb_connection'

## Details

Files stored remotely and locally cannot be combined in the same
connection.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 prepared_db <- accessParquetData()
 DBI::dbListTables(prepared_db)

 single_type <- accessParquetData(data_types = "pathcoverage_unstratified")
 DBI::dbListTables(single_type)
 }
} # }
```
