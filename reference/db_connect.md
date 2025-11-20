# Connect to DuckDB database instance

'db_connect' establishes a DuckDB connection in the location specified
or in memory, and confirms that the 'httpfs' extension is installed.

## Usage

``` r
db_connect(dbdir = ":memory:")
```

## Arguments

- dbdir:

  Location for database files. Should be a path to an existing directory
  in the file system or the value ':memory:' to keep data in RAM.
  Default: ':memory:'

## Value

DuckDB connection object of class 'duckdb_connection'

## See also

[`dbConnect`](https://dbi.r-dbi.org/reference/dbConnect.html),
[`dbExecute`](https://dbi.r-dbi.org/reference/dbExecute.html)
[`duckdb`](https://r.duckdb.org/reference/duckdb.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 connection <- db_connect(dbdir = ":memory:")
 class(connection)
 }
} # }
```
