# Create database views for all available or requested data types

'retrieve_views' creates database views for all of the data types
available in a repo of interest (see inst/extdata/parquet_repos.csv). an
individual type or vector of types may also be requested to avoid
unwanted views.

## Usage

``` r
retrieve_views(con, repo = NULL, data_types = NULL)
```

## Arguments

- con:

  DuckDB connection object of class 'duckdb_connection'

- repo:

  String (optional): Hugging Face repo where the parquet files are
  stored. If NULL, the repo listed as the default in get_repo_info()
  will be selected. Default: NULL

- data_types:

  Character vector (optional): list of data types to establish database
  views for. If NULL, views will be created for all available data
  types. Default: NULL

## Details

'retrieve_views' uses 'output_file_types' as the initial list of data
types to retrieve, and checks if they exist as parquet files in the repo
of interest. If they do not, they are simply skipped and the user is
notified.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 con <- db_connect()

 retrieve_views(con, repo = "waldronlab/metagenomics_mac",
                data_types = c("relative_abundance",
                               "viral_clusters",
                               "pathcoverage_unstratified"))
 DBI::dbListTables(con)
 }
} # }
```
