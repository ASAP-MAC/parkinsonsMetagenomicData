# Choose the most appropriate DuckDB view/table for filtering

'pick_projection' takes a data type and the name of a feature to filter
that data by and chooses the appropriate DuckDB view/table for
performing that filtering in an efficient way. If no view is tailored to
the specific feature, a view/table sorted by UUID will be the default.

## Usage

``` r
pick_projection(con, data_type, feature_name = "uuid")
```

## Arguments

- con:

  DuckDB connection object of class 'duckdb_connection'. This connection
  contains the views/tables to select from.

- data_type:

  Single string: value found in the data_type' column of
  output_file_types() and also as the name of a view found in
  DBI::dbListTables(con), indicating which view to collect data from.

- feature_name:

  Single string: the name of the feature that the file is intended to be
  filtered by. Default = "uuid"

## Value

Single string: the name of a DuckDB view/table

## See also

[`dbListTables`](https://dbi.r-dbi.org/reference/dbListTables.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
                          data_types = "relative_abundance")
 pick_projection(con, "relative_abundance", "clade_name_species")
 pick_projection(con, "relative_abundance")
 }
} # }
```
