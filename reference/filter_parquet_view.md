# Filter a database view by any number of column:value argument pairs

'filter_parquet_view' takes a named list of exact value filter arguments
and applies them to a DuckDB database view or table. This function
applies the filters in the order provided, ensuring that the most
efficient filtering method is used for the first provided condition.

## Usage

``` r
filter_parquet_view(view, filter_values)
```

## Arguments

- view:

  DuckDB database view or table: obtained by calling
  tbl(duckdb_connection, view_name).

- filter_values:

  Named list: element name equals the column name to be filtered and
  element value equals a vector of exact column values.

## Value

A filtered DuckDB database view or table. This is still lazy until
collect() is called.

## Details

Optimization for large files is done by filtering first by the column
with the least provided values. This is the most selective filtering
step and will therefore reduce the amount of data that is being filtered
with subsequent column:value arguments. Because of this, it is ideal to
ensure that the provided DuckDB view or table is sorted by the column
involved in the first filter condition. If you have multiple views with
different sorting schemas, interpret_and_filter() will select the
appropriate view and apply the filter for you.

## See also

[`filter`](https://dplyr.tidyverse.org/reference/filter.html),
[`setops`](https://dplyr.tidyverse.org/reference/setops.html)
[`sym`](https://rlang.r-lib.org/reference/sym.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
                          data_types = "genefamilies_stratified")
 fvalues <- list(uuid = c("d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
                          "38d449c8-1462-4d30-ba87-d032d95942ce",
                          "5f8d4254-7653-46e3-814e-ed72cdfcb4d0"),
                 gene_family_uniref = c("UniRef90_R6K8T6",
                                       "UniRef90_B0PDE3"))

 filter_parquet_view(view = tbl(con, "genefamilies_stratified_uuid"),
                filter_values = fvalues)
 }
} # }
```
