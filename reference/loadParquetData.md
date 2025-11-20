# Retrieve data from a DuckDB view and convert to Summarized Experiment

'loadParquetData' accesses a DuckDB view created by 'accessParquetData'
and loads it into R as a Summarized Experiment object. Arguments can be
provided to filter or transform the DuckDB view. To filter, provide a
named list (format: name = column name, value = single exact value or
vector of exact values) to filter_values. To further transform the view,
provide a saved sequence of function calls starting with dplyr::tbl to
custom_view.

## Usage

``` r
loadParquetData(
  con,
  data_type,
  filter_values = NULL,
  custom_view = NULL,
  include_empty_samples = FALSE,
  dry_run = FALSE
)
```

## Arguments

- con:

  DuckDB connection object of class 'duckdb_connection'

- data_type:

  Single string: value found in the data_type' column of
  output_file_types() and also as part of the name of a view found in
  DBI::dbListTables(con), indicating which views to consider when
  collecting data.

- filter_values:

  Named list: element name equals the column name to be filtered and
  element value equals a vector of exact column values. Default: NULL

- custom_view:

  Saved object with the initial class 'tbl_duckdb_connection'
  (optional): DuckDB tables/views can be accessed with with the
  'dplyr::tbl' function, and piped into additional functions such as
  'dplyr::filter' prior to loading into memory with 'dplyr::collect'. A
  particular sequence of function calls can be saved and provided to
  this function for collection and formatting as a Summarized
  Experiment. See the function example. Default: NULL

- include_empty_samples:

  Boolean (optional): should samples provided via a 'uuid' argument
  within 'filter_values' be included in the final
  TreeSummarizedExperiment if they do not show up in the results from
  filtering the source parquet data file. Default: FALSE

- dry_run:

  Boolean (optional): if TRUE, the function will return the
  tbl_duckdb_connection object prior to calling 'dplyr::collect'.
  Default: FALSE

## Value

A TreeSummarizedExperiment object with process metadata, row data,
column names, and relevant assays. If dry_run = TRUE, a
tbl_duckdb_connection object.

## Details

If 'custom_view' is provided, it must use one of the views indicated by
data_type'.

## See also

[`dbListTables`](https://dbi.r-dbi.org/reference/dbListTables.html)
[`tbl`](https://dplyr.tidyverse.org/reference/tbl.html),
[`filter`](https://dplyr.tidyverse.org/reference/filter.html),
[`compute`](https://dplyr.tidyverse.org/reference/compute.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
                          data_types = "pathcoverage_unstratified")

 custom_filter <- tbl(con, "pathcoverage_unstratified_pathway") |>
                  filter(grepl("UMP biosynthesis", pathway))

 custom_tse <- loadParquetData(con,
                               data_type = "pathcoverage_unstratified",
                               filter_values = list(uuid = c("8793b1dc-3ba1-4591-82b8-4297adcfa1d7",
                                                             "cc1f30a0-45d9-41b1-b592-7d0892919ee7",
                                                             "fb7e8210-002a-4554-b265-873c4003e25f",
                                                             "d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
                                                             "4985aa08-6138-4146-8ae3-952716575395",
                                                             "8eb9f7ae-88c2-44e5-967e-fe7f6090c7af")),
                               custom_view = custom_filter)
 custom_tse
 }
} # }
```
