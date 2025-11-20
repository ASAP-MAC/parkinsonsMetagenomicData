# Convert tabulated parquet file data to a Summarized Experiment

'parquet_to_tse' takes tabulated data from a parquet file to a
Summarized Experiment object. Associated sample metadata is
automatically attached as colData.

## Usage

``` r
parquet_to_tse(parquet_table, data_type, empty_data = NULL)
```

## Arguments

- parquet_table:

  Table or data frame: data taken directly from a parquet file found in
  the repo of interest (see inst/extdata/parquet_repos.csv).

- data_type:

  Single string: value found in the data_type' column of
  output_file_types() and also as part of the name of a file in the repo
  of interest.

## Value

A TreeSummarizedExperiment object with process metadata, row data,
column names, and relevant assays.

## See also

`c("rowwise", "rowwise")`,
[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html),
[`select`](https://dplyr.tidyverse.org/reference/select.html)
[`pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
[`rownames`](https://tibble.tidyverse.org/reference/rownames.html)
[`DataFrame-class`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html),
`S4VectorsOverview`
[`TreeSummarizedExperiment-class`](https://rdrr.io/pkg/TreeSummarizedExperiment/man/TreeSummarizedExperiment-class.html),
[`TreeSummarizedExperiment`](https://rdrr.io/pkg/TreeSummarizedExperiment/man/TreeSummarizedExperiment-constructor.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
                          data_types = "pathcoverage_unstratified")
 parquet_tbl <- tbl(con, "pathcoverage_unstratified_uuid") |> collect()

 se <- parquet_to_tse(parquet_tbl, "pathcoverage_unstratified")
 se
 }
} # }
```
