# Return a TreeSummarizedExperiment with data based on sample data and feature data tables

'returnSamples' takes tables with sample and feature information and
retrieves the relevant data as a TreeSummarizedExperiment.

## Usage

``` r
returnSamples(
  data_type,
  sample_data = NULL,
  feature_data = NULL,
  repo = NULL,
  local_files = NULL,
  include_empty_samples = TRUE,
  dry_run = FALSE
)
```

## Arguments

- data_type:

  Single string: value found in the data_type' column of
  output_file_types() and also as part of the name of a view found in
  DBI::dbListTables(con), indicating which views to consider when
  collecting data.

- sample_data:

  Data frame: a table of sample metadata with a 'uuid' column. Often
  created by accessing 'sampleMetadata' and filtering or otherwise
  transforming the result to only include samples of interest.

- feature_data:

  Data frame: a table of feature data. Each column will become a
  filtering argument. Often created by accessing one of the files listed
  in 'get_ref_info()' with 'load_ref()', then filtering or otherwise
  transforming the result to only include feature combinations of
  interest.

- repo:

  String (optional): Hugging Face repo where the parquet files are
  stored. If NULL and local_files is also NULL, the repo listed as the
  default in get_repo_info() will be selected. Default: NULL

- local_files:

  String or vector of strings (optional): path(s) to parquet file(s).
  Default: NULL

- include_empty_samples:

  Boolean (optional): should samples provided via a 'uuid' argument
  within 'filter_values' be included in the final
  TreeSummarizedExperiment if they do not show up in the results from
  filtering the source parquet data file. Default: TRUE

- dry_run:

  Boolean (optional): if TRUE, the function will return the
  tbl_duckdb_connection object prior to calling 'dplyr::collect'.
  Default: FALSE

## Value

A TreeSummarizedExperiment object with process metadata, row data,
column names, and relevant assays. If dry_run = TRUE, a
tbl_duckdb_connection object.

## Details

Files stored remotely and locally cannot be combined in the same
connection.

## See also

[`dbListTables`](https://dbi.r-dbi.org/reference/dbListTables.html),
[`dbDisconnect`](https://dbi.r-dbi.org/reference/dbDisconnect.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 table(sampleMetadata$control, useNA = "ifany")
 sample_data <- sampleMetadata %>%
     filter(control %in% c("Case", "Study Control") &
            age >= 16 &
            is.na(sex) != TRUE)
 sample_data_small <- sample_data[1:15,]

 clade_name_ref <- load_ref("clade_name_ref")
 feature_data_genus <- clade_name_ref %>%
     filter(grepl("Faecalibacterium", clade_name_genus)) %>%
     select(clade_name_genus)

 genus_ex <- returnSamples(data_type = "relative_abundance",
                           sample_data = sample_data_small,
                           feature_data = feature_data_genus)
 genus_ex
 }
} # }
```
