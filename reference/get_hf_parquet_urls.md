# Get Parquet File URLs and Metadata from a Hugging Face Repository

This function queries the Hugging Face Hub API to find all Parquet files
within a specified dataset repository. It constructs the direct download
URLs and then joins this information with a local file containing
definitions for BioBakery data types.

## Usage

``` r
get_hf_parquet_urls(repo_name = NULL)
```

## Arguments

- repo_name:

  A character string specifying the Hugging Face dataset repository name
  in the format "user/repo" or "org/repo". If NULL, the repo listed as
  the default in get_repo_info() will be selected. Default: NULL

## Value

A data.frame with the following columns:

- filename:

  The name of the Parquet file.

- URL:

  The full download URL for the file.

- DataType:

  The base name of the file, used for joining with metadata.

- Tool:

  The bioBakery tool that typically produces the data type.

- Description:

  A brief description of the data type.

- Units.Normalization:

  The units or normalization method used.

## Details

The metadata is sourced from the "biobakery-file-definitions.csv" file,
which is expected to be in the `inst/extdata` directory of the
`parkinsonsMetagenomicData` package. If this package is not available,
the metadata columns will be populated with `NA`.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 file_info <- get_hf_parquet_urls()
 head(file_info)
 }
} # }
```
