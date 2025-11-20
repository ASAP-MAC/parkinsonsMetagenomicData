# Load a single parquet reference file

'load_ref' retrieves a single parquet file by name from a designated
repo and loads it into a table in R.

## Usage

``` r
load_ref(ref, repo = NULL)
```

## Arguments

- ref:

  String: the name of a reference file as found in get_ref_info()

- repo:

  String (optional): Hugging Face repo where the parquet files are
  stored. If NULL, the repo listed as the default in get_repo_info()
  will be selected. Default: NULL

## Value

A table of reference information

## See also

[`filter`](https://dplyr.tidyverse.org/reference/filter.html),
[`pull`](https://dplyr.tidyverse.org/reference/pull.html)
[`read_parquet`](https://arrow.apache.org/docs/r/reference/read_parquet.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 load_ref("clade_name_ref")
 }
} # }
```
