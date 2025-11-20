# Return a table with information about available Hugging Face repos.

'get_repo_info' returns a table of information associated with each
Hugging Face repo that contains relevant parquet files.

## Usage

``` r
get_repo_info()
```

## Value

Data frame: A table of repo information, including information on
overall organization, name, URL, and whether or not the repo is the
selected default.

## See also

[`read_delim`](https://readr.tidyverse.org/reference/read_delim.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 get_repo_info()
 }
} # }
```
