# Return a table with information about available parquet reference files.

'get_ref_info' returns a table of information associated with each
parquet reference file. The table can optionally be filtered by
providing a column name to filter by and a string/regular expression to
filter the selected column with.

## Usage

``` r
get_ref_info(filter_col = NULL, filter_string = NULL)
```

## Arguments

- filter_col:

  Character string (optional): name of the column to filter by

- filter_string:

  Character string (optional): string/regular expression to filter the
  selected column with.

## Value

Data frame: A table of ref information, including information on general
data types and tools served as well as descriptions.

## See also

[`read_delim`](https://readr.tidyverse.org/reference/read_delim.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 get_ref_info()
 }
} # }
```
