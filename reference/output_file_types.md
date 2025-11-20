# Read in extdata/output_files.csv

'output_file_types' reads in the table extdata/output_files.csv. The
table can optionally be filtered by providing a column name to filter by
and a string/regular expression to filter the selected column with.

## Usage

``` r
output_file_types(filter_col = NULL, filter_string = NULL)
```

## Arguments

- filter_col:

  Character string (optional): name of the column to filter by

- filter_string:

  Character string (optional): string/regular expression to filter the
  selected column with.

## Value

Tibble with columns 'tool', 'data_type', 'file_name', and 'subdir'

## See also

[`read_delim`](https://readr.tidyverse.org/reference/read_delim.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 output_file_types()
 output_file_types("tool", "metaphlan")
 }
} # }
```
