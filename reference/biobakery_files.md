# Read in extdata/biobakery_file_definitions.csv

'biobakery_files' reads in the table
extdata/biobakery_file_definitions.csv.

## Usage

``` r
biobakery_files()
```

## Value

Tibble with columns 'DataType', 'Tool', 'Description', and
Units/Normalization'

## Details

DETAILS

## See also

[`read_delim`](https://readr.tidyverse.org/reference/read_delim.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 biobakery_files()
 }
} # }
```
