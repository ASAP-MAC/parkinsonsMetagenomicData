# Detect which accepted data type a string is referring to

'detect_data_type' parses the longest value matching a value in
output_file_types()\$data_type.

## Usage

``` r
detect_data_type(string)
```

## Arguments

- string:

  String(s): a single string or vector of strings to parse

## Value

String(s): detected data type values

## See also

[`str_extract`](https://stringr.tidyverse.org/reference/str_extract.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 detect_data_type("genefamilies_cpm_but_sorted")
 }
} # }
```
