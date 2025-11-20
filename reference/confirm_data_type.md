# Validate 'data_type' argument

'confirm_data_type' checks that a single string is valid to be used as a
'data_type' argument for various functions in parkinsonsMetagenomicData.
Specifically, the input should be a single value that is found in the
'data_type' column of the output data frame from output_file_types().
The allowed values can optionally be restricted by providing a column
name and string/regular expression to filter the output_file_types()
data frame with.

## Usage

``` r
confirm_data_type(data_type, filter_col = NULL, filter_string = NULL)
```

## Arguments

- data_type:

  String: input to be validated

- filter_col:

  String (optional): name of column to filter by

- filter_string:

  String (optional): string to filter for within 'filter_col'

## Details

This function is intended to be used within another function as input
validation. If the input is valid, nothing will happen. If it is not,
the function will throw a 'stop()' error.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 confirm_data_type("relative_abundance")
 confirm_data_type("relative_abundance", "tool", "humann")
 confirm_data_type(c("relative_abundance", "viral_clusters"))
 confirm_data_type("horse")
 }
} # }
```
