# Validate UUIDs

'confirm_uuids' checks that a single string or vector of strings are
valid UUIDs.

## Usage

``` r
confirm_uuids(uuids)
```

## Arguments

- uuids:

  String or character vector: strings to validate

## Details

This function is intended to be used within another function as input
validation. If the input is valid, nothing will happen. If it is not,
the function will throw a 'stop()' error.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 confirm_uuids("56aa2ad5-007d-407c-a644-48aac1e9a8f0")
 confirm_uuids("horse")
 }
} # }
```
