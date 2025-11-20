# Validate 'filter_values' argument

'confirm_filter_values' checks that a named list is valid to be used as
a 'filter_values' argument for various functions in
parkinsonsMetagenomicData. Specifically, the input should be a named
list, where the element name equals the name of a column to be filtered
and element value equals a vector of exact column values. Base usage of
'confirm_filter_values' just confirms that the object is a named list,
and if any of the elements are named 'uuid', validates that the values
of that element are valid uuids. If a vector of available features is
provided, the names of the elements of the list will be compared to that
vector.

## Usage

``` r
confirm_filter_values(filter_values, available_features = NULL)
```

## Arguments

- filter_values:

  Named list: input to be validated

- available_features:

  Character vector: features that the list element names should be found
  in. Default: NULL

## Details

This function is intended to be used within another function as input
validation. If the input is valid, nothing will happen. If it is not,
the function will throw a 'stop()' error.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 l1 <- list(uuid = "56aa2ad5-007d-407c-a644-48aac1e9a8f0", animals = c("frog", "horse"))
 l2 <- list(uuid = "blue")
 confirm_filter_values(l1)
 confirm_filter_values(l1, c("uuid", "animals", "shapes"))
 confirm_filter_values(l1, c("animals", "shapes"))
 confirm_filter_values(l2)
 }
} # }
```
