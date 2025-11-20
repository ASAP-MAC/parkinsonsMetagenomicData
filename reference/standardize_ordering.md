# Standardize the order of a vector of delimited strings

'standardize_ordering' takes a vector of strings, splits each of them on
a specified delimiter, orders them, then re-collapses them with the same
delimiter. This confirms that when calling unique(), there are no
strings that contain the same elements but in a different order.

## Usage

``` r
standardize_ordering(vec, delim)
```

## Arguments

- vec:

  Vector of strings: vector of strings to be standardized

- delim:

  Character: delimiter to split each of the strings by.

## Value

Vector of strings

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 vec <- c("horse|gecko|frog",
          "cow|camel|fish",
          "frog|gecko|horse")

 standardize_ordering(vec, delim = "|")
 }
} # }
```
