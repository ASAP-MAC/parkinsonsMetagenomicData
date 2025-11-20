# Validate 'repo' argument

'confirm_repo' checks that a single string is a valid repo name as
listed in get_repo_info() or a NULL value.

'confirm_ref' checks that a single string is a valid reference file name
as listed in get_ref_info() or a NULL value.

## Usage

``` r
confirm_repo(repo)

confirm_ref(ref)
```

## Arguments

- repo:

  String: input to be validated

## Details

This function is intended to be used within another function as input
validation. If the input is valid, nothing will happen. If it is not,
the function will throw a 'stop()' error.

This function is intended to be used within another function as input
validation. If the input is valid, nothing will happen. If it is not,
the function will throw a 'stop()' error.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 confirm_repo(NULL)
 confirm_repo("horse")
 confirm_repo("waldronlab/metagenomics_mac")
 }
} # }
if (FALSE) { # \dontrun{
if(interactive()){
 confirm_ref("horse")
 confirm_ref("clade_name_ref")
 }
} # }
```
