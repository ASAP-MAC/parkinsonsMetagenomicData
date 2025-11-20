# Retrieve Google Bucket locators for output

'get_bucket_locators' gives the names of objects within the Google
Bucket gs://metagenomics-mac. Output is requested by associated sample
UUID and type of output file.

## Usage

``` r
get_bucket_locators(uuids, data_type = "relative_abundance")
```

## Arguments

- uuids:

  Vector of strings: sample UUID(s) to get output for

- data_type:

  Single string: value found in the data_type' column of
  output_file_types(), indicating which output files to get, Default:
  'relative_abundance'

## Value

Vector of strings: names of requested Google Bucket objects

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 get_bucket_locators(uuids = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
                     data_type = "relative_abundance")
 }
} # }
```
