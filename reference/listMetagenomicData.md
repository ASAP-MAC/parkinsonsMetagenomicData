# List metagenomic data available for download

'listMetagenomicData' provides a table of all objects within the
'gs://metagenomics-mac' Google Cloud Bucket that are available for
download.

## Usage

``` r
listMetagenomicData()
```

## Value

Tibble: information on the available files, including UUID, data type,
Google Cloud Bucket object name, object size, and time the object was
last updated

## See also

[`gcs_list_objects`](https://cloudyr.github.io/googleCloudStorageR//reference/gcs_list_objects.html)
[`read_delim`](https://readr.tidyverse.org/reference/read_delim.html)
[`filter`](https://dplyr.tidyverse.org/reference/filter.html)
[`str_detect`](https://stringr.tidyverse.org/reference/str_detect.html),
[`str_split`](https://stringr.tidyverse.org/reference/str_split.html)
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 listMetagenomicData()
 }
} # }
```
