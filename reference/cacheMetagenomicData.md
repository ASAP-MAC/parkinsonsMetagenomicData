# Retrieve and cache output files

'cacheMetagenomicData' takes UUID and data type arguments, downloads the
corresponding output files, and stores them in a local
parkinsonsMetagenomicData cache. If the same files are requested again
through this function, they will not be re-downloaded unless explicitly
specified, in order to reduce excessive downloads.

## Usage

``` r
cacheMetagenomicData(
  uuids,
  data_type = "relative_abundance",
  redownload = "no",
  custom_cache = NULL
)
```

## Arguments

- uuids:

  Vector of strings: sample UUID(s) to get output for

- data_type:

  Single string: value found in the data_type' column of
  output_file_types(), indicating which output files to get, Default:
  'relative_abundance'

- redownload:

  String: "yes", "no", or "ask"; should the function re-download a file
  that is already present in the cache, Default: 'no'

- custom_cache:

  BiocFileCache object: a custom cache object may be specified instead
  of the default created by pMD_get_cache(), Default: NULL

## Value

A tibble with information on the cached files, including UUID, data
type, Google Cloud Bucket object name, local cache ID, and cached file
path

## See also

[`str_split`](https://stringr.tidyverse.org/reference/str_split.html)
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 cacheMetagenomicData(uuid = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
                      data_type = "relative_abundance",
                      redownload = "ask")
 }
} # }
```
