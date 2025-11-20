# Load cached files into R as a merged TreeSummarizedExperiment object

'loadMetagenomicData' takes a table of information about cached files,
including paths to the cached files as well as associated UUIDs and data
types, and loads the files into R as TreeSummarizedExperiment objects.
Associated sample metadata is automatically attached as colData, and the
objects are merged into a single TreeSummarizedExperiment object.

## Usage

``` r
loadMetagenomicData(cache_table)
```

## Arguments

- cache_table:

  A data.frame or tibble: structured like cacheMetagenomicData() output;
  contains the columns 'uuid', 'cache_path', and 'data_type', with
  appropriate entries for each file to be loaded in.

## Value

A TreeSummarizedExperiment object with relevant sample metadata attached
as colData.

## Details

At the moment, only the `metaphlan_lists` data types, `viral_clusters`
and `relative_abundance`, as well as the `humann` data types, have
parsing functions for automatically loading into
TreeSummarizedExperiment objects.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 cache_table <- cacheMetagenomicData(uuid = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
                                     data_type = "relative_abundance")
 loadMetagenomicData(cache_table = cache_table)
 }
} # }
```
