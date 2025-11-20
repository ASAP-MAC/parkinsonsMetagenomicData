# Merge TreeSummarizedExperiment objects with the same assay types together

'mergeExperiments' takes a list of TreeSummarizedExperiment objects with
the same assays but different samples, and combines them into a single
TreeSummarizedExperiment object.

## Usage

``` r
mergeExperiments(merge_list)
```

## Arguments

- merge_list:

  List of TreeSummarizedExperiment objects: to be merged into a single
  TreeSummarizedExperiment object

## Value

TreeSummarizedExperiment object with multiple samples

## Details

The assays contained in the TreeSummarizedExperiments to be merged must
be of the same type (i.e. have the same name) and be in the same order
if there are multiple assays.

## See also

[`map`](https://purrr.tidyverse.org/reference/map.html),
[`reduce`](https://purrr.tidyverse.org/reference/reduce.html)
[`TreeSummarizedExperiment-class`](https://rdrr.io/pkg/TreeSummarizedExperiment/man/TreeSummarizedExperiment-class.html),
`c("TreeSummarizedExperiment-class", "TreeSummarizedExperiment")`
[`rownames`](https://tibble.tidyverse.org/reference/rownames.html)
[`mutate-joins`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html),
[`across`](https://dplyr.tidyverse.org/reference/across.html),
[`bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)
[`everything`](https://tidyselect.r-lib.org/reference/everything.html)
[`replace_na`](https://tidyr.tidyverse.org/reference/replace_na.html)
[`SimpleList-class`](https://rdrr.io/pkg/S4Vectors/man/SimpleList-class.html),
`c("DataFrame-class", "S4VectorsOverview")`
[`extract`](https://magrittr.tidyverse.org/reference/aliases.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 fpath <- file.path(system.file("extdata",
                                package = "parkinsonsMetagenomicData"),
                    "sample_experiment_list.Rds")
 sample_experiment_list <- readRDS(fpath)

 mergeExperiments(sample_experiment_list)
 }
} # }
```
