# Parse basic MetaPhlAn output for a single sample as a TreeSummarizedExperiment object

'parse_metaphlan_list' reads a file obtained from running MetaPhlAn for
microbial profiling (with or without unclassified fraction estimation)
or viral sequence cluster analysis. This file is parsed into a
TreeSummarizedExperiment object.

## Usage

``` r
parse_metaphlan_list(sample_id, file_path, data_type)
```

## Arguments

- sample_id:

  String: A sample identifier

- file_path:

  String: Path to a locally stored MetaPhlAn output file in TSV format

- data_type:

  String: The type of MetaPhlAn output file to be parsed, either
  'relative_abundance' or 'viral_clusters'

## Value

A TreeSummarizedExperiment object with process metadata, row data,
column names, and relevant assays.

## Details

This function does not integrate sample metadata as column data. The
provided sample_id is used as the column name for assays within the
TreeSummarizedExperiment object and is intended to be used for
integration of sample metadata.

## See also

[`read_delim`](https://readr.tidyverse.org/reference/read_delim.html)
[`DataFrame-class`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html),
`S4VectorsOverview`
[`TreeSummarizedExperiment-class`](https://rdrr.io/pkg/TreeSummarizedExperiment/man/TreeSummarizedExperiment-class.html),
[`TreeSummarizedExperiment`](https://rdrr.io/pkg/TreeSummarizedExperiment/man/TreeSummarizedExperiment-constructor.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 fpath <- file.path(system.file("extdata",
                                package = "parkinsonsMetagenomicData"),
                    "sample_metaphlan_bugs_list.tsv.gz")
 parse_metaphlan_list(sample_id = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
                      file_path = fpath,
                      data_type = "relative_abundance")
 }
} # }
```
