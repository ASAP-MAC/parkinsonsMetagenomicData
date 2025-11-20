# Parse HUMAnN output for a single sample as a TreeSummarizedExperiment object

'parse_humann' reads a file obtained from running HUMAnN on a single
sample. This file is parsed into a TreeSummarizedExperiment object.

## Usage

``` r
parse_humann(sample_id, file_path, data_type)
```

## Arguments

- sample_id:

  String: A sample identifier

- file_path:

  String: Path to a locally stored HUMAnN output file in gzipped TSV
  format

- data_type:

  String: The type of HUMAnN output file to be parsed, as found in
  output_file_types("tool", "humann")

## Value

A TreeSummarizedExperiment object with process metadata, row names,
column names, and relevant assays.

## Details

This function does not integrate sample metadata as column data. The
provided sample_id is used as the column name for assays within the
TreeSummarizedExperiment object and is intended to be used for
integration of sample metadata.

## See also

[`read_delim`](https://readr.tidyverse.org/reference/read_delim.html)
[`DataFrame-class`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html)
[`TreeSummarizedExperiment-class`](https://rdrr.io/pkg/TreeSummarizedExperiment/man/TreeSummarizedExperiment-class.html),
[`TreeSummarizedExperiment`](https://rdrr.io/pkg/TreeSummarizedExperiment/man/TreeSummarizedExperiment-constructor.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 #EXAMPLE1
 }
} # }
```
