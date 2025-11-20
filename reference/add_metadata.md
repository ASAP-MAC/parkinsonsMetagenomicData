# Add sample metadata to TreeSummarizedExperiment object as colData

'add_metadata' uses the IDs of samples included in a
TreeSummarizedExperiment object to attach sample metadata as colData.
This new metadata can combine with pre-existing colData.

## Usage

``` r
add_metadata(sample_ids, id_col = "uuid", experiment, method = "append")
```

## Arguments

- sample_ids:

  Vector of strings: sample IDs, such as UUIDs, that will be used to
  attach samples to their sampleMetadata

- id_col:

  String: column name within sampleMetadata where 'sample_ids' will be
  found, Default: 'uuid'

- experiment:

  TreeSummarizedExperiment object: contains samples to add metadata to

- method:

  String: 'append', 'overwrite', or 'ignore', indicating how to handle
  duplicate colData columns, Default: 'append'

## Value

TreeSummarizedExperiment object with sampleMetadata stored as colData

## Details

sampleMetadata columns found to have the same name as pre-existing
colData columns can be either appended (preserving both duplicate
columns), used to overwrite the pre-existing columns (leaving only the
new version of the duplicate columns), or ignored (leaving only the old
version of the duplicate columns)

## See also

[`DataFrame-class`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html),
`S4VectorsOverview`
[`TreeSummarizedExperiment-class`](https://rdrr.io/pkg/TreeSummarizedExperiment/man/TreeSummarizedExperiment-class.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 fpath <- file.path(system.file("extdata",
                                package = "parkinsonsMetagenomicData"),
                    "sample_experiment.Rds")
 sample_experiment <- readRDS(fpath)

 add_metadata(sample_ids = colnames(sample_experiment),
              id_col = "uuid",
              experiment = sample_experiment)
 }
} # }
```
