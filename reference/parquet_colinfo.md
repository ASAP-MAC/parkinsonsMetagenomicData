# Retrieve column info for parquet files based on original file type

'parquet_colinfo' returns the column info associated with a parquet file
made from a particular output file type.

## Usage

``` r
parquet_colinfo(data_type)
```

## Arguments

- data_type:

  Single string: value found in the data_type' column of
  output_file_types() and also as part of the name of a file in the repo
  https://huggingface.co/datasets/waldronlab/metagenomics_mac or
  https://huggingface.co/datasets/waldronlab/metagenomics_mac_examples.

## Value

Data frame with columns 'general_data_type', 'col_name', 'col_class',
'description', 'se_role', and 'position'

## See also

[`read_delim`](https://readr.tidyverse.org/reference/read_delim.html)
[`filter`](https://dplyr.tidyverse.org/reference/filter.html),
[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 parquet_colinfo("viral_clusters")
 }
} # }
```
