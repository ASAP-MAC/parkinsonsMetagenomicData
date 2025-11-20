# Convert standard https:// URLs to httpfs-compatible hf:// URLs

'file_to_hf' converts standard https:// URLs representing files in a
Hugging Face repo to URLs compatible with httpfs as described in the
[DuckDB
Docs](https://duckdb.org/docs/stable/core_extensions/httpfs/hugging_face.html)

## Usage

``` r
file_to_hf(url)
```

## Arguments

- url:

  String: a URL referencing a single file in a Hugging Face repo.

## Value

String: a URL referencing the same file in a format matching the httpfs
protocol.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 file_to_hf("https://huggingface.co/datasets/waldronlab/metagenomics_mac/resolve/main/relative_abundance.parquet")
 }
} # }
```
