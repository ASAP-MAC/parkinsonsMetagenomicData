# Return all "extensions" from a file path

'get_exts' returns the extension of a file name or path, including
pseudo-extensions such as ".tsv" in "file.tsv.gz".

## Usage

``` r
get_exts(file_path)
```

## Arguments

- file_path:

  String: file name or path to get extension(s) from

## Value

String: file extension, including pseudo-extensions

## See also

[`str_split`](https://stringr.tidyverse.org/reference/str_split.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 get_exts("path/file.tsv.gz")
 }
} # }
```
