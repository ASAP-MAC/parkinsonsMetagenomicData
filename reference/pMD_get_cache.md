# Get location of dedicated file cache

'pMD_get_cache' returns the location of the dedicated
parkinsonsMetagenomicData file cache or creates it if it does not exist.

## Usage

``` r
pMD_get_cache()
```

## Value

BiocFileCache cache object

## See also

`userdir`
[`BiocFileCache-class`](https://rdrr.io/pkg/BiocFileCache/man/BiocFileCache-class.html),
[`BiocFileCache`](https://rdrr.io/pkg/BiocFileCache/man/BiocFileCache-class.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 pMD_get_cache()
 }
} # }
```
