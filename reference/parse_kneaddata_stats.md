# Parse KneadData log file for a single sample as a named vector

'parse_kneaddata_stats' reads a file obtained from running KneadData on
a FASTQ file. This file is parsed into a named vector.

## Usage

``` r
parse_kneaddata_stats(file_path)
```

## Arguments

- file_path:

  String: Path to a locally stored KneadData log file stored in TXT
  format

## Value

A named vector containing a number of read counts at different stages

## Details

'parse_kneaddata_stats' does not parse every single item of information
within the out_kneaddata.log file, but gathers only the lines with read
counts.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 fpath <- file.path(system.file("extdata",
                                package = "parkinsonsMetagenomicData"),
                    "sample_out_kneaddata.log")
 parse_kneaddata_stats(fpath)
 }
} # }
```
