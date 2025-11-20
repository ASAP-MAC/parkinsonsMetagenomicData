# Parse FastQC data file for a single sample as a named vector

'parse_fastqc_stats' reads a file obtained from running FastQC on a
FASTQ file. This file is parsed into a named vector.

## Usage

``` r
parse_fastqc_stats(file_path)
```

## Arguments

- file_path:

  String: Path to a locally stored MetaPhlAn output file in TXT format

## Value

A named vector containing a number of sample-level statistics.

## Details

'parse_fastqc_stats' does not parse every single item of information
within the fastqc_data.txt file, but gathers only the 'Basic Statistics'
module.

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 fpath <- file.path(system.file("extdata",
                                package = "parkinsonsMetagenomicData"),
                    "sample_fastqc_data.txt")
 parse_fastqc_stats(fpath)
 }
} # }
```
