## Downloading small parquet files for local function examples

library(parkinsonsMetagenomicData)

## pathcoverage_unstratified files:
# pathcoverage_unstratified_pathway.parquet
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "pathcoverage_unstratified_pathway.parquet")

download.file("https://huggingface.co/datasets/waldronlab/metagenomics_mac_examples/resolve/main/pathcoverage_unstratified_pathway.parquet?download=true",
              destfile = extdata_path,
              method = "wget")

# pathcoverage_unstratified_uuid.parquet
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "pathcoverage_unstratified_uuid.parquet")

download.file("https://huggingface.co/datasets/waldronlab/metagenomics_mac_examples/resolve/main/pathcoverage_unstratified_uuid.parquet?download=true",
              destfile = extdata_path,
              method = "wget")

