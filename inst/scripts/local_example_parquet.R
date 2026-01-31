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

## pathway_ref
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "pathway_ref.parquet")

download.file("https://huggingface.co/datasets/waldronlab/metagenomics_mac_examples/resolve/main/pathway_ref.parquet?download=true",
              destfile = extdata_path,
              method = "wget")

full_file <- read_parquet(extdata_path)
part_file <- full_file[50:200,]

write_parquet(part_file, extdata_path)
