# Assign Google Cloud Bucket name
.onLoad <- function(libname, pkgname) {
    googleCloudStorageR::gcs_global_bucket("metagenomics-mac") |>
        suppressMessages()
}

# Declare global variables
utils::globalVariables(c(
    ".",
    ".data",
    "sampleMetadata"
))
