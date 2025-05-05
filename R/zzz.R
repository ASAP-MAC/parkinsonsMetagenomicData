.onLoad <- function(libname, pkgname) {
    googleCloudStorageR::gcs_global_bucket("metagenomics-mac")
}
