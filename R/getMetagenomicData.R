#### Download raw output files from cloud storage
# set up file caching

pMD_get_cache <- function() {
    ## Create a directory for cached data
    cache <- tools::R_user_dir("parkinsonsMetagenomicData", "cache")

    ## Directory path of cache
    BiocFileCache::BiocFileCache(cache = cache)
}

get_metaphlan_locator <- function(uuid, data_type) {
    allowed_types <- c("bugs", "viruses", "unknown")

    ## Check that data_type is valid
    if (!all(data_type %in% allowed_types)) {
        error_vals <- data_type[!data_type %in% allowed_types]
        stop(paste0("'", error_vals, "' is not an allowed value for 'data_type'. Please enter either 'bugs', 'viruses', or 'unknown'."))
    }

    ## Create locator
    locator <- paste0("results/cMDv4/",
                      uuid,
                      "/metaphlan_lists/metaphlan_",
                      data_type,
                      "_list.tsv.gz")
    return(locator)
}

#' Check cache for requested resource, download or update as needed
#' @import BiocFileCache
#' @import googleCloudStorageR
#'
cache_metaphlan <- function(locator, ask_on_update = TRUE) {
    ## Convert locator to download URL
    url <- gcs_download_url(locator)

    ## Get cache
    bfc <- pMD_get_cache()

    # Check if file already cached
    bfcres <- bfcquery(x = bfc,
                       query = locator,
                       field = "rname")
    rid <- bfcres$rid

    ## Cached file not found
    if (!length(rid)) {
        newpath <- bfcnew(x = bfc,
                          rname = locator,
                          ext = ".tsv.gz",
                          fname = "exact")
        rid <- names(newpath)
        gcs_get_object(locator, saveToDisk = newpath)
    } else { ## Cached file found, ask whether to overwrite cache
        over <- readline(prompt = "Resource found in cache. Redownload and overwrite? (yes/no): ")
        response <- substr(tolower(over), 1, 1)
        doit <- switch(response, y = TRUE, n = FALSE, NA)

        if (doit) {
            rpath <- bfcrpath(bfc, rids = rid)
            gcs_get_object(locator, saveToDisk = rpath)
        }
    }

    ## Return path of cached resource
    res <- bfcrpath(bfc, rids = rid)
    return(res)
}

####

#### Load and format downloaded file

format_metaphlan <- function(uuid, data_type) {
    # check that data_type %in% c("bugs", "viruses", "unknown")
    # load raw file from cache



    if (data_type %in% c("bugs", "unkown")) {
        load_file <- read_tsv(fpath, skip = 4)
    } else if (data_type == "viruses") {
        # convert to SummarizedExperiment (different raw format)
    }
}
####
