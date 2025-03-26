#### Download and cache raw output files from cloud storage

#' @title Get location of dedicated file cache
#' @description 'pMD_get_cache' returns the location of the dedicated
#' parkinsonsMetagenomicData file cache or creates it if it does not exist.
#' @return BiocFileCache cache object
#' @examples
#' \dontrun{
#' if(interactive()){
#'  pMD_get_cache()
#'  }
#' }
#' @seealso
#'  \code{\link[tools]{userdir}}
#'  \code{\link[BiocFileCache]{BiocFileCache-class}}, \code{\link[BiocFileCache]{BiocFileCache}}
#' @rdname pMD_get_cache
#' @export
#' @importFrom tools R_user_dir
#' @importFrom BiocFileCache BiocFileCache
pMD_get_cache <- function() {
    ## Create a directory for cached data
    cache <- tools::R_user_dir("parkinsonsMetagenomicData", "cache")
    if (!dir.exists(cache)) {dir.create(cache)}

    ## Directory path of cache
    BiocFileCache::BiocFileCache(cache = cache)
}

#' @title Retrieve locators for MetaPhlAn output
#' @description 'get_metaphan_locators' gives the names of objects within the
#' Google Bucket gs://metagenomics-mac that contain MetaPhlAn output. Output is
#' requested by associated sample UUID and type of MetaPhlAn output file.
#' @param uuids Vector of strings: sample UUID(s) to get output for
#' @param data_types Single string or vector of strings: values found in the
#' 'data_type' column of listMetagenomicData() or 'all', indicating which output
#' files to get, Default: 'all'
#' @return Vector of strings: names of requested Google Bucket objects
#' @details 'data_types' can be supplied as a single string, to be applied to
#' all provided values in 'uuids', or as a vector of strings the same length as
#' 'uuids' to supply individual values for each UUID.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_metaphlan_locators(uuids = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
#'                         data_types = "all")
#'  }
#' }
#' @rdname get_metaphlan_locators
#' @export
get_metaphlan_locators <- function(uuids, data_types = "all") {
    ## Get available data_types
    fpath <- system.file("extdata", "output_files.csv",
                         package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath)

    allowed_types <- c(ftable$data_type, "all")
    single_type <- FALSE

    ## Check that data_types is valid
    # length
    if (length(data_types) > 1 && length(data_types) != length(uuids)) {
        stop(paste0("The length of 'data_types' is not valid. Please provide either a single value or a vector the same length as 'uuids'."))
    } else if (length(data_types) == 1) {
        single_type <- TRUE
    }
    # values
    if (!all(data_types %in% allowed_types)) {
        error_vals <- data_types[!data_types %in% allowed_types]
        stop(paste0("'", error_vals, "' is not an allowed value for 'data_types'. Please enter a value found in listMetagenomicData() or 'all'."))
    }

    ## Create locator(s)
    locators <- c()

    for (i in 1:length(uuids)) {
        current_uuid <- uuids[i]

        if (single_type) {
            parsed_type <- data_types
        } else {
            parsed_type <- data_types[i]
        }

        if (parsed_type == "all") {
            current_type <- ftable$data_type
            current_name <- ftable$file_name
            current_subdir <- ftable$subdir
        } else {
            current_type <- parsed_type
            filerow <- ftable[ftable$data_type == current_type,]
            current_name <- filerow$file_name
            current_subdir <- filerow$subdir
        }

        current_locator <- paste0("results/cMDv4/",
                                  current_uuid,
                                  "/",
                                  current_subdir,
                                  current_name)
        locators <- c(locators, current_locator)
    }

    return(locators)
}

#' @title Cache Google Bucket object
#' @description 'cache_gcb' checks the parkinsonsMetagenomicData cache for
#' the presence of a Google Bucket object, downloads or updates the file as
#' needed, and returns the path of the cached file.
#' @param locator String: the name of a Google Bucket object
#' @param ask_on_update Boolean: should the function ask the user before
#' re-downloading a file that is already present in the cache, Default: TRUE
#' @return Named vector of strings: Names are the rids of cached files, values
#' are the paths to the cached files
#' @examples
#' \dontrun{
#' if(interactive()){
#'  cache_gcb("results/cMDv4/004c5d07-ec87-40fe-9a72-6b23d6ec584e/metaphlan_lists/metaphlan_bugs_list.tsv.gz")
#'  }
#' }
#' @seealso
#'  \code{\link[googleCloudStorageR]{gcs_download_url}}, \code{\link[googleCloudStorageR]{gcs_get_object}}
#'  \code{\link[BiocFileCache]{BiocFileCache-class}}
#' @rdname cache_gcb
#' @export
#' @importFrom googleCloudStorageR gcs_get_object
#' @importFrom BiocFileCache bfcquery bfcnew bfcrpath
cache_gcb <- function(locator, ask_on_update = TRUE) {
    ## Get cache
    bfc <- pMD_get_cache()

    # Check if file already cached
    bfcres <- BiocFileCache::bfcquery(x = bfc,
                                      query = locator,
                                      field = "rname")
    rid <- bfcres$rid

    ## Cached file not found
    if (!length(rid)) {
        newpath <- BiocFileCache::bfcnew(x = bfc,
                                         rname = locator,
                                         ext = ".tsv.gz",
                                         fname = "exact")
        rid <- names(newpath)
        googleCloudStorageR::gcs_get_object(locator, saveToDisk = newpath)
    } else { ## Cached file found, ask whether to overwrite cache
        if (interactive()) {
            over <- readline(prompt = paste0("Resource with rname = '", locator, "' found in cache. Redownload and overwrite? (yes/no): "))
            response <- substr(tolower(over), 1, 1)
            doit <- switch(response, y = TRUE, n = FALSE, NA)
        } else {
            doit <- FALSE
        }

        if (doit) {
            rpath <- BiocFileCache::bfcrpath(bfc, rids = rid)
            googleCloudStorageR::gcs_get_object(locator, saveToDisk = rpath)
        }
    }

    ## Return path of cached resource
    res <- BiocFileCache::bfcrpath(bfc, rids = rid)
    return(res)
}

#' @title Retrieve and cache MetaPhlAn output files
#' @description 'getMetagenomicData' takes UUID and data type arguments,
#' downloads the corresponding MetaPhlAn output files, and stores them in a
#' local parkinsonsMetagenomicData cache. If the same files are requested again
#' through this function, they will not be re-downloaded unless explicitly
#' specified, in order to reduce excessive downloads.
#' @param uuids Vector of strings: sample UUID(s) to get output for
#' @param data_types Single string or vector of strings: 'bugs', 'viruses',
#' 'unknown', or 'all', indicating which output files to get, Default: 'all'
#' @param ask_on_update Boolean: should the function ask the user before
#' re-downloading a file that is already present in the cache, Default: TRUE
#' @return Tibble: information on the cached files, including UUID, data type,
#' Google Cloud Bucket object name, local cache ID, and cached file path
#' @details 'data_types' can be supplied as a single string, to be applied to
#' all provided values in 'uuids', or as a vector of strings the same length as
#' 'uuids' to supply individual values for each UUID.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  getMetagenomicData(uuid = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
#'                     data_type = "all")
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_split}}
#'  \code{\link[tibble]{tibble}}
#' @rdname getMetagenomicData
#' @export
#' @importFrom stringr str_split
#' @importFrom tibble tibble
getMetagenomicData <- function(uuids,
                               data_types = "all",
                               ask_on_update = TRUE) {
    ## Get Google Bucket locators for requested files
    locators <- get_metaphlan_locators(uuids, data_types)

    ## Download and cache requested files
    cache_paths <- c()
    for (locator in locators) {
        current_file <- cache_gcb(locator, ask_on_update = ask_on_update)
        cache_paths <- c(cache_paths, current_file)
    }

    ## Format cache information for user
    parsed_locators <- stringr::str_split(locators, "/")
    parsed_uuids <- unlist(lapply(parsed_locators, function(x) x[3]))

    parsed_data_types <- unlist(lapply(parsed_locators, function(x) x[5]))
    parsed_data_types <- gsub("metaphlan_|_list.tsv.gz", "", parsed_data_types)

    cache_tbl <- tibble::tibble(UUID = parsed_uuids,
                                data_type = parsed_data_types,
                                gcb_object = locators,
                                cache_id = names(cache_paths),
                                cache_path = cache_paths)

    return(cache_tbl)
}

#' @title List metagenomic data available for download
#' @description 'listMetagenomicData' provides a table of all objects within the
#' 'gs://metagenomics-mac' Google Cloud Bucket that are available for download.
#' @return Tibble: information on the available files, including UUID, data
#' type, Google Cloud Bucket object name, object size, and time the object was
#' last updated
#' @examples
#' \dontrun{
#' if(interactive()){
#'  listMetagenomicData()
#'  }
#' }
#' @seealso
#'  \code{\link[googleCloudStorageR]{gcs_list_objects}}
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[stringr]{str_detect}}, \code{\link[stringr]{str_split}}
#'  \code{\link[tibble]{tibble}}
#' @rdname listMetagenomicData
#' @export
#' @importFrom googleCloudStorageR gcs_list_objects
#' @importFrom readr read_csv
#' @importFrom dplyr filter
#' @importFrom stringr str_detect str_split
#' @importFrom tibble tibble
listMetagenomicData <- function() {
    ## List all objects
    objs <- googleCloudStorageR::gcs_list_objects()

    ## Get output file types to list
    fpath <- system.file("extdata", "output_files.csv",
                         package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath)
    fdetect <- paste(ftable$file_name, collapse = "|")

    ## Filter for allowed file types
    objs <- objs %>%
        dplyr::filter(stringr::str_detect(name, fdetect))

    ## Format file information for user
    parsed_locators <- stringr::str_split(objs$name, "/")
    parsed_uuids <- unlist(lapply(parsed_locators, function(x) x[3]))

    parsed_data_types <- unlist(lapply(parsed_locators, function(x) x[5]))
    parsed_shorthand <- ftable$data_type[match(parsed_data_types,
                                               ftable$file_name)]

    data_tbl <- tibble::tibble(UUID = parsed_uuids,
                               data_type = parsed_shorthand,
                               gcb_object = objs$name,
                               size = objs$size,
                               updated = objs$updated)
    return(data_tbl)
}

####

#### Load and format downloaded file

format_metaphlan_list <- function(uuid, file_path, data_type) {

    if (data_type %in% c("bugs", "unknown")) {
        meta <- readLines(file_path, n = 4)[1:3]
        meta <- gsub("#| reads processed", "", meta)
        meta_list <- as.list(meta)
        names(meta_list) <- c("metaphlan_database",
                              "command",
                              "reads_processed")

        load_file <- readr::read_tsv(file_path, skip = 4)
        colnames(load_file) <- c("clade_name", "ncbi_tax_id",
                                 "relative_abundance", "additional_species")

        rdata <- S4Vectors::DataFrame(load_file[,c("ncbi_tax_id", "additional_species")])
        rownames(rdata) <- load_file$clade_name

        cdata <- S4Vectors::DataFrame(matrix(nrow = 1, ncol = 0))
        rownames(cdata) <- uuid

        relabundance <- as.matrix(load_file$relative_abundance)
        alist <- list(relabundance)
        names(alist) <- paste0(data_type, "_relative_abundance")

        ex <- SummarizedExperiment::SummarizedExperiment(assays = alist,
                                                         rowData = rdata,
                                                         colData = cdata,
                                                         metadata = meta_list)
    } else if (data_type == "viruses") {
        # convert to SummarizedExperiment (different raw format)
    } else {
        stop(paste0("data_type '", data_type, "' is not 'bugs', 'viruses', or 'unknown'. Please enter one of these values or use a different formatting function"))
    }

    return(ex)
}
####
