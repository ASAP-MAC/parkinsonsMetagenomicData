#### Download and cache raw output files from cloud storage

#' @title Retrieve Google Bucket locators for output
#' @description 'get_bucket_locators' gives the names of objects within the
#' Google Bucket gs://metagenomics-mac. Output is requested by associated sample
#' UUID and type of output file.
#' @param uuids Vector of strings: sample UUID(s) to get output for
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types(), indicating which output files to get,
#' Default: 'relative_abundance'
#' @return Vector of strings: names of requested Google Bucket objects
#' @examples
#' get_bucket_locators(uuids = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
#'                     data_type = "relative_abundance")
#' @rdname get_bucket_locators
#' @export
get_bucket_locators <- function(uuids, data_type = "relative_abundance") {
    ## Confirm inputs are valid
    confirm_uuids(uuids)
    confirm_data_type(data_type)

    ## Create locator(s)
    ftable <- output_file_types()
    filerow <- ftable[ftable$data_type == data_type,]
    file_name <- filerow$file_name
    subdir <- filerow$subdir

    locators <- paste0("results/cMDv4/",
                        uuids,
                        "/",
                        subdir,
                        file_name)

    return(locators)
}

#' @title Cache Google Bucket object
#' @description 'cache_gcb' checks the parkinsonsMetagenomicData cache for
#' the presence of a Google Bucket object, downloads or updates the file as
#' needed, and returns the path of the cached file.
#' @param locator String: the name of a Google Bucket object
#' @param redownload String: "yes", "no", or "ask"; should the function
#' re-download a file that is already present in the cache, Default: 'no'
#' @param custom_cache BiocFileCache object: a custom cache object may be
#' specified instead of the default created by pMD_get_cache(), Default: NULL
#' @return Named vector of strings: Names are the rids of cached files, values
#' are the paths to the cached files
#' @examples
#' \donttest{
#'  locator <- "results/cMDv4/004c5d07-ec87-40fe-9a72-6b23d6ec584e/metaphlan_lists/metaphlan_bugs_list.tsv.gz"
#'  cache_gcb(locator = locator,
#'            redownload = "ask")
#' }
#' @seealso
#'  \code{\link[googleCloudStorageR]{gcs_download_url}}
#'  \code{\link[googleCloudStorageR]{gcs_get_object}}
#'  \code{\link[BiocFileCache]{BiocFileCache-class}}
#' @rdname cache_gcb
#' @export
#' @importFrom googleCloudStorageR gcs_get_object
#' @importFrom BiocFileCache bfcquery bfcnew bfcremove bfcrpath
cache_gcb <- function(locator, redownload = "no", custom_cache = NULL) {
    ## Check redownload value
    allowed_redown <- c("y", "n", "a")
    p_redown <- substr(tolower(redownload), 1, 1)
    if (!p_redown %in% allowed_redown) {
        stop("'", redownload, "' is not an allowed value for ",
             "'redownload'. Please enter 'yes', 'no', or 'ask'")
    }

    ## Get cache
    if (!is.null(custom_cache)) {
        stopifnot(methods::is(custom_cache, "BiocFileCache"))
        bfc <- custom_cache
    } else {
        bfc <- pMD_get_cache()
    }

    ## Check if file already cached
    bfcres <- BiocFileCache::bfcquery(x = bfc,
                                        query = locator,
                                        field = "rname")

    if (nrow(bfcres) > 0) {
        rid <- bfcres$rid[bfcres$create_time == max(bfcres$create_time)]
    } else {
        rid <- bfcres$rid
    }

    ## Cached file not found
    if (!length(rid)) {
        cache_new(bfc, locator)
    ## Cached file found, follow "redownload" instructions
    } else if (length(rid)) {
        handle_redownload(bfc, locator, rid, p_redown)
    }

    ## Return path of cached resource
    res <- BiocFileCache::bfcrpath(bfc, rids = rid)
    return(res)
}

#' @title Retrieve and cache output files
#' @description 'cacheMetagenomicData' takes UUID and data type arguments,
#' downloads the corresponding output files, and stores them in a local
#' parkinsonsMetagenomicData cache. If the same files are requested again
#' through this function, they will not be re-downloaded unless explicitly
#' specified, in order to reduce excessive downloads.
#' @param uuids Vector of strings: sample UUID(s) to get output for
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types(), indicating which output files to get,
#' Default: 'relative_abundance'
#' @param redownload String: "yes", "no", or "ask"; should the function
#' re-download a file that is already present in the cache, Default: 'no'
#' @param custom_cache BiocFileCache object: a custom cache object may be
#' specified instead of the default created by pMD_get_cache(), Default: NULL
#' @return A tibble with information on the cached files, including UUID, data
#' type, Google Cloud Bucket object name, local cache ID, and cached file path
#' @examples
#' \donttest{
#'  cacheMetagenomicData(uuid = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
#'                       data_type = "pathabundance_unstratified",
#'                       redownload = "ask")
#' }
#' @seealso
#'  \code{\link[stringr]{str_split}}
#'  \code{\link[tibble]{tibble}}
#' @rdname cacheMetagenomicData
#' @export
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom stats setNames
cacheMetagenomicData <- function(uuids,
                                data_type = "relative_abundance",
                                redownload = "no",
                                custom_cache = NULL) {
    ## Confirm inputs are valid
    confirm_uuids(uuids)
    confirm_data_type(data_type)
    allowed_redown <- c("y", "n", "a")
    p_redown <- substr(tolower(redownload), 1, 1)
    if (!p_redown %in% allowed_redown) {
        stop("'", redownload, "' is not an allowed value for ",
             "'redownload'. Please enter 'yes', 'no', or 'ask'")
    }
    if (!is.null(custom_cache)) {
        stopifnot(methods::is(custom_cache, "BiocFileCache"))
    }

    ## Get Google Bucket locators for requested files
    locators <- get_bucket_locators(uuids, data_type)

    ## Download and cache requested files
    res <- cache_list(locators, p_redown, custom_cache)

    ## Format cache information for user
    parsed_locators <- stringr::str_split(locators, "/")
    parsed_uuids <- unlist(lapply(parsed_locators, function(x) x[3]))

    parsed_filenames <- unlist(lapply(parsed_locators,
                                        function(x) x[length(x)]))
    fpath <- system.file("extdata", "output_files.csv",
                        package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE)
    parsed_data_types <- ftable$data_type[match(parsed_filenames,
                                                ftable$file_name)]

    cache_tbl <- tibble::tibble(uuid = parsed_uuids,
                                data_type = parsed_data_types,
                                gcb_object = locators,
                                cache_id = names(res$cache_paths),
                                cache_path = res$cache_paths)

    ## Print any errors and return cache information
    if (length(res$errors) > 0) {
        warning(res$errors)
    }

    return(cache_tbl)
}

#' @title Load cached files into R as a merged TreeSummarizedExperiment object
#' @description 'loadMetagenomicData' takes a table of information about cached
#' files, including paths to the cached files as well as associated UUIDs and
#' data types, and loads the files into R as TreeSummarizedExperiment objects.
#' Associated sample metadata is automatically attached as colData, and the
#' objects are merged into a single TreeSummarizedExperiment object.
#' @param cache_table A data.frame or tibble: structured like
#' cacheMetagenomicData() output; contains the columns 'uuid', 'cache_path', and
#' 'data_type', with appropriate entries for each file to be loaded in.
#' @return A TreeSummarizedExperiment object with relevant sample metadata
#' attached as colData.
#' @details At the moment, only the `metaphlan_lists` data types,
#' `viral_clusters` and `relative_abundance`, as well as the `humann` data
#' types, have parsing functions for automatically loading into
#' TreeSummarizedExperiment objects.
#' @examples
#' \donttest{
#'  uuid <- "004c5d07-ec87-40fe-9a72-6b23d6ec584e"
#'  cache_table <- cacheMetagenomicData(uuid = uuid,
#'                                      data_type = "relative_abundance")
#'  loadMetagenomicData(cache_table = cache_table)
#' }
#' @rdname loadMetagenomicData
#' @export
loadMetagenomicData <- function(cache_table) {
    ## Check input table format
    stopifnot(methods::is(cache_table, "data.frame"))
    req_cols <- c("uuid", "cache_path", "data_type")
    missing_cols <- req_cols[!req_cols %in% colnames(cache_table)]
    if (length(missing_cols) > 0) {
        print_missing <- paste(missing_cols, collapse = "\n")
        stop("One or more columns are not present in the input data frame.\n",
            print_missing)
    }

    ## Check that all data_type values are the same and valid
    data_type <- unique(cache_table$data_type)
    if (length(data_type) > 1) {
        stop("Multiple 'data_type' values detected. Please provide a ",
             "table where all rows have the same value for 'data_type'.")
    }
    confirm_data_type(data_type, "subdir", "humann|metaphlan_lists")

    ## Load data as TreeSummarizedExperiment objects
    se_list <- vector("list", nrow(cache_table))
    humann_types <- output_file_types("tool", "humann")$data_type
    metaphlan_types <- output_file_types("tool", "metaphlan")$data_type

    for (i in seq_len(nrow(cache_table))) {
        if (data_type %in% metaphlan_types) {
            se_list[[i]] <- parse_metaphlan_list(cache_table$uuid[i],
                                                cache_table$cache_path[i],
                                                cache_table$data_type[i])

        } else if (data_type %in% humann_types) {
            se_list[[i]] <- parse_humann(cache_table$uuid[i],
                                        cache_table$cache_path[i],
                                        cache_table$data_type[i])
        }
    }
    names(se_list) <- paste(cache_table$uuid, cache_table$data_type, sep = "_")

    ## Merge into single TreeSummarizedExperiment object
    merged_se <- mergeExperiments(se_list)

    ## Add sample metadata
    merged_se <- add_metadata(colnames(merged_se),
                                id_col = "uuid",
                                merged_se)

    return(merged_se)
}

#' @title List metagenomic data available for download
#' @description 'listMetagenomicData' provides a table of all objects within the
#' 'gs://metagenomics-mac' Google Cloud Bucket that are available for download.
#' @return Tibble: information on the available files, including UUID, data
#' type, Google Cloud Bucket object name, object size, and time the object was
#' last updated
#' @examples
#' \donttest{
#'  listMetagenomicData()
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
    ftable <- output_file_types()
    fdetect <- paste(ftable$file_name, collapse = "|")

    ## Filter for allowed file types
    objs <- objs %>%
        dplyr::filter(stringr::str_detect(.data$name, fdetect))

    ## Format file information for user
    parsed_locators <- stringr::str_split(objs$name, "/")
    parsed_uuids <- unlist(lapply(parsed_locators, function(x) x[3]))

    parsed_data_types <- unlist(lapply(parsed_locators,
                                        function(x) x[length(x)]))
    parsed_shorthand <- ftable$data_type[match(parsed_data_types,
                                                ftable$file_name)]

    data_tbl <- tibble::tibble(uuid = parsed_uuids,
                                data_type = parsed_shorthand,
                                gcb_object = objs$name,
                                size = objs$size,
                                updated = objs$updated)
    return(data_tbl)
}

#' @title Add sample metadata to TreeSummarizedExperiment object as colData
#' @description 'add_metadata' uses the IDs of samples included in a
#' TreeSummarizedExperiment object to attach sample metadata as colData. This
#' new metadata can combine with pre-existing colData.
#' @param sample_ids Vector of strings: sample IDs, such as UUIDs, that will be
#' used to attach samples to their sampleMetadata
#' @param id_col String: column name within sampleMetadata where 'sample_ids'
#' will be found, Default: 'uuid'
#' @param experiment TreeSummarizedExperiment object: contains samples to add
#' metadata to
#' @param method String: 'append', 'overwrite', or 'ignore', indicating how to
#' handle duplicate colData columns, Default: 'append'
#' @return TreeSummarizedExperiment object with sampleMetadata stored as colData
#' @details  sampleMetadata columns found to have the same name as pre-existing
#' colData columns can be either appended (preserving both duplicate columns),
#' used to overwrite the pre-existing columns (leaving only the new version of
#' the duplicate columns), or ignored (leaving only the old version of the
#' duplicate columns)
#' @examples
#' fpath <- file.path(system.file("extdata",
#'                                package = "parkinsonsMetagenomicData"),
#'                    "sample_experiment.Rds")
#' sample_experiment <- readRDS(fpath)
#'
#' add_metadata(sample_ids = colnames(sample_experiment),
#'              id_col = "uuid",
#'              experiment = sample_experiment)
#' @seealso
#'  \code{\link[S4Vectors]{DataFrame-class}}
#'  \code{\link[TreeSummarizedExperiment]{TreeSummarizedExperiment-class}}
#' @rdname add_metadata
#' @export
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData
#' @importFrom utils data
add_metadata <- function(sample_ids, id_col = "uuid", experiment,
                        method = "append") {
    ## Check input
    if (length(sample_ids) != ncol(experiment)) {
        stop("'sample_ids' has a different number of samples than ",
             "'experiment'.")
    }
    stopifnot(methods::is(experiment, "TreeSummarizedExperiment"))
    utils::data("sampleMetadata", package = "parkinsonsMetagenomicData",
        envir = environment())
    meta <- sampleMetadata
    if (!id_col %in% colnames(meta)) {
        stop("'", id_col, "' is not a column in sampleMetadata.")
    } else if (length(unique(meta[[id_col]])) != nrow(meta)) {
        stop("'", id_col, "' is not unique for every sample and ",
             "therefore cannot be used to retrieve metadata.")
    }
    valid_methods <- c("append", "overwrite", "ignore")
    if (!method %in% valid_methods) {
        stop("'", method, "' is not a valid value for 'method'. Please ",
             "enter 'append', 'overwrite', or 'ignore'.")
    }

    ## Get metadata rows based on sample ID
    meta <- meta[match(sample_ids, meta[[id_col]]),] |>
        S4Vectors::DataFrame()
    rownames(meta) <- colnames(experiment)

    ## Add sample metadata to colData according to chosen method
    cdata <- SummarizedExperiment::colData(experiment)
    duplicated <- intersect(colnames(cdata), colnames(meta))
    not_duplicated <- setdiff(colnames(meta), duplicated)
    if (length(duplicated) != 0) {
        dup_message <- paste(duplicated, collapse = ", ")
        message("Duplicate metadata columns found, will be processed ",
            "according to method '", method, "':")
        message(dup_message)
    }
    if (method == "append") {
        newdata <- cbind(cdata, meta)
    } else if (method == "overwrite") {
        cdata[duplicated] <- meta[duplicated]
        newdata <- cbind(cdata, meta[not_duplicated])
    } else if (method == "ignore") {
        newdata <- cbind(cdata, meta[not_duplicated])
    }
    SummarizedExperiment::colData(experiment) <- newdata

    return(experiment)
}

#' @title Merge TreeSummarizedExperiment objects with the same assay types
#' together
#' @description 'mergeExperiments' takes a list of TreeSummarizedExperiment
#' objects with the same assays but different samples, and combines them into a
#' single TreeSummarizedExperiment object.
#' @param merge_list List of TreeSummarizedExperiment objects: to be merged into
#' a single TreeSummarizedExperiment object
#' @return TreeSummarizedExperiment object with multiple samples
#' @details The assays contained in the TreeSummarizedExperiments to be merged
#' must be of the same type (i.e. have the same name) and be in the same order
#' if there are multiple assays.
#' @examples
#' fpath <- file.path(system.file("extdata",
#'                                package = "parkinsonsMetagenomicData"),
#'                    "sample_experiment_list.Rds")
#' sample_experiment_list <- readRDS(fpath)
#'
#' mergeExperiments(sample_experiment_list)
#' @seealso
#'  \code{\link[purrr]{map}}, \code{\link[purrr]{reduce}}
#'  \code{\link[TreeSummarizedExperiment]{TreeSummarizedExperiment-class}}
#'  \code{\link[TreeSummarizedExperiment]{TreeSummarizedExperiment}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[dplyr]{across}}, \code{\link[dplyr]{bind_rows}}
#'  \code{\link[tidyselect]{everything}}
#'  \code{\link[tidyr]{replace_na}}
#'  \code{\link[S4Vectors]{SimpleList-class}}
#'  \code{\link[S4Vectors]{DataFrame-class}}
#'  \code{\link[magrittr]{extract}}
#' @rdname mergeExperiments
#' @export
#' @importFrom SummarizedExperiment assayNames assay rowData colData
#' @importFrom purrr map reduce
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom dplyr full_join mutate across bind_rows
#' @importFrom tidyselect everything
#' @importFrom tidyr replace_na
#' @importFrom S4Vectors SimpleList DataFrame
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
mergeExperiments <- function(merge_list) {
    ## Check that list contains more than one TreeSummarizedExperiment
    if (length(merge_list) == 1) {
        return(merge_list[[1]])
    }

    for (i in seq_along(merge_list)) {
        if (!methods::is(merge_list[[i]], "TreeSummarizedExperiment")) {
            stop("The list item at index = ", i, " is not a ",
                 "TreeSummarizedExperiment object.")
        }
    }


    ## Merge assays
    assay_list <- merge_assays(merge_list)

    ## Merge row data
    rowData <- merge_rowdata(merge_list, assay_list)

    ## Merge column data
    colData <-
        purrr::map(merge_list, SummarizedExperiment::colData) |>
        purrr::map(as.data.frame) |>
        purrr::map(tibble::rownames_to_column) |>
        dplyr::bind_rows() |>
        tibble::column_to_rownames() |>
        S4Vectors::DataFrame()

    ## Reformat as TreeSummarizedExperiment
    se <- TreeSummarizedExperiment::TreeSummarizedExperiment(
                                                            assays = assay_list,
                                                            rowData = rowData,
                                                            colData = colData)
    return(se)
}
