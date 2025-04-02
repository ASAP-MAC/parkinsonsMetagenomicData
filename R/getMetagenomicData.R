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
#'  cache_gcb(locator = "results/cMDv4/004c5d07-ec87-40fe-9a72-6b23d6ec584e/metaphlan_lists/metaphlan_bugs_list.tsv.gz",
#'            ask_on_update = TRUE)
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
#' downloads the corresponding MetaPhlAn output files, stores them in a
#' local parkinsonsMetagenomicData cache, and loads them into R as
#' SummarizedExperiment objects if requested. If the same files are requested
#' again through this function, they will not be re-downloaded unless explicitly
#' specified, in order to reduce excessive downloads.
#' @param uuids Vector of strings: sample UUID(s) to get output for
#' @param data_types Single string or vector of strings: 'bugs', 'viruses',
#' 'unknown', or 'all', indicating which output files to get, Default: 'all'
#' @param ask_on_update Boolean: should the function ask the user before
#' re-downloading a file that is already present in the cache, Default: TRUE
#' @param load Boolean: should the retrieved files be loaded into R as a list of
#' SummarizedExperiment objects, Default: TRUE
#' @return If load = TRUE, a list of SummarizedExperiment objects. If
#' load = FALSE, a tibble with information on the cached files, including UUID,
#' data type, Google Cloud Bucket object name, local cache ID, and cached file
#' path
#' @details 'data_types' can be supplied as a single string, to be applied to
#' all provided values in 'uuids', or as a vector of strings the same length as
#' 'uuids' to supply individual values for each UUID.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  getMetagenomicData(uuid = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
#'                     data_type = "all",
#'                     load = TRUE)
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
                               ask_on_update = TRUE,
                               load = TRUE) {
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

    parsed_filenames <- unlist(lapply(parsed_locators, function(x) x[5]))
    fpath <- system.file("extdata", "output_files.csv",
                         package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath)
    parsed_data_types <- ftable$data_type[match(parsed_filenames, ftable$file_name)]

    cache_tbl <- tibble::tibble(UUID = parsed_uuids,
                                data_type = parsed_data_types,
                                gcb_object = locators,
                                cache_id = names(cache_paths),
                                cache_path = cache_paths)

    ## Load data as SummarizedExperiment objects if requested
    if (load) {
        se_list <- vector("list", nrow(cache_tbl))

        for (i in 1:nrow(cache_tbl)) {
            se_list[[i]] <- parse_metaphlan_list(cache_tbl$UUID[i],
                                                 cache_tbl$cache_path[i],
                                                 cache_tbl$data_type[i])
        }
        names(se_list) <- paste(cache_tbl$UUID, cache_tbl$data_type, sep = "_")

        return(se_list)
    } else {return(cache_tbl)}

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

#' @title Parse basic MetaPhlAn output for a single sample as a
#' SummarizedExperiment object
#' @description 'parse_metaphlan_list' reads a file obtained from running
#' MetaPhlAn for microbial profiling (with or without unclassified fraction
#' estimation) or viral sequence cluster analysis. This file is parsed into a
#' SummarizedExperiment object.
#' @param sample_id String: A sample identifier
#' @param file_path String: Path to a locally stored MetaPhlAn output file in
#' TSV format
#' @param data_type String: The type of MetaPhlAn output file to be parsed, as
#' found in the 'data_type' column of listMetagenomicData()
#' @return A SummarizedExperiment object with process metadata, row data, column
#' names, and relevant assays.
#' @details This function does not integrate sample metadata as column data. The
#' provided sample_id is used as the column name for assays within the
#' SummarizedExperiment object and is intended to be used for integration of
#' sample metadata.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  parse_metaphlan_list(sample_id = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
#'                       file_path = file.path(system.file("extdata",
#'                                             package = "parkinsonsMetagenomicData"),
#'                                             "sample_metaphlan_list.tsv.gz"),
#'                       data_type = "bugs")
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[S4Vectors]{DataFrame-class}}, \code{\link[S4Vectors]{S4VectorsOverview}}
#'  \code{\link[SummarizedExperiment]{SummarizedExperiment-class}}, \code{\link[SummarizedExperiment]{SummarizedExperiment}}
#' @rdname parse_metaphlan_list
#' @export
#' @importFrom readr read_tsv
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment SummarizedExperiment
parse_metaphlan_list <- function(sample_id, file_path, data_type) {
    ## Slight differences in output file format
    if (data_type %in% c("bugs", "unknown")) {
        ## Convert commented header lines to metadata
        meta <- readLines(file_path, n = 3)
        meta <- gsub("#| reads processed", "", meta)
        meta_list <- as.list(meta)
        names(meta_list) <- c("metaphlan_database",
                              "command",
                              "reads_processed")

        ## Read remainder of output file
        load_file <- readr::read_tsv(file_path, skip = 4)
        colnames(load_file) <- c("clade_name", "ncbi_tax_id",
                                 "relative_abundance", "additional_species")

        ## Separate out row data
        rdata_cols <- c("ncbi_tax_id", "additional_species")
        rdata <- S4Vectors::DataFrame(load_file[,rdata_cols])
        rownames(rdata) <- load_file$clade_name

        ## Set sample ID as column name
        cdata <- S4Vectors::DataFrame(matrix(nrow = 1, ncol = 0))
        rownames(cdata) <- sample_id

        ## Set relative abundance as assay
        relabundance <- as.matrix(load_file$relative_abundance)
        alist <- list(relabundance)
        names(alist) <- paste0(data_type, "_relative_abundance")

    } else if (data_type == "viruses") {
        ## Convert commented header lines to metadata
        meta <- readLines(file_path, n = 2)
        meta <- gsub("#", "", meta)
        meta_list <- as.list(meta)
        names(meta_list) <- c("metaphlan_database",
                              "command")

        ## Read remainder of output file
        load_file <- readr::read_tsv(file_path, skip = 3)
        colnames(load_file) <- c("m_group_cluster", "genome_name", "length",
                                 "breadth_of_coverage",
                                 "depth_of_coverage_mean",
                                 "depth_of_coverage_median", "m_group_type_k_u",
                                 "first_genome_in_cluster", "other_genomes")

        ## Separate out row data
        rdata_cols <- c("genome_name", "length", "m_group_type_k_u",
                        "first_genome_in_cluster", "other_genomes")
        rdata <- S4Vectors::DataFrame(load_file[,rdata_cols])
        rownames(rdata) <- load_file$m_group_cluster

        ## Set sample ID as column name
        cdata <- S4Vectors::DataFrame(matrix(nrow = 1, ncol = 0))
        rownames(cdata) <- sample_id

        ## Set breadth and depth of coverage measurements as separate assays
        breadth <- as.matrix(load_file$breadth_of_coverage)
        depth_mean <- as.matrix(load_file$depth_of_coverage_mean)
        depth_median <- as.matrix(load_file$depth_of_coverage_median)
        alist <- list(breadth,
                      depth_mean,
                      depth_median)
        names(alist) <- c("viral_breadth_of_coverage",
                          "viral_depth_of_coverage_mean",
                          "viral_depth_of_coverage_median")
    } else {
        ## Notify if output file is not able to be parsed by this function
        stop(paste0("data_type '", data_type, "' is not 'bugs', 'viruses', or 'unknown'. Please enter one of these values or use a different parsing function."))
    }

    ## Combine process metadata, row data, sample ID, and assays into
    ## SummarizedExperiment object
    ex <- SummarizedExperiment::SummarizedExperiment(assays = alist,
                                                     rowData = rdata,
                                                     colData = cdata,
                                                     metadata = meta_list)

    return(ex)
}

add_metadata <- function(sample_ids, id_col = "uuid", experiment, method = "append") {
    ## Check that the length of sample_ids matches the number of samples
    if (length(sample_ids) != ncol(experiment)) {
        stop("'sample_ids' has a different number of samples than 'experiment'.")
    }

    ## Retrieve sample metadata
    meta <- sampleMetadata

    ## Check that id_col and method are valid
    if (!id_col %in% colnames(meta)) {
        stop(paste0("'", id_col, "' is not a column in sampleMetadata."))
    } else if (length(unique(meta[[id_col]])) != nrow(meta)) {
        stop(paste0("'", id_col, "' is not unique for every sample and therefore cannot be used to retrieve metadata."))
    }

    valid_methods <- c("append", "overwrite", "ignore")
    if (!method %in% valid_methods) {
        stop(paste0("'", method, "' is not a valid value for 'method'. Please enter 'append', 'overwrite', or 'ignore'."))
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
        message(paste0("Duplicate metadata columns found, will be processed according to method '", method, "':"))
        message(paste(duplicated, collapse = ", "))
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

merge_metaphlan_lists <- function(merge_list) {
    ## Check that list contains more than one SummarizedExperiment
    if (length(merge_list) == 1) {
        return(merge_list[[1]])
    }

    ## Check that assays match
    assay_names <- lapply(merge_list, SummarizedExperiment::assayNames) |>
        unique()

    if (length(assay_names) != 1) {
        stop("'merge_list' contains multiple assay types, please provide a list where all assays match.")
    }

    ## Merge assays

    ## Merge row data

    ## Merge column data

    ## Reformat as SummarizedExperiment
}
