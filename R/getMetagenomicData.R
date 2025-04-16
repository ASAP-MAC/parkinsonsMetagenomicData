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
#' @description 'get_metaphlan_locators' gives the names of objects within the
#' Google Bucket gs://metagenomics-mac that contain MetaPhlAn output. Output is
#' requested by associated sample UUID and type of MetaPhlAn output file.
#' @param uuids Vector of strings: sample UUID(s) to get output for
#' @param data_type Single string: value found in the data_type' column of
#' listMetagenomicData(), indicating which output files to get, Default: 'microbe_abundance'
#' @return Vector of strings: names of requested Google Bucket objects
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_metaphlan_locators(uuids = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
#'                         data_type = "microbe_abundance")
#'  }
#' }
#' @rdname get_metaphlan_locators
#' @export
get_metaphlan_locators <- function(uuids, data_type = "microbe_abundance") {
    ## Get available data_type values
    fpath <- system.file("extdata", "output_files.csv",
                         package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath)

    allowed_types <- ftable$data_type

    ## Check that data_type is valid
    # length
    if (length(data_type) > 1) {
        stop(paste0("'data_type' should be a single value."))
    }

    # values
    if (!data_type %in% allowed_types) {
        stop(paste0("'", data_type, "' is not an allowed value for 'data_type'. Please enter a value found in listMetagenomicData()."))
    }

    ## Create locator(s)
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
#' @description 'cacheMetagenomicData' takes UUID and data type arguments,
#' downloads the corresponding MetaPhlAn output files, and stores them in a
#' local parkinsonsMetagenomicData cache. If the same files are requested
#' again through this function, they will not be re-downloaded unless explicitly
#' specified, in order to reduce excessive downloads.
#' @param uuids Vector of strings: sample UUID(s) to get output for
#' @param data_type Single string: 'microbe_abundance' or 'viruses', indicating
#' which output files to get, Default: 'microbe_abundance'
#' @param ask_on_update Boolean: should the function ask the user before
#' re-downloading a file that is already present in the cache, Default: TRUE
#' @return A tibble with information on the cached files, including UUID, data
#' type, Google Cloud Bucket object name, local cache ID, and cached file path
#' @examples
#' \dontrun{
#' if(interactive()){
#'  cacheMetagenomicData(uuid = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
#'                       data_type = "microbe_abundance")
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_split}}
#'  \code{\link[tibble]{tibble}}
#' @rdname cacheMetagenomicData
#' @export
#' @importFrom stringr str_split
#' @importFrom tibble tibble
cacheMetagenomicData <- function(uuids,
                                 data_type = "microbe_abundance",
                                 ask_on_update = TRUE) {
    ## Get Google Bucket locators for requested files
    locators <- get_metaphlan_locators(uuids, data_type)

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

    return(cache_tbl)

}

#' @title Load cached files into R as a merged SummarizedExperiment object
#' @description 'loadMetagenomicData' takes a table of information about cached
#' files, including paths to the cached files as well as associated UUIDs and
#' data types, and loads the files into R as SummarizedExperiment objects.
#' Associated sample metadata is automatically attached as colData, and the
#' objects are merged into a single SummarizedExperiment object.
#' @param cache_table A data.frame or tibble: structured like
#' cacheMetagenomicData() output; contains the columns 'UUID', 'cache_path', and
#' 'data_type', with appropriate entries for each file to be loaded in.
#' @return A SummarizedExperiment object with relevant sample metadata attached
#' as colData.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname loadMetagenomicData
#' @export
loadMetagenomicData <- function(cache_table) {
    ## Load data as SummarizedExperiment objects
    se_list <- vector("list", nrow(cache_table))

    for (i in 1:nrow(cache_table)) {
        se_list[[i]] <- parse_metaphlan_list(cache_table$UUID[i],
                                             cache_table$cache_path[i],
                                             cache_table$data_type[i])
    }
    names(se_list) <- paste(cache_table$UUID, cache_table$data_type, sep = "_")

    ## Merge into single SummarizedExperiment object
    merged_se <- merge_experiments(se_list)

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
#'  fpath <- file.path(system.file("extdata",
#'                                 package = "parkinsonsMetagenomicData"),
#'                     "sample_metaphlan_bugs_list.tsv.gz")
#'  parse_metaphlan_list(sample_id = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
#'                       file_path = fpath,
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
    if (data_type == "microbe_abundance") {
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
        names(alist) <- "microbe_relative_abundance"

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
        rdata_cols <- c("m_group_cluster", "length", "m_group_type_k_u",
                        "first_genome_in_cluster", "other_genomes")
        rdata <- S4Vectors::DataFrame(load_file[,rdata_cols])
        rownames(rdata) <- load_file$genome_name

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
        stop(paste0("data_type '", data_type, "' is not 'microbe_abundance' or 'viruses'. Please enter one of these values or use a different parsing function."))
    }

    ## Combine process metadata, row data, sample ID, and assays into
    ## SummarizedExperiment object
    ex <- SummarizedExperiment::SummarizedExperiment(assays = alist,
                                                     rowData = rdata,
                                                     colData = cdata,
                                                     metadata = meta_list)

    return(ex)
}

#' @title Add sample metadata to SummarizedExperiment object as colData
#' @description 'add_metadata' uses the IDs of samples included in a
#' SummarizedExperiment object to attach sample metadata as colData. This new
#' metadata can combine with pre-existing colData.
#' @param sample_ids Vector of strings: sample IDs, such as UUIDs, that will be
#' used to attach samples to their sampleMetadata
#' @param id_col String: column name within sampleMetadata where 'sample_ids'
#' will be found, Default: 'uuid'
#' @param experiment SummarizedExperiment object: contains samples to add
#' metadata to
#' @param method String: 'append', 'overwrite', or 'ignore', indicating how to
#' handle duplicate colData columns, Default: 'append'
#' @return SummarizedExperiment object with sampleMetadata stored as colData
#' @details  sampleMetadata columns found to have the same name as pre-existing
#' colData columns can be either appended (preserving both duplicate columns),
#' used to overwrite the pre-existing columns (leaving only the new version of
#' the duplicate columns), or ignored (leaving only the old version of the
#' duplicate columns)
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fpath <- file.path(system.file("extdata",
#'                                 package = "parkinsonsMetagenomicData"),
#'                     "sample_experiment.Rds")
#'  sample_experiment <- readRDS(fpath)
#'
#'  add_metadata(sample_ids = colnames(sample_experiment),
#'               id_col = "uuid",
#'               experiment = sample_experiment)
#'  }
#' }
#' @seealso
#'  \code{\link[S4Vectors]{DataFrame-class}}, \code{\link[S4Vectors]{S4VectorsOverview}}
#'  \code{\link[SummarizedExperiment]{SummarizedExperiment-class}}
#' @rdname add_metadata
#' @export
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData
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

#' @title Merge SummarizedExperiment objects with the same assay types together
#' @description 'merge_experiments' takes a list of SummarizedExperiment objects
#' with the same assays but different samples, and combines them into a single
#' SummarizedExperiment object.
#' @param merge_list List of SummarizedExperiment objects: to be merged into a
#' single SummarizedExperiment object
#' @return SummarizedExperiment object with multiple samples
#' @details The assays contained in the SummarizedExperiments to be merged must
#' be of the same type (i.e. have the same name) and be in the same order if
#' there are multiple assays.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fpath <- file.path(system.file("extdata",
#'                                 package = "parkinsonsMetagenomicData"),
#'                     "sample_experiment_list.Rds")
#'  sample_experiment_list <- readRDS(fpath)
#'
#'  merge_experiments(sample_experiment_list)
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}, \code{\link[purrr]{reduce}}
#'  \code{\link[SummarizedExperiment]{SummarizedExperiment-class}}, \code{\link[SummarizedExperiment]{c("SummarizedExperiment-class", "SummarizedExperiment")}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{bind_rows}}
#'  \code{\link[tidyselect]{everything}}
#'  \code{\link[tidyr]{replace_na}}
#'  \code{\link[S4Vectors]{SimpleList-class}}, \code{\link[S4Vectors]{c("DataFrame-class", "S4VectorsOverview")}}
#'  \code{\link[magrittr]{extract}}
#' @rdname merge_experiments
#' @export
#' @importFrom purrr map_chr map reduce
#' @importFrom SummarizedExperiment assayNames assay rowData colData SummarizedExperiment
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom dplyr full_join mutate across bind_rows
#' @importFrom tidyselect everything
#' @importFrom tidyr replace_na
#' @importFrom S4Vectors SimpleList DataFrame
#' @importFrom magrittr set_names
merge_experiments <- function(merge_list) {
    ## Check that list contains more than one SummarizedExperiment
    if (length(merge_list) == 1) {
        return(merge_list[[1]])
    }

    ## Check that assays match
    assay_names <-
        lapply(merge_list, SummarizedExperiment::assayNames) |>
        unique()

    if (length(assay_names) != 1) {
        stop("'merge_list' contains multiple assay types, please provide a list where all assays match in type and order.")
    }

    ## Merge assays
    assay_list <- vector("list", length(assay_names[[1]]))
    names(assay_list) <- assay_names[[1]]
    for (i in 1:length(assay_list)) {
        assay_list[[i]] <-
            purrr::map(merge_list, \(x) SummarizedExperiment::assay(x, i)) |>
            purrr::map(as.matrix) |>
            purrr::map(as.data.frame) |>
            purrr::map(tibble::rownames_to_column) |>
            purrr::reduce(dplyr::full_join, by = "rowname") |>
            tibble::column_to_rownames() |>
            dplyr::mutate(dplyr::across(tidyselect::everything(),
                                        .fns = ~ tidyr::replace_na(.x, 0))) |>
            as.matrix()
    }

    assay_list <- assay_list |>
        S4Vectors::SimpleList()

    ## Merge row data
    rowData <-
        purrr::map(merge_list, SummarizedExperiment::rowData) |>
        purrr::map(as.data.frame) |>
        purrr::map(tibble::rownames_to_column)

    join_by <-
        purrr::map(rowData, colnames) |>
        purrr::reduce(intersect)

    rowData <-
        purrr::reduce(rowData, dplyr::full_join, by = join_by) |>
        tibble::column_to_rownames() |>
        S4Vectors::DataFrame()

    ## Merge column data
    colData <-
        purrr::map(merge_list, SummarizedExperiment::colData) |>
        purrr::map(as.data.frame) |>
        purrr::map(tibble::rownames_to_column) |>
        dplyr::bind_rows() |>
        tibble::column_to_rownames() |>
        S4Vectors::DataFrame()

    ## Reformat as SummarizedExperiment
    se <- SummarizedExperiment::SummarizedExperiment(assays = assay_list,
                                                     rowData = rowData,
                                                     colData = colData)
    return(se)
}
