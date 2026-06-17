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
#' @noRd
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
#' single TreeSummarizedExperiment object. Delegates to
#' \code{\link[curatedCore]{mergeExperiments}}.
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
#'  \code{\link[curatedCore]{mergeExperiments}}
#' @rdname mergeExperiments
#' @noRd
mergeExperiments <- function(merge_list) {
    curatedCore::mergeExperiments(merge_list)
}
