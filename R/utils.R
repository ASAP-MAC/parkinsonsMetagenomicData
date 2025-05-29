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

#' @title Read in extdata/output_files.csv
#' @description 'output_file_types' reduces the lines of code needed to read in
#' extdata/output_files.csv.
#' @return Tibble with columns 'data_type', 'file_name', and 'subdir'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  output_file_types()
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @rdname output_file_types
#' @export
#' @importFrom readr read_csv
output_file_types <- function() {
    fpath <- system.file("extdata", "output_files.csv",
                         package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE) |>
        as.data.frame()
    return(ftable)
}

#' @title Return all "extensions" from a file path
#' @description 'get_exts' returns the extension of a file name or path,
#' including pseudo-extensions such as ".tsv" in "file.tsv.gz".
#' @param file_path String: file name or path to get extension(s) from
#' @return String: file extension, including pseudo-extensions
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_exts("path/file.tsv.gz")
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_split}}
#' @rdname get_exts
#' @export
#' @importFrom stringr str_split_fixed
get_exts <- function(file_path) {
    bname <- basename(file_path)
    exts <- paste0(".", stringr::str_split_fixed(bname, "\\.", 2)[2])
    return(exts)
}

#' @title Validate UUIDs
#' @description 'confirm_uuids' checks that a single string or vector of strings
#' are valid UUIDs.
#' @param uuids String or character vector: strings to validate
#' @return NULL
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  confirm_uuids("56aa2ad5-007d-407c-a644-48aac1e9a8f0")
#'  confirm_uuids("horse")
#'  }
#' }
#' @rdname confirm_uuids
#' @export
confirm_uuids <- function(uuids) {
    results <- c()
    for (x in uuids) {
        is_uuid <- grepl("[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}", x)
        results <- c(results, is_uuid)
    }

    if (!any(is_uuid)) {
        bad_uuids <- paste(uuids[!results], collapse = "\n")
        stop(paste0("One or more values are not valid UUIDs.\n", bad_uuids))
    }
}

#' @title Validate 'data_type' argument
#' @description 'confirm_data_type' checks that a single string is valid to be
#' used as a 'data_type' argument for various functions in
#' parkinsonsMetagenomicData. Specifically, the input should be a single value
#' that is found in the 'data_type' column of the output data frame from
#' output_file_types().
#' @param data_type String: input to be validated
#' @return NULL
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  confirm_data_type("relative_abundance")
#'  confirm_data_type(c("relative_abundance", "viral_clusters"))
#'  confirm_data_type("horse")
#'  }
#' }
#' @rdname confirm_data_type
#' @export
confirm_data_type <- function(data_type, filter_col = NULL, filter_string = NULL) {
    ## Get allowed types
    types <- output_file_types()
    all_types <- types$data_type

    if (!is.null(filter_col) & !is.null(filter_string)) {
        filter_ind <- TRUE
        filtered_types <- types %>%
            filter(grepl(filter_string, .data[[filter_col]])) %>%
            pull(data_type)
    } else {
        filter_ind <- FALSE
    }

    ## Check that data_type is valid
    # length
    if (length(data_type) > 1) {
        stop(paste0("'data_type' should be a single value."))
    }

    # values
    if (filter_ind) {
        if (!data_type %in% filtered_types) {
            print_filtered <- paste(filtered_types, collapse = ", ")
            stop(paste0("'", data_type,
                        "' is not an allowed value for this function. Please enter one of the following values: ",
                        print_filtered))
        }
    } else {
        if (!data_type %in% all_types) {
            stop(paste0("'", data_type,
                        "' is not an allowed value for 'data_type'. Please enter a value found in output_file_types()."))
        }
    }
}
