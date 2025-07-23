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
#' @description 'output_file_types' reads in the table extdata/output_files.csv.
#' The table can optionally be filtered by providing a column name to filter by
#' and a string/regular expression to filter the selected column with.
#' @return Tibble with columns 'tool', 'data_type', 'file_name', and 'subdir'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  output_file_types()
#'  output_file_types("tool", "metaphlan")
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @rdname output_file_types
#' @export
#' @importFrom readr read_csv
output_file_types <- function(filter_col = NULL, filter_string = NULL) {
    ## Read file
    fpath <- system.file("extdata", "output_files.csv",
                         package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE) |>
        as.data.frame()

    ## Filter table if requested
    if (!is.null(filter_col) & !is.null(filter_string)) {
        if (!filter_col %in% colnames(ftable)) {
            print_colnames <- paste(colnames(ftable), collapse = ", ")
            stop(paste0("'", filter_col, "' is not a column of output_files.csv. Please choose one of the following: ", print_colnames))
        }

        ftable <- ftable %>%
            filter(grepl(filter_string, .data[[filter_col]], ignore.case = TRUE))
    }

    return(ftable)
}

#' @title Retrieve column info for parquet files based on original file type
#' @description 'parquet_colinfo' returns the column info associated with a
#' parquet file made from a particular output file type.
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as the name of a file in the repo
#' https://huggingface.co/datasets/waldronlab/metagenomics_mac.
#' @return Data frame with columns 'general_data_type', 'col_name', 'col_class',
#' 'description', 'se_role', and 'position'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  parquet_colinfo("viral_clusters")
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{arrange}}
#' @rdname parquet_colinfo
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr filter arrange
parquet_colinfo <- function(data_type) {
    ## Load in parquet column dictionary
    fpath <- system.file("extdata", "parquet_dictionary.csv",
                         package = "parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE) |>
        as.data.frame()

    ## Validate data_type
    g_types <- unique(ftable$general_data_type)
    type_string <- paste(g_types, collapse = "|")
    confirm_data_type(data_type,
                      filter_col = "general_data_type",
                      filter_string = type_string)

    ## Find corresponding general_data_type
    gen_type <- output_file_types(filter_col = "data_type",
                                  filter_string = data_type)$general_data_type

    ## Pull and return column info in order
    rel_cols <- ftable %>%
        dplyr::filter(general_data_type == gen_type) %>%
        dplyr::arrange(position)

    return(rel_cols)
}

#' @title Return a URL associated with repo of parquet files
#' @description 'get_parquet_url' returns a URL associated with a repo that
#' holds data in the form of parquet files. The version can be specified, or the
#' latest version can be automatically returned.
#' @param url_type String: either 'repo', 'file', or 'httpfs'. Indicates which type of URL
#' to return. 'repo' is the URL of the full repo, 'file' is the prefix to use
#' when downloading files through standard protocols, and 'httpfs' is the prefix
#' to use when using DuckDB/DBI to download files.
#' @param version String: which numbered version of the repo to retrieve, or the
#' string 'latest' to return the latest version. Default: 'latest'
#' @return String: URL associated with a parquet file repo
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_parquet_url("repo", "latest")
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @rdname get_parquet_url
#' @export
#' @importFrom readr read_csv
get_parquet_url <- function(url_type, version = "latest") {
    ## Confirm url_type
    url_ind <- substr(tolower(url_type), 1, 1)

    if (!url_ind %in% c("r", "f", "h")) {
        stop(paste0("'", url_type, "' is not a valid 'url_type'. Please enter 'repo', 'file', or 'httpfs'."))
    }

    ## Load in parquet repo URL table
    fpath <- system.file("extdata", "parquet_repos.csv",
                         package = "parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE) |>
        as.data.frame()

    if (version == "latest") {
        p_row <- ftable[ftable$latest == "Y"]
    } else {
        if (!version %in% ftable$version) {
            stop(paste0("'", version, "' is not a valid version. Please enter a different version or 'latest'."))
        }
        p_row <- ftable[ftable$version == version]
    }

    if (url_ind == "h") {
        p_url <- p_row$httpfs_url
    } else if (url_ind == "f") {
        p_url <- p_row$file_url
    } else if (url_ind == "r") {
        p_url <- p_row$repo_url
    }

    return(p_url)
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
#' output_file_types(). The allowed values can optionally be restricted by
#' providing a column name and string/regular expression to filter the
#' output_file_types() data frame with.
#' @param data_type String: input to be validated
#' @param filter_col String (optional): name of column to filter by
#' @param filter_string String (optional): string to filter for within
#' 'filter_col'
#' @return NULL
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  confirm_data_type("relative_abundance")
#'  confirm_data_type("realtive_abundance", "tool", "humann")
#'  confirm_data_type(c("relative_abundance", "viral_clusters"))
#'  confirm_data_type("horse")
#'  }
#' }
#' @rdname confirm_data_type
#' @export
confirm_data_type <- function(data_type, filter_col = NULL, filter_string = NULL) {
    ## Get allowed types
    ftable <- output_file_types()
    all_types <- ftable$data_type

    if (!is.null(filter_col) & !is.null(filter_string)) {
        if (!filter_col %in% colnames(ftable)) {
            print_colnames <- paste(colnames(ftable), collapse = ", ")
            stop(paste0("'", filter_col, "' is not a column of output_files.csv. Please choose one of the following: ", print_colnames))
        }

        filter_ind <- TRUE
        filtered_types <- output_file_types(filter_col, filter_string)$data_type
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
            print_filtered <- paste(filtered_types, collapse = "\n")
            stop(paste0("'", data_type,
                        "' is not an allowed value for this function. Please enter one of the following values:\n",
                        print_filtered))
        }
    } else {
        if (!data_type %in% all_types) {
            stop(paste0("'", data_type,
                        "' is not an allowed value for 'data_type'. Please enter a value found in output_file_types()."))
        }
    }
}

#' @title Validate DuckDB connection argument
#' @description 'confirm_duckdb_con' checks that an object is a valid DuckDB
#' connection object.
#' @param con Object to validate
#' @return NULL
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- db_connect()
#'  confirm_duckdb_con(con)
#'  confirm_duckdb_con("horse")
#'  }
#' }
#' @rdname confirm_duckdb_con
#' @export
confirm_duckdb_con <- function(con) {
    if (class(con)[1] != "duckdb_connection") {
        stop("Please provide a valid 'duckdb_connection' object.")
    }
}
