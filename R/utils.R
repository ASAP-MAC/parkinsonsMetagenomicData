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
#' @param filter_col Character string (optional): name of the column to filter
#' by
#' @param filter_string Character string (optional): string/regular expression
#' to filter the selected column with.
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

#' @title Read in extdata/biobakery_file_definitions.csv
#' @description 'biobakery_files' reads in the table
#' extdata/biobakery_file_definitions.csv.
#' @return Tibble with columns 'DataType', 'Tool', 'Description', and
#' Units/Normalization'
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  biobakery_files()
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @rdname biobakery_files
#' @export
#' @importFrom readr read_csv
biobakery_files <- function() {
    ## Read file
    fpath <- system.file("extdata", "biobakery_file_definitions.csv",
                         package = "parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE)

    return(ftable)
}

#' @title Retrieve column info for parquet files based on original file type
#' @description 'parquet_colinfo' returns the column info associated with a
#' parquet file made from a particular output file type.
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as part of the name of a file in the repo
#' https://huggingface.co/datasets/waldronlab/metagenomics_mac or
#' https://huggingface.co/datasets/waldronlab/metagenomics_mac_examples.
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
#' @importFrom dplyr filter
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

    ## Pull and return column info
    rel_cols <- ftable %>%
        dplyr::filter(general_data_type == gen_type)

    return(rel_cols)
}

#' @title Detect which accepted data type a string is referring to
#' @description 'detect_data_type' parses the longest value matching a value in
#' output_file_types()$data_type.
#' @param string String(s): a single string or vector of strings to parse
#' @return String(s): detected data type values
#' @examples
#' \dontrun{
#' if(interactive()){
#'  detect_data_type("genefamilies_cpm_but_sorted")
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_extract}}
#' @rdname detect_data_type
#' @export
#' @importFrom stringr str_extract
detect_data_type <- function(string) {
    ## Retrieve and sort all possible types
    data_types <- output_file_types()$data_type
    reg_types <- paste0(data_types[order(nchar(data_types), decreasing = TRUE)],
                        collapse = "|")

    ## Extract first match from input
    matches <- stringr::str_extract(string, reg_types)

    return(matches)
}

#' @title Choose the most appropriate DuckDB view/table for filtering
#' @description 'pick_projection' takes a data type and the name of a feature
#' to filter that data by and chooses the appropriate DuckDB view/table for
#' performing that filtering in an efficient way. If no view is tailored to the
#' specific feature, a view/table sorted by UUID will be the default.
#' @param con DuckDB connection object of class 'duckdb_connection'. This
#' connection contains the views/tables to select from.
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as the name of a view found in
#' DBI::dbListTables(con), indicating which view to collect data from.
#' @param feature_name Single string: the name of the feature that the file is
#' intended to be filtered by. Default = "uuid"
#' @return Single string: the name of a DuckDB view/table
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
#'                           data_types = "relative_abundance")
#'  pick_projection(con, "relative_abundance", "clade_name_species")
#'  pick_projection(con, "relative_abundance")
#'  }
#' }
#' @seealso
#'  \code{\link[DBI]{dbListTables}}
#' @rdname pick_projection
#' @export
#' @importFrom DBI dbListTables
pick_projection <- function(con, data_type, feature_name = "uuid") {
    ## Check input
    # con
    confirm_duckdb_con(con)

    # data_type
    confirm_data_type(data_type)

    ## Find available views
    tbs <- DBI::dbListTables(con)

    ## Filter for views that match data_type
    detected_types <- detect_data_type(tbs)
    matching_views <- tbs[which(detected_types == data_type)]

    if (length(matching_views) == 0) {
        stop(paste0("'", data_type, "' does not match any existing views." ))
    }

    ## Create expected view
    eview <- paste0(data_type, "_", feature_name)

    ## Default to "uuid" view or first view available if no exact match
    if (!eview %in% matching_views) {
        if (any(grepl("uuid", matching_views))) {
            cview <- matching_views[grepl("uuid", matching_views)]
        } else {
            cview <- matching_views[1]
        }
        message("Exact match for '", eview, "' not found, selecting '", cview, "'")
    } else {
        cview <- eview
    }

    return(cview)
}

#' @title Return a table with information about available Hugging Face repos.
#' @description 'get_repo_info' returns a table of information associated with
#' each Hugging Face repo that contains relevant parquet files.
#' @return Data frame: A table of repo information, including information on
#' overall organization, name, URL, and whether or not the repo is the selected
#' default.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_repo_info()
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @rdname get_parquet_url
#' @export
#' @importFrom readr read_csv
get_repo_info <- function() {
    ## Load in parquet repo URL table
    fpath <- system.file("extdata", "parquet_repos.csv",
                         package = "parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE) |>
        as.data.frame()

    return(ftable)
}

#' @title Convert standard https:// URLs to httpfs-compatible hf:// URLs
#' @description 'file_to_hf' converts standard https:// URLs representing files
#' in a Hugging Face repo to URLs compatible with httpfs as described in the
#' \href{https://duckdb.org/docs/stable/core_extensions/httpfs/hugging_face.html}{DuckDB Docs}
#' @param url String: a URL referencing a single file in a Hugging Face repo.
#' @return String: a URL referencing the same file in a format matching the
#' httpfs protocol.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  file_to_hf("https://huggingface.co/datasets/waldronlab/metagenomics_mac/resolve/main/relative_abundance.parquet")
#'  }
#' }
#' @export
file_to_hf <- function(url) {
    hf_url <- url |>
        gsub(pattern = "https://huggingface.co/", replacement = "hf://") |>
        gsub(pattern = "resolve/main/", replacement = "")

    return(hf_url)
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
#'  confirm_data_type("relative_abundance", "tool", "humann")
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

#' @title Validate 'filter_values' argument
#' @description 'confirm_filter_values' checks that a named list is valide to be
#' used as a 'filter_values' argument for various functions in
#' parkinsonsMetagenomicData. Specifically, the input should be a named list,
#' where the element name equals the name of a column to be
#' filtered and element value equals a vector of exact column values. Base usage
#' of 'confirm_filter_values' just confirms that the object is a named list, and
#' if any of the elements are named 'uuid', validates that the values of that
#' element are valid uuids. If a vector of available features is provided, the
#' names of the elements of the list will be compared to that vector.
#' @param filter_values Named list: input to be validated
#' @param available_features Character vector: features that the list element
#' names should be found in. Default: NULL
#' @return NULL
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  l1 <- list(uuid = "56aa2ad5-007d-407c-a644-48aac1e9a8f0", animals = c("frog", "horse"))
#'  l2 <- list(uuid = "blue")
#'  confirm_filter_values(l1)
#'  confirm_filter_values(l1, c("uuid", "animals", "shapes"))
#'  confirm_filter_values(l1, c("animals", "shapes"))
#'  confirm_filter_values(l2)
#'  }
#' }
#' @rdname confirm_filter_values
#' @export
confirm_filter_values <- function(filter_values, available_features = NULL) {
    ## Check that object is a named list
    if (!is.list(filter_values) || is.null(names(filter_values))) {
        stop("'filter_values' must be a named list of column = values")
    }

    ## If any of the list items are named 'uuid', run confirm_uuids()
    if (!is.null(filter_values[["uuid"]])) {
        confirm_uuids(filter_values[["uuid"]])
    }

    ## Confirm all filter features are available if availability provided
    if (!is.null(available_features)) {
        for (n in names(filter_values)) {
            if (!n %in% available_features) {
                stop("'", n,
                     "' is not an available feature. All list elements should be named one of the following:\n",
                     paste0(available_features, collapse = ", "))
            }
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
    ## Check that object class is valid
    if (class(con)[1] != "duckdb_connection") {
        stop("Please provide a valid 'duckdb_connection' object.")
    }
}

#' @title Validate DuckDB view/table argument
#' @description 'confirm_duckdb_view' checks that an object is a valid DuckDB
#' table connection object
#' @param view Object to validate
#' @return NULL
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- accessParquetData(data_types = "pathcoverage_unstratified")
#'  view <- tbl(con, "pathcoverage_unstratified_uuid")
#'  confirm_duckdb_view(view)
#'  }
#' }
#' @rdname confirm_duckdb_view
#' @export
confirm_duckdb_view <- function(view) {
    ## Check that object class is valid
    if (class(view)[1] != "tbl_duckdb_connection") {
        stop("Please provide a valid object of the class 'tbl_duckdb_connection'.")
    }
}

#' @title Validate 'repo' argument
#' @description 'confirm_repo' checks that a single string is a valid repo name
#' as listed in get_repo_info() or a NULL value.
#' @param repo String or NULL: input to be validated
#' @return NULL
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  confirm_repo(NULL)
#'  confirm_repo("horse")
#'  confirm_repo("waldronlab/metagenomics_mac")
#'  }
#' }
#' @rdname confirm_repo
#' @export
confirm_repo <- function(repo) {
    ri <- get_repo_info()
    d <- ri$repo_name[ri$default == "Y"]

    if (!is.null(repo) && !repo %in% ri$repo_name) {
        stop(paste0("Please provide one of the following valid repo names or NULL to select the default (", d, "):\n", paste(ri$repo_name, collapse = ", ")))
    }
}
