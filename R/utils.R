#' @title Get location of dedicated file cache
#' @description 'pMD_get_cache' returns the location of the dedicated
#' parkinsonsMetagenomicData file cache or creates it if it does not exist.
#' @return BiocFileCache cache object
#' @examples
#' pMD_get_cache()
#' @seealso
#'  \code{\link[tools]{userdir}}
#'  \code{\link[BiocFileCache]{BiocFileCache-class}},
#'  \code{\link[BiocFileCache]{BiocFileCache}}
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
#' output_file_types()
#' output_file_types("tool", "metaphlan")
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
            stop(paste0("'", filter_col, "' is not a column of ",
                    "output_files.csv. Please choose one of the following: ",
                    print_colnames))
        }

        ftable <- ftable %>%
            filter(grepl(filter_string, .data[[filter_col]],
                        ignore.case = TRUE))
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
#' biobakery_files()
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
#' parquet_colinfo("viral_clusters")
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
                                filter_string = data_type)$general_data_type |>
        unique()

    ## Pull and return column info
    rel_cols <- ftable %>%
        dplyr::filter(.data$general_data_type == gen_type)

    return(rel_cols)
}

#' @title Detect which accepted data type a string is referring to
#' @description 'detect_data_type' parses the longest value matching a value in
#' output_file_types()$data_type.
#' @param string String(s): a single string or vector of strings to parse
#' @return String(s): detected data type values
#' @examples
#' detect_data_type("genefamilies_cpm_but_sorted")
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

    ## Loop through input
    matches <- rep(NA, length(string))

    for (i in seq_along(string)) {
        s <- string[i]

        if (grepl("_ref", s)) {
            ## Detect reference type
            match <- "reference"
        } else {
            ## Extract first match from input
            match <- stringr::str_extract(s, reg_types)
        }

        ## Save current type
        matches[i] <- match
    }

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
#' fpaths <- c(file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_uuid.parquet"),
#'             file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_pathway.parquet"))
#' con <- accessParquetData(local_files = fpaths,
#'                          data_type = "pathcoverage_unstratified")
#' pick_projection(con, "pathcoverage_unstratified")
#' pick_projection(con, "pathcoverage_unstratified", feature_name = "pathway")
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
        stop("'", data_type, "' does not match any existing views." )
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
        message("Exact match for '", eview, "' not found, selecting '", cview,
                "'")
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
#' get_repo_info()
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @rdname get_repo_info
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

#' @title Return a table with information about curated metadata features
#' @description 'data_dict' returns a table of information associated with
#' each curated feature (not prefixed with "uncurated_") in sampleMetadata.
#' @return Data frame: A table of metadata feature information, including
#' information on allowed values and requiredness as well as other parameters.
#' @examples
#' data_dict()
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @rdname data_dict
#' @export
#' @importFrom readr read_csv
data_dict <- function() {
    ## Load in data dictionary table
    fpath <- system.file("extdata", "data_dictionary.csv",
                        package = "parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE) |>
        as.data.frame()

    return(ftable)

}

#' @title Return a table with information about available parquet reference
#' files.
#' @description 'get_ref_info' returns a table of information associated with
#' each parquet reference file. The table can optionally be filtered by
#' providing a column name to filter by and a string/regular expression to
#' filter the selected column with.
#' @param filter_col Character string (optional): name of the column to filter
#' by
#' @param filter_string Character string (optional): string/regular expression
#' to filter the selected column with.
#' @return Data frame: A table of ref information, including information on
#' general data types and tools served as well as descriptions.
#' @examples
#' get_ref_info()
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @rdname get_ref_info
#' @export
#' @importFrom readr read_csv
get_ref_info <- function(filter_col = NULL, filter_string = NULL) {
    ## Load in reference file info table
    fpath <- system.file("extdata", "ref_file_definitions.csv",
                        package = "parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE) |>
        as.data.frame()

    ## Filter table if requested
    if (!is.null(filter_col) & !is.null(filter_string)) {
        if (!filter_col %in% colnames(ftable)) {
            print_colnames <- paste(colnames(ftable), collapse = ", ")
            stop("'", filter_col, paste0("' is not a column of ",
                    "output_files.csv. Please choose one of the following: "),
                    print_colnames)
        }

        ftable <- ftable %>%
            filter(grepl(filter_string, .data[[filter_col]],
                         ignore.case = TRUE))
    }

    return(ftable)
}

#' @title Convert standard https:// URLs to httpfs-compatible hf:// URLs
#' @description 'file_to_hf' converts standard https:// URLs representing files
#' in a Hugging Face repo to URLs compatible with httpfs as described in the
#' \href{https://duckdb.org/docs/stable/core_extensions/httpfs/
#' hugging_face.html}{DuckDB Docs}
#' @param url String: a URL referencing a single file in a Hugging Face repo.
#' @return String: a URL referencing the same file in a format matching the
#' httpfs protocol.
#' @examples
#' file <- paste0("https://huggingface.co/datasets/waldronlab/",
#'                "metagenomics_mac/resolve/main/relative_abundance.parquet")
#' file_to_hf(file)
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
#' get_exts("path/file.tsv.gz")
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

#' @title Convert sample and feature metadata tables to a 'filter_values' list
#' @description 'convert_to_filter_values' converts two tables containing sample
#' and feature metadata to a list of the features and values that will be most
#' useful to use as filters. This list format is used for the 'filter_values'
#' argument in a number of readParquet.R functions.
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as part of the name of a view found in
#' DBI::dbListTables(con), indicating which views to consider when collecting
#' data.
#' @param sample_data Data frame: a table of sample metadata with a 'uuid'
#' column. Often created by accessing 'data(sampleMetadata)' and filtering or
#' otherwise transforming the result to only include samples of interest.
#' @param feature_data Data frame: a table of feature data. Each column will
#' become a filtering argument. Often created by accessing one of the files
#' listed in 'get_ref_info()' with 'load_ref()', then filtering or otherwise
#' transforming the result to only include feature combinations of interest.
#' @return Named list: element name equals the column name to be
#' filtered and element value equals a vector of exact column values.
#' @examples
#' if (!exists("sampleMetadata", envir = environment())) {
#'     data("sampleMetadata", package = "parkinsonsMetagenomicData",
#'     envir = environment())
#' }
#'
#' uuids <- c("8793b1dc-3ba1-4591-82b8-4297adcfa1d7",
#'            "cc1f30a0-45d9-41b1-b592-7d0892919ee7",
#'            "fb7e8210-002a-4554-b265-873c4003e25f",
#'            "d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
#'            "4985aa08-6138-4146-8ae3-952716575395",
#'            "8eb9f7ae-88c2-44e5-967e-fe7f6090c7af")
#'
#' sample_data <- sampleMetadata %>%
#'     filter(uuid %in% uuids) %>%
#'     select(where(~ !any(is.na(.x))))
#'
#' fpaths <- c(file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_uuid.parquet"),
#'             file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_pathway.parquet"))
#'
#' con <- accessParquetData(local_files = fpaths,
#'                          data_types = "pathcoverage_unstratified")
#'
#' refpath <- file.path(system.file("extdata",
#'                                  package = "parkinsonsMetagenomicData"),
#'                      "pathway_ref.parquet")
#'
#' pathway_ref <- load_ref("pathway_ref", file_path = refpath)
#' feature_data_genus <- pathway_ref %>%
#'     dplyr::filter(grepl("Faecalibacterium", pathway_genus)) %>%
#'     dplyr::select(pathway_uniref) %>%
#'     dplyr::rename(pathway = pathway_uniref)
#'
#' convert_to_filter_values(con = con, data_type = "pathcoverage_unstratified",
#'                          sample_data = sample_data,
#'                          feature_data = feature_data_genus)
#' @seealso
#'  \code{\link[DBI]{dbListTables}}
#' @rdname convert_to_filter_values
#' @export
#' @importFrom DBI dbListTables
convert_to_filter_values <- function(con, data_type, sample_data,
                                    feature_data) {
    ## Convert sample_data and feature_data to filter_values
    filter_values <- list()
    if (!is.null(feature_data)) {
        # Retrieve available projections
        projs <- DBI::dbListTables(con) |>
            gsub(pattern = paste0(data_type, "_"), replacement = "")

        # Determine primary filter column and values
        fcols <- colnames(feature_data)

        fsets <- vector(mode = "list", length = length(fcols))
        for (i in seq_along(fcols)) {
            cur_col <- fcols[i]
            names(fsets)[i] <- cur_col
            fsets[i] <- as.vector(unique(feature_data[,cur_col]))
        }

        filter_values <- c(filter_values, fsets)
    }

    return(filter_values)
}

#' @title Validate UUIDs
#' @description 'confirm_uuids' checks that a single string or vector of strings
#' are valid UUIDs.
#' @param uuids String or character vector: strings to validate
#' @return NULL (invisibly)
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' confirm_uuids("56aa2ad5-007d-407c-a644-48aac1e9a8f0")
#' confirm_uuids("horse")
#' @rdname confirm_uuids
#' @export
confirm_uuids <- function(uuids) {
    results <- c()
    for (x in uuids) {
        is_uuid <- grepl(paste0("[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}",
                                "-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}"), x)
        results <- c(results, is_uuid)
    }

    if (!any(is_uuid)) {
        bad_uuids <- paste(uuids[!results], collapse = "\n")
        stop("One or more values are not valid UUIDs.\n", bad_uuids)
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
#' @return NULL (invisibly)
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' confirm_data_type("relative_abundance")
#' confirm_data_type("relative_abundance", "tool", "humann")
#' confirm_data_type(c("relative_abundance", "viral_clusters"))
#' confirm_data_type("horse")
#' @rdname confirm_data_type
#' @export
confirm_data_type <- function(data_type, filter_col = NULL,
                                filter_string = NULL) {
    ## Get allowed types
    ftable <- output_file_types()
    all_types <- ftable$data_type

    if (!is.null(filter_col) & !is.null(filter_string)) {
        if (!filter_col %in% colnames(ftable)) {
            print_colnames <- paste(colnames(ftable), collapse = ", ")
            stop("'", filter_col, paste0("' is not a column of ",
                "output_files.csv. Please choose one of the following: "),
                print_colnames)
        }

        filter_ind <- TRUE
        filtered_types <- output_file_types(filter_col, filter_string)$data_type
    } else {
        filter_ind <- FALSE
    }

    ## Check that data_type is valid
    # length
    if (length(data_type) > 1) {
        stop("'data_type' should be a single value.")
    }

    # values
    if (filter_ind) {
        if (!data_type %in% filtered_types) {
            print_filtered <- paste(filtered_types, collapse = "\n")
            stop("'", data_type, paste0("' is not an allowed value for this ",
                "function. Please enter one of the following values:\n"),
                print_filtered)
        }
    } else {
        if (!data_type %in% all_types) {
            stop("'", data_type, paste0("' is not an allowed value for ",
            "'data_type'. Please enter a value found in output_file_types()."))
        }
    }
}

#' @title Validate 'filter_values' argument
#' @description 'confirm_filter_values' checks that a named list is valid to be
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
#' @return NULL (invisibly)
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' l1 <- list(uuid = "56aa2ad5-007d-407c-a644-48aac1e9a8f0",
#'            animals = c("frog", "horse"))
#' l2 <- list(uuid = "blue")
#' confirm_filter_values(l1)
#' confirm_filter_values(l1, c("uuid", "animals", "shapes"))
#' confirm_filter_values(l1, c("animals", "shapes"))
#' confirm_filter_values(l2)
#' @rdname confirm_filter_values
#' @export
confirm_filter_values <- function(filter_values, available_features = NULL) {
    ## Check that object is a named list or NULL
    if (!is.null(filter_values) &
        (!is.list(filter_values) || is.null(names(filter_values)))) {
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
                af_message <- paste(available_features, collapse = ", ")
                stop("'", n, paste0("' is not an available feature. All list ",
                    "elements should be named one of the following:\n"),
                    af_message)
            }
        }
    }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sample_data PARAM_DESCRIPTION
#' @param feature_data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' sample_data <- data.frame(id = c(1, 2, 3),
#'                           color = c("red", "blue", "yellow"))
#' feature_data <- data.frame(phylum = c("Actinobacteria", "Actinobacteria",
#'                                       "Firmicutes"),
#'                            class = c("Actinomycetia", "Coriobacteriia",
#'                                      "Clostridia"))
#' confirm_sample_feature_data(sample_data, feature_data)
#' sample_data$uuid <- c("a1", "b2", "c3")
#' confirm_sample_feature_data(sample_data, feature_data)
#' confirm_sample_feature_data(sample_data, "features")
#' confirm_sample_feature_data(NULL, feature_data)
#' confirm_sample_feature_data(NULL, NULL)
#' @rdname confirm_sample_feature_data
#' @export
confirm_sample_feature_data <- function(sample_data, feature_data) {
    ## Check that sample_data is a data frame that contains a 'uuid' column
    if (!is.null(sample_data)) {
        if (!is.data.frame(sample_data)) {
            stop("'sample_data' should be a data.frame.")
        } else if (!"uuid" %in% colnames(sample_data)) {
            message(paste0("'sample_data' does not have a 'uuid' column, all ",
                           "samples will be returned."))
        }
    }

    ## Check that feature_data is a data frame
    if (!is.null(feature_data)) {
        if (!is.data.frame(feature_data)) {
            stop("'feature_data' should be a data.frame.")
        }
    }

    ## Warn if neither sample_data nor feature_data is provided
    if (is.null(sample_data) & is.null(feature_data)) {
        message(paste0("No 'sample_data' or 'feature_data' provided, all data ",
                       "will be returned."))
    }
}

#' @title Validate DuckDB connection argument
#' @description 'confirm_duckdb_con' checks that an object is a valid DuckDB
#' connection object.
#' @param con Object to validate
#' @return NULL (invisibly)
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' con <- db_connect()
#' confirm_duckdb_con(con)
#' confirm_duckdb_con("horse")
#' @rdname confirm_duckdb_con
#' @export
confirm_duckdb_con <- function(con) {
    ## Check that object class is valid
    if (!methods::is(con, "duckdb_connection")) {
        stop("Please provide a valid 'duckdb_connection' object.")
    }
}

#' @title Validate DuckDB view/table argument
#' @description 'confirm_duckdb_view' checks that an object is a valid DuckDB
#' table connection object
#' @param view Object to validate
#' @return NULL (invisibly)
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' fpaths <- c(file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_uuid.parquet"),
#'             file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_pathway.parquet"))
#' con <- accessParquetData(local_files = fpaths,
#'                          data_types = "pathcoverage_unstratified")
#' view <- tbl(con, "pathcoverage_unstratified_uuid")
#' confirm_duckdb_view(view)
#' @rdname confirm_duckdb_view
#' @export
confirm_duckdb_view <- function(view) {
    ## Check that object class is valid
    if (!methods::is(view, "tbl_duckdb_connection")) {
        stop(paste0("Please provide a valid object of the class ",
                    "'tbl_duckdb_connection'."))
    }
}

#' @title Validate 'repo' argument
#' @description 'confirm_repo' checks that a single string is a valid repo name
#' as listed in get_repo_info() or a NULL value.
#' @param repo String or NULL: input to be validated
#' @return NULL (invisibly)
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' confirm_repo(NULL)
#' confirm_repo("horse")
#' confirm_repo("waldronlab/metagenomics_mac")
#' @rdname confirm_repo
#' @export
confirm_repo <- function(repo) {
    ri <- get_repo_info()
    d <- ri$repo_name[ri$default == "Y"]

    if (!is.null(repo) && !repo %in% ri$repo_name) {
        ri_message <- paste(ri$repo_name, collapse = ", ")
        stop(paste0("Please provide one of the following valid repo names or ",
                    "NULL to select the default ("), d, "):\n", ri_message)
    }
}

#' @title Validate 'ref' argument
#' @description 'confirm_ref' checks that a single string is a valid reference
#' file name as listed in get_ref_info() or a NULL value.
#' @param ref String: input to be validated
#' @return NULL (invisibly)
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' confirm_ref("horse")
#' confirm_ref("clade_name_ref")
#' @rdname confirm_ref
#' @export
confirm_ref <- function(ref) {
    ri <- get_ref_info()

    if (!ref %in% ri$ref_file) {
        ri_message <- paste(ri$ref_file, collapse = ", ")
        stop(paste0("Please provide one of the following valid reference file ",
                    "names:\n"), ri_message)
    }
}

#' @title Pull the individual column roles from parquet_colinfo() output
#' @description 'find_tse_cols' saves space by organizing all column roles into
#' a single list object.
#' @param colinfo Dataframe: output from parquet_colinfo()
#' @return A list of names of the columns marked as the following roles: cname,
#' cdata, rname, rdata, and assay
#' @examples
#' find_tse_cols(parquet_colinfo("pathcoverage_unstratified"))
#' @rdname find_tse_cols
#' @export
find_tse_cols <- function(colinfo) {
    ## Get columns for each se_role value
    cnames_col <- colinfo$col_name[colinfo$se_role == "cname"]
    cdata_cols <- colinfo$col_name[colinfo$se_role == "cdata"]
    rnames_col <- colinfo$col_name[colinfo$se_role == "rname"]
    rdata_cols <- colinfo$col_name[colinfo$se_role == "rdata"]
    assay_cols <- colinfo$col_name[colinfo$se_role == "assay"]

    ## Combine into list
    collist <- list(cnames_col = cnames_col, cdata_cols = cdata_cols,
                    rnames_col = rnames_col, rdata_cols = rdata_cols,
                    assay_cols = assay_cols)

    return(collist)
}

#' @title Build SummarizedExperiment assay tables
#' @description 'build_tse_assays' takes a number of pieces that are used to
#' create assay tables consistent with the SummarizedExperiment data type and
#' derivatives.
#' @param assay_cols Character vector: column(s) that indicate an assay
#' @param rnames_col Character string: column that supplies row names
#' @param cnames_col Character string: column that supplies column names
#' @param parquet_table Table or data frame: data taken directly from a parquet
#' file found in the repo of interest (see inst/extdata/parquet_repos.csv).
#' @param esamps Character vector: IDs of requested samples not present in
#' parquet_table. Default: NULL
#' @return A list of assay tables compatible with the SummarizedExperiment
#' format
#' @examples
#' fpaths <- c(file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_uuid.parquet"),
#'             file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_pathway.parquet"))
#'
#' con <- accessParquetData(local_files = fpaths,
#'                          data_types = "pathcoverage_unstratified")
#'
#' parquet_tbl <- tbl(con, "pathcoverage_unstratified_uuid") |> collect()
#'
#' atab <- build_tse_assays(assay_cols = "coverage",
#'                          rnames_col = "pathway",
#'                          cnames_col = "uuid",
#'                          parquet_table = parquet_tbl,
#'                          esamps = c("9b91f0a9-7f56-400d-a652-4fe6e1f1955e",
#'                                     "88a4d532-64fa-414c-b3d0-f02b291341c0"))
#' @seealso
#'  \code{\link[tidyselect]{all_of}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[tibble]{rownames}}
#' @rdname build_tse_assays
#' @export
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
build_tse_assays <- function(assay_cols, rnames_col, cnames_col, parquet_table,
                            esamps = NULL) {
    alist <- lapply(assay_cols, function(acol) {
        ## Select columns relevant to assay tables and format
        pdata <- parquet_table %>%
            select(tidyselect::all_of(c(rnames_col, acol, "uuid"))) %>%
            tidyr::pivot_wider(
                names_from  = tidyselect::all_of(cnames_col),
                values_from = tidyselect::all_of(acol),
                values_fill = 0
            ) %>%
            tibble::column_to_rownames({{rnames_col}}) %>%
            as.matrix()

        ## Add data from "empty samples" if provided
        edata <- matrix(NA, nrow(pdata), length(esamps),
                        dimnames = list(NULL, esamps))

        cbind(pdata, edata)
    })
    names(alist) <- assay_cols

    return(alist)
}

#' @title Build SummarizedExperiment colData table
#' @description 'build_tse_coldata' takes a number of pieces that are used to
#' create a colData table consistent with the SummarizedExperiment data type and
#' derivatives.
#' @param cnames_col Character string: column that supplies column names
#' @param cdata_cols Character string: column(s) that indicate colData
#' @param parquet_table Table or data frame: data taken directly from a parquet
#' file found in the repo of interest (see inst/extdata/parquet_repos.csv).
#' @param esamps Character vector: IDs of requested samples not present in
#' parquet_table. Default: NULL
#' @param empty_data Table or data frame (optional): data on samples not
#' included in parquet_table. Default: NULL
#' @return A colData table compatible with the SummarizedExperiment format
#' @examples
#' fpaths <- c(file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_uuid.parquet"),
#'             file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_pathway.parquet"))
#'
#' con <- accessParquetData(local_files = fpaths,
#'                          data_types = "pathcoverage_unstratified")
#'
#' parquet_tbl <- tbl(con, "pathcoverage_unstratified_uuid") |> collect()
#'
#' edat <- data.frame(uuid = c("9b91f0a9-7f56-400d-a652-4fe6e1f1955e",
#'                             "88a4d532-64fa-414c-b3d0-f02b291341c0"),
#'                    humann_header = c("# Pathway\tout_Coverage",
#'                                      "# Pathway\tout_Coverage"))
#'
#' cdat <- build_tse_coldata(cnames_col = "uuid",
#'                           cdata_cols = "humann_header",
#'                           parquet_table = parquet_table,
#'                           esamps = c("9b91f0a9-7f56-400d-a652-4fe6e1f1955e",
#'                                      "88a4d532-64fa-414c-b3d0-f02b291341c0"),
#'                           empty_data = edat)
#' @seealso
#'  \code{\link[tidyselect]{all_of}}
#'  \code{\link[dplyr]{distinct}}
#'  \code{\link[dplyr]{mutate-joins}}
#'  \code{\link[dplyr]{join_by}}
#' @rdname build_tse_coldata
#' @export
#' @importFrom tidyselect any_of
#' @importFrom dplyr distinct left_join join_by
build_tse_coldata <- function(cnames_col, cdata_cols, parquet_table,
                                esamps = NULL, empty_data = NULL) {
    ## Check if empty data was supplied and pull relevant columns if so
    if (!is.null(empty_data)) {
        etab <- empty_data %>%
            filter(.data$uuid %in% esamps) %>%
            select(tidyselect::any_of(c(cnames_col, cdata_cols))) %>%
            as.data.frame()
    }

    ## Select relevant info from main parquet table
    cdata <- parquet_table %>%
        select(tidyselect::any_of(c(cnames_col, cdata_cols))) %>%
        dplyr::distinct() %>%
        as.data.frame()

    ## Combine
    if (exists("etab")) {
        cdata <- rbind(cdata, etab)
    }

    ## Add sample metadata
    cdata <- cdata %>%
        dplyr::left_join(sampleMetadata, dplyr::join_by("uuid"))
    rownames(cdata) <- cdata[[cnames_col]]

    return(cdata)
}

#' @title Build SummarizedExperiment rowData table
#' @description 'build_tse_rowdata' takes a number of pieces that are used to
#' create a rowData table consistent with the SummarizedExperiment data type and
#' derivatives.
#' @param parquet_table Table or data frame: data taken directly from a parquet
#' file found in the repo of interest (see inst/extdata/parquet_repos.csv).
#' @param rnames_col Character string: column that supplies row names
#' @param cdata_cols Character string: column(s) that indicate rowData
#' @return A rowData table compatible with the SummarizedExperiment format
#' @examples
#' fpaths <- c(file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_uuid.parquet"),
#'             file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_pathway.parquet"))
#'
#' con <- accessParquetData(local_files = fpaths,
#'                          data_types = "pathcoverage_unstratified")
#'
#' parquet_tbl <- tbl(con, "pathcoverage_unstratified_uuid") |> collect()
#'
#' rdat <- build_tse_rowdata(parquet_table = parquet_table,
#'                           rnames_col = "pathway",
#'                           rdata_cols = c("pathway_uniref", "pathway_genus",
#'                                          "pathway_species"))
#' @seealso
#'  \code{\link[tidyselect]{all_of}}
#'  \code{\link[dplyr]{distinct}}
#' @rdname build_tse_rowdata
#' @export
#' @importFrom tidyselect any_of
#' @importFrom dplyr distinct
build_tse_rowdata <- function(parquet_table, rnames_col, rdata_cols) {
    rdata <- parquet_table %>%
        select(tidyselect::any_of(c(rnames_col, rdata_cols))) %>%
        dplyr::distinct() %>%
        as.data.frame()
    rownames(rdata) <- rdata[[rnames_col]]

    return(rdata)
}

#' @title Confirm that SummarizedExperiment rowData, colData, and assays have
#' the same row/column orders
#' @description 'order_tse_elements' is a precaution to make sure that all
#' SummarizedExperiment elements are ordered the same.
#' @param rdata Table: SummarizedExperiment rowData
#' @param cdata Table: SummarizedExperiment colData
#' @param alist List of tables: SummarizedExperiment assays
#' @return List of three elements: a rowData table, a colData table, and a list
#' of assay tables
#' @examples
#' fpaths <- c(file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_uuid.parquet"),
#'             file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_pathway.parquet"))
#'
#' con <- accessParquetData(local_files = fpaths,
#'                          data_types = "pathcoverage_unstratified")
#'
#' rdata <- build_tse_rowdata(parquet_table = parquet_table,
#'                           rnames_col = "pathway",
#'                           rdata_cols = c("pathway_uniref", "pathway_genus",
#'                                          "pathway_species"))
#' cdata <- build_tse_coldata(cnames_col = "uuid",
#'                           cdata_cols = "humann_header",
#'                           parquet_table = parquet_table)
#' alist <- build_tse_assays(assay_cols = "coverage",
#'                          rnames_col = "pathway",
#'                          cnames_col = "uuid",
#'                          parquet_table = parquet_tbl)
#'
#' ordered <- order_tse_elements(rdata, cdata, alist)
#' @rdname order_tse_elements
#' @export
order_tse_elements <- function(rdata, cdata, alist) {
    ## Get rows that exist in both rowData/colData and assays
    rowids <- intersect(rownames(rdata), unlist(lapply(alist, rownames)))
    colids <- intersect(rownames(cdata), unlist(lapply(alist, colnames)))

    ## Order rowData, colData, and assays the same
    rdata <- rdata[rowids,, drop = FALSE]
    cdata <- cdata[colids,, drop = FALSE]
    alist <- lapply(alist, function(x) x[rowids, colids, drop = FALSE])

    ## Package for return
    combined <- list(rdata = rdata, cdata = cdata, alist = alist)

    return(combined)
}

#' @title Standardize the order of a vector of delimited strings
#' @description 'standardize_ordering' takes a vector of strings, splits each of
#' them on a specified delimiter, orders them, then re-collapses them with the
#' same delimiter. This confirms that when calling unique(), there are no
#' strings that contain the same elements but in a different order.
#' @param vec Vector of strings: vector of strings to be standardized
#' @param delim Character: delimiter to split each of the strings by.
#' @return Vector of strings
#' @examples
#' vec <- c("horse|gecko|frog",
#'          "cow|camel|fish",
#'          "frog|gecko|horse")
#'
#' standardize_ordering(vec, delim = "|")
#' @rdname standardize_ordering
#' @export
#' @importFrom stringr str_split str_escape
standardize_ordering <- function(vec, delim) {
    vec <- lapply(vec, function(x) {
        if (!is.na(x)) {
            stringr::str_split(x, pattern = stringr::str_escape(delim)) |>
                unlist() |>
                sort() |>
                paste(collapse = delim)
        } else { x }
        }) |>
        unlist()

    return(vec)
}

#' @title Retrieve the URL of the data source from a lazy DuckDB connection
#' @description 'get_view_source' takes a DuckDB connection object and a lazy
#' table using one of the connection's views/tables as a source and returns the
#' source URL.
#' @param con DuckDB connection object of class 'duckdb_connection'. This
#' connection contains the source view/table
#' @param lazy Lazy table using one of the DuckDB connection's views/tables as a
#' source.
#' @return String: the URL of the data source used
#' @examples
#' \donttest{
#'  con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
#'                           data_types = "pathcoverage_unstratified")
#'  lazy <- tbl(con, "pathcoverage_unstratified_pathway") |>
#'              filter(grepl("UMP biosynthesis", pathway))
#'
#'  get_view_source(con, lazy)
#' }
#' @seealso
#'  \code{\link[stringr]{str_extract}}
#'  \code{\link[dbplyr]{lazy_multi_join_query}}
#'  \code{\link[DBI]{dbGetQuery}}
#' @rdname get_view_source
#' @export
#' @importFrom stringr str_extract
#' @importFrom dbplyr sql_render
#' @importFrom DBI dbGetQuery
get_view_source <- function(con, lazy) {
    proj_name <- stringr::str_extract(as.character(dbplyr::sql_render(lazy)),
                                      "(?<=FROM (?=[^(])).+?(?=\\n)")
    proj_source <- DBI::dbGetQuery(con,
                                   paste0("SELECT sql FROM duckdb_views() ",
                                            "WHERE view_name = '",
                                            proj_name, "';"))[1,1]
    proj_url <- stringr::str_extract(proj_source, "(?<=').+?(?=')")

    return(proj_url)
}
