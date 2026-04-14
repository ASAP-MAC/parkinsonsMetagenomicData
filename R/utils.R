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
#' @noRd
#' @importFrom readr read_csv
#' @importFrom dplyr filter
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
            stop("'", filter_col, "' is not a column of ",
                 "output_files.csv. Please choose one of the following: ",
                 print_colnames)
        }

        ftable <- ftable %>%
            dplyr::filter(grepl(filter_string, .data[[filter_col]],
                        ignore.case = TRUE))
    }

    return(ftable)
}

#' @title Read in extdata/biobakery_file_definitions.csv
#' @description 'biobakery_files' reads in the table
#' extdata/biobakery_file_definitions.csv, which contains information about
#' all available microbiome data types including their tool of origin,
#' description, and units/normalization method.
#' @return Tibble with columns 'data_type', 'tool', 'description', and
#' 'units_normalization'
#' @examples
#' biobakery_files()
#' @seealso
#'  \code{\link[readr]{read_delim}}
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
#' @noRd
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
#' @noRd
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

#' @title Apply filtering and custom view transformations to a DuckDB view
#' @description 'prepare_view' accesses a DuckDB view created by
#' 'accessParquetData' and applies requested filtering and transformations.
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as part of the name of a view found in
#' DBI::dbListTables(con), indicating which views to consider when collecting
#' data.
#' @param filter_values Named list: element name equals the column name to be
#' filtered and element value equals a vector of exact column values.
#' @param custom_view Saved object with the initial class
#' 'tbl_duckdb_connection' (optional): DuckDB tables/views can be accessed with
#' with the 'dplyr::tbl' function, and piped into additional functions such as
#' 'dplyr::filter' prior to loading into memory with 'dplyr::collect'. A
#' particular sequence of function calls can be saved and provided to this
#' function for collection and formatting as a Summarized Experiment. See the
#' function example.
#' @param include_empty_samples Boolean (optional): should samples provided via
#' a 'uuid' argument within 'filter_values' be included in the final
#' TreeSummarizedExperiment if they do not show up in the results from filtering
#' the source parquet data file.
#' @return Named list: the 'working_view' element is a DuckDB database view or
#' table. This is still lazy until collect() is called. The 'sample_headers'
#' element contains metadata for any empty samples.
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
#' custom_filter <- dplyr::tbl(con, "pathcoverage_unstratified_pathway") |>
#'                  dplyr::filter(grepl("UMP biosynthesis", pathway))
#'
#' uuids <- c("8793b1dc-3ba1-4591-82b8-4297adcfa1d7",
#'            "cc1f30a0-45d9-41b1-b592-7d0892919ee7",
#'            "fb7e8210-002a-4554-b265-873c4003e25f",
#'            "d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
#'            "4985aa08-6138-4146-8ae3-952716575395",
#'            "8eb9f7ae-88c2-44e5-967e-fe7f6090c7af")
#'
#' prep <- prepare_view(con,
#'                      data_type = "pathcoverage_unstratified",
#'                      filter_values = list(uuid = uuids),
#'                      custom_view = custom_filter,
#'                      include_empty_samples = FALSE)
#' working_view <- prep$working_view
#' @rdname prepare_view
#' @noRd
#' @importFrom dplyr tbl
prepare_view <- function(con, data_type, filter_values, custom_view,
                         include_empty_samples) {
    sample_headers <- NULL
    if (!is.null(filter_values)) {
        if (!is.null(custom_view)) {
            working_view <- filter_parquet_view(custom_view, filter_values)
        } else {
            working_view <- interpret_and_filter(con, data_type, filter_values)

            if ("uuid" %in% names(filter_values) && include_empty_samples) {
                sample_headers <- get_cdata_only(con, data_type,
                                                 filter_values$uuid)
                full_empties <- setdiff(filter_values$uuid, sample_headers$uuid)
                emat <- as.data.frame(matrix(nrow = length(full_empties),
                                             ncol = ncol(sample_headers),
                                             dimnames = list(c(),
                                                    colnames(sample_headers))))
                emat$uuid <- full_empties
                sample_headers <- rbind(sample_headers, emat)
            }
        }
    } else {
        if (!is.null(custom_view)) {
            working_view <- custom_view
        } else {
            proj <- pick_projection(con, data_type)
            working_view <- dplyr::tbl(con, proj)
        }
    }

    wv_list <- list(working_view = working_view,
                    sample_headers = sample_headers)

    return(wv_list)
}

#' @title Collect a DuckDB view and provide important notifications
#' @description 'collect_and_notify' calls dplyr::collect() on a DuckDB database
#' view or table and notifies the user if it accesses a remote resource that may
#' cause delays or failure.
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as part of the name of a view found in
#' DBI::dbListTables(con), indicating which views to consider when collecting
#' data.
#' @param working_view DuckDB database view or table
#' @return Tibble data.frame
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
#' custom_filter <- dplyr::tbl(con, "pathcoverage_unstratified_pathway") |>
#'                  dplyr::filter(grepl("UMP biosynthesis", pathway))
#'
#' uuids <- c("8793b1dc-3ba1-4591-82b8-4297adcfa1d7",
#'            "cc1f30a0-45d9-41b1-b592-7d0892919ee7",
#'            "fb7e8210-002a-4554-b265-873c4003e25f",
#'            "d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
#'            "4985aa08-6138-4146-8ae3-952716575395",
#'            "8eb9f7ae-88c2-44e5-967e-fe7f6090c7af")
#'
#' prep <- prepare_view(con,
#'                      data_type = "pathcoverage_unstratified",
#'                      filter_values = list(uuid = uuids),
#'                      custom_view = custom_filter,
#'                      include_empty_samples = FALSE)
#'
#' collected_view <- collect_and_notify(con,
#'                                      data_type = "pathcoverage_unstratified",
#'                                      prep$working_view)
#' @rdname collect_and_notify
#' @noRd
#' @importFrom dplyr collect
collect_and_notify <- function(con, data_type, working_view) {
    current_gen <- output_file_types(filter_col = "data_type",
                filter_string = paste0("^", data_type, "$"))$general_data_type
    hf_ind <- get_view_source(con, working_view) |> startsWith("hf")
    if (current_gen == "genefamilies" && hf_ind) {
        message("'", data_type, "' is a large data type, and collecting",
                " the query can take a while. To avoid going through the Hugging Face ",
                "API, download the source file ", get_view_source(con, working_view),
                " and provide it to accessParquetData() in the 'local files' ",
                "argument.")
    }

    collected_view <- working_view |>
        dplyr::collect()

    return(collected_view)
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
#' This includes column names, data types, descriptions, allowed values,
#' and whether fields are required or allow multiple values.
#' @return Data frame: A table of metadata feature information with columns:
#' 'ColName', 'ColClass', 'Unique', 'Required', 'MultipleValues',
#' 'Description', 'AllowedValues', 'Delimiter', 'Separater',
#' 'DynamicEnum', and 'DynamicEnumProperty'.
#' @examples
#' # View the sample metadata data dictionary
#' metadata_info <- data_dict()
#' head(metadata_info)
#'
#' # Check required fields
#' required_fields <- metadata_info[metadata_info$Required == "required", ]
#' required_fields$ColName
#' @seealso
#'  \code{\link[readr]{read_delim}}
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
#' @importFrom dplyr filter
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
            stop("'", filter_col, "' is not a column of ",
                 "output_files.csv. Please choose one of the following: ",
                 print_colnames)
        }

        ftable <- ftable %>%
            dplyr::filter(grepl(filter_string, .data[[filter_col]],
                         ignore.case = TRUE))
    }

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
#' file <- paste0("https://huggingface.co/datasets/waldronlab/",
#'                "metagenomics_mac/resolve/main/relative_abundance.parquet")
#' file_to_hf(file)
#' @noRd
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
#' @noRd
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
#'     utils::data("sampleMetadata", package = "parkinsonsMetagenomicData",
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
#'     dplyr::filter(uuid %in% uuids) %>%
#'     dplyr::select(where(~ !any(is.na(.x))))
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
#' @noRd
#' @importFrom DBI dbListTables
convert_to_filter_values <- function(con, data_type, sample_data,
                                    feature_data) {
    ## Convert sample_data and feature_data to filter_values
    filter_values <- list()
    if (!is.null(feature_data)) {
        # Determine primary filter column and values
        fcols <- colnames(feature_data)

        fsets <- vector(mode = "list", length = length(fcols))
        for (i in seq_along(fcols)) {
            cur_col <- fcols[i]
            names(fsets)[i] <- cur_col
            fsets[[i]] <- as.vector(unique(feature_data[,cur_col]))
        }

        filter_values <- c(filter_values, fsets)
    }

    if (!is.null(sample_data)) {
        # Add sample uuids
        uuid_arg <- list(uuid = sample_data$uuid)
        filter_values <- c(filter_values, uuid_arg)
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
#' try(confirm_uuids("56aa2ad5-007d-407c-a644-48aac1e9a8f0"))
#' try(confirm_uuids("horse"))
#' @rdname confirm_uuids
#' @noRd
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
#' try(confirm_data_type("relative_abundance"))
#' try(confirm_data_type("relative_abundance", "tool", "humann"))
#' try(confirm_data_type(c("relative_abundance", "viral_clusters")))
#' try(confirm_data_type("horse"))
#' @rdname confirm_data_type
#' @noRd
confirm_data_type <- function(data_type, filter_col = NULL,
                                filter_string = NULL) {
    ## Get allowed types
    ftable <- output_file_types()
    all_types <- ftable$data_type

    if (!is.null(filter_col) & !is.null(filter_string)) {
        if (!filter_col %in% colnames(ftable)) {
            print_colnames <- paste(colnames(ftable), collapse = ", ")
            stop("'", filter_col, "' is not a column of ",
                 "output_files.csv. Please choose one of the following: ",
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
            stop("'", data_type, "' is not an allowed value for this ",
                 "function. Please enter one of the following values:\n",
                 print_filtered)
        }
    } else {
        if (!data_type %in% all_types) {
            stop("'", data_type, "' is not an allowed value for ",
                 "'data_type'. Please enter a value found in output_file_types().")
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
#' try(confirm_filter_values(l1))
#' try(confirm_filter_values(l1, c("uuid", "animals", "shapes")))
#' try(confirm_filter_values(l1, c("animals", "shapes")))
#' try(confirm_filter_values(l2))
#' @rdname confirm_filter_values
#' @noRd
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
                stop("'", n, "' is not an available feature. All list ",
                     "elements should be named one of the following:\n",
                     af_message)
            }
        }
    }
}

#' @title Validate 'sample_data' and 'feature_data' arguments
#' @description 'confirm_sample_feature_data' checks that the 'sample_data' and
#' 'feature_data' arguments are of the correct format, contain key information,
#' and at least one is not NULL.
#' @param sample_data Table: sample data table to be validated
#' @param feature_data Table: feature data table to be validated
#' @return NULL (invisibly)
#' @details This function is intended to be used within another function as
#' input validation. If the input is valid, nothing will happen. If it is not,
#' the function will throw a 'stop()' error.
#' @examples
#' sample_data <- data.frame(id = c(1, 2, 3),
#'                           color = c("red", "blue", "yellow"))
#' feature_data <- data.frame(phylum = c("Actinobacteria", "Actinobacteria",
#'                                       "Firmicutes"),
#'                            class = c("Actinomycetia", "Coriobacteriia",
#'                                      "Clostridia"))
#' try(confirm_sample_feature_data(sample_data, feature_data))
#' sample_data$uuid <- c("a1", "b2", "c3")
#' try(confirm_sample_feature_data(sample_data, feature_data))
#' try(confirm_sample_feature_data(sample_data, "features"))
#' try(confirm_sample_feature_data(NULL, feature_data))
#' try(confirm_sample_feature_data(NULL, NULL))
#' @rdname confirm_sample_feature_data
#' @noRd
confirm_sample_feature_data <- function(sample_data, feature_data) {
    ## Check that sample_data is a data frame that contains a 'uuid' column
    if (!is.null(sample_data)) {
        if (!is.data.frame(sample_data)) {
            stop("'sample_data' should be a data.frame.")
        } else if (!"uuid" %in% colnames(sample_data)) {
            message("'sample_data' does not have a 'uuid' column, all ",
                    "samples will be returned.")
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
        message("No 'sample_data' or 'feature_data' provided, all data ",
            "will be returned.")
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
#' try(confirm_duckdb_con(con))
#' try(confirm_duckdb_con("horse"))
#' @rdname confirm_duckdb_con
#' @noRd
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
#' view <- dplyr::tbl(con, "pathcoverage_unstratified_uuid")
#' try(confirm_duckdb_view(view))
#' @rdname confirm_duckdb_view
#' @noRd
confirm_duckdb_view <- function(view) {
    ## Check that object class is valid
    if (!methods::is(view, "tbl_duckdb_connection")) {
        stop("Please provide a valid object of the class ",
             "'tbl_duckdb_connection'.")
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
#' try(confirm_repo(NULL))
#' try(confirm_repo("horse"))
#' try(confirm_repo("waldronlab/metagenomics_mac"))
#' @rdname confirm_repo
#' @noRd
confirm_repo <- function(repo) {
    ri <- get_repo_info()
    d <- ri$repo_name[ri$default == "Y"]

    if (!is.null(repo) && !repo %in% ri$repo_name) {
        ri_message <- paste(ri$repo_name, collapse = ", ")
        stop("Please provide one of the following valid repo names or ",
             "NULL to select the default (", d, "):\n", ri_message)
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
#' try(confirm_ref("horse"))
#' try(confirm_ref("clade_name_ref"))
#' @rdname confirm_ref
#' @noRd
confirm_ref <- function(ref) {
    ri <- get_ref_info()

    if (!ref %in% ri$ref_file) {
        ri_message <- paste(ri$ref_file, collapse = ", ")
        stop("Please provide one of the following valid reference file ",
             "names:\n", ri_message)
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
#' @noRd
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
#' parquet_tbl <- dplyr::tbl(con, "pathcoverage_unstratified_uuid") |>
#'                     dplyr::collect()
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
#' @noRd
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom dplyr select
build_tse_assays <- function(assay_cols, rnames_col, cnames_col, parquet_table,
                            esamps = NULL) {
    alist <- lapply(assay_cols, function(acol) {
        ## Select columns relevant to assay tables and format
        pdata <- parquet_table %>%
            dplyr::select(tidyselect::all_of(c(rnames_col, acol,
                                                cnames_col))) %>%
            tidyr::pivot_wider(
                names_from  = tidyselect::all_of(cnames_col),
                values_from = tidyselect::all_of(acol),
                values_fill = 0
            ) %>%
            tibble::column_to_rownames(var = rnames_col) %>%
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
#' parquet_tbl <- dplyr::tbl(con, "pathcoverage_unstratified_uuid") |>
#'                     dplyr::collect()
#'
#' edat <- data.frame(uuid = c("9b91f0a9-7f56-400d-a652-4fe6e1f1955e",
#'                             "88a4d532-64fa-414c-b3d0-f02b291341c0"),
#'                    humann_header = c("# Pathway\tout_Coverage",
#'                                      "# Pathway\tout_Coverage"))
#'
#' cdat <- build_tse_coldata(cnames_col = "uuid",
#'                           cdata_cols = "humann_header",
#'                           parquet_table = parquet_tbl,
#'                           esamps = c("9b91f0a9-7f56-400d-a652-4fe6e1f1955e",
#'                                      "88a4d532-64fa-414c-b3d0-f02b291341c0"),
#'                           empty_data = edat)
#' @seealso
#'  \code{\link[tidyselect]{all_of}}
#'  \code{\link[dplyr]{distinct}}
#'  \code{\link[dplyr]{mutate-joins}}
#'  \code{\link[dplyr]{join_by}}
#' @rdname build_tse_coldata
#' @noRd
#' @importFrom tidyselect any_of
#' @importFrom dplyr distinct left_join join_by filter select
#' @importFrom utils data
build_tse_coldata <- function(cnames_col, cdata_cols, parquet_table,
                                esamps = NULL, empty_data = NULL) {
    ## Check if empty data was supplied and pull relevant columns if so
    if (!is.null(empty_data)) {
        etab <- empty_data %>%
            dplyr::filter(.data$uuid %in% esamps) %>%
            dplyr::select(tidyselect::any_of(c(cnames_col, cdata_cols))) %>%
            as.data.frame()
    }

    ## Select relevant info from main parquet table
    cdata <- parquet_table %>%
        dplyr::select(tidyselect::any_of(c(cnames_col, cdata_cols))) %>%
        dplyr::distinct() %>%
        as.data.frame()

    ## Combine
    if (exists("etab")) {
        cdata <- rbind(cdata, etab)
    }

    ## Load sampleMetadata
    utils::data("sampleMetadata", package = "parkinsonsMetagenomicData",
         envir = environment())

    ## Add sample metadata
    cdata <- cdata %>%
        dplyr::left_join(sampleMetadata, by = cnames_col)
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
#' @param rdata_cols Character string: column(s) that indicate rowData
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
#' parquet_tbl <- dplyr::tbl(con, "pathcoverage_unstratified_uuid") |>
#'                     dplyr::collect()
#'
#' rdat <- build_tse_rowdata(parquet_table = parquet_tbl,
#'                           rnames_col = "pathway",
#'                           rdata_cols = c("pathway_uniref", "pathway_genus",
#'                                          "pathway_species"))
#' @seealso
#'  \code{\link[tidyselect]{all_of}}
#'  \code{\link[dplyr]{distinct}}
#' @rdname build_tse_rowdata
#' @noRd
#' @importFrom tidyselect any_of
#' @importFrom dplyr distinct select
build_tse_rowdata <- function(parquet_table, rnames_col, rdata_cols) {
    rdata <- parquet_table %>%
        dplyr::select(tidyselect::any_of(c(rnames_col, rdata_cols))) %>%
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
#' parquet_tbl <- dplyr::tbl(con, "pathcoverage_unstratified_uuid") |>
#'                     dplyr::collect()

#' rdata <- build_tse_rowdata(parquet_table = parquet_tbl,
#'                           rnames_col = "pathway",
#'                           rdata_cols = c("pathway_uniref", "pathway_genus",
#'                                          "pathway_species"))
#' cdata <- build_tse_coldata(cnames_col = "uuid",
#'                           cdata_cols = "humann_header",
#'                           parquet_table = parquet_tbl)
#' alist <- build_tse_assays(assay_cols = "coverage",
#'                          rnames_col = "pathway",
#'                          cnames_col = "uuid",
#'                          parquet_table = parquet_tbl)
#'
#' ordered <- order_tse_elements(rdata, cdata, alist)
#' @rdname order_tse_elements
#' @noRd
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
#' @noRd
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

#' @title Merge TreeSummarizedExperiment assays
#' @description 'merge_assays' takes the assay elements of multiple
#' TreeSummarizedExperiment objects and merges them into a single assay per
#' type.
#' @param merge_list List of TreeSummarizedExperiment objects
#' @return List of tables formatted as TreeSummarizedExperiment assays
#' @examples
#' fpath <- file.path(system.file("extdata",
#'                                package = "parkinsonsMetagenomicData"),
#'                    "sample_experiment_list.Rds")
#' sample_experiment_list <- readRDS(fpath)
#' assay_list <- merge_assays(sample_experiment_list)
#' @seealso
#'  \code{\link[SummarizedExperiment]{SummarizedExperiment-class}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[purrr]{reduce}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[dplyr]{mutate-joins}}
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[dplyr]{across}}
#'  \code{\link[tidyselect]{everything}}
#'  \code{\link[tidyr]{replace_na}}
#'  \code{\link[S4Vectors]{SimpleList-class}}
#' @rdname merge_assays
#' @noRd
#' @importFrom SummarizedExperiment assayNames assay
#' @importFrom purrr map reduce
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom dplyr full_join mutate across
#' @importFrom tidyselect everything
#' @importFrom tidyr replace_na
#' @importFrom S4Vectors SimpleList
merge_assays <- function(merge_list) {
    ## Check that assays match
    assay_names <-
        lapply(merge_list, SummarizedExperiment::assayNames) |>
        unique()

    if (length(assay_names) != 1) {
        stop("'merge_list' contains multiple assay types, please ",
             "provide a list where all assays match in type and order.")
    }

    ## Merge assays
    assay_list <- vector("list", length(assay_names[[1]]))
    names(assay_list) <- assay_names[[1]]
    for (i in seq_along(assay_list)) {
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

    return(assay_list)
}

#' @title Merge TreeSummarizedExperiment rowData
#' @description 'merge_rowdata' takes the rowData elements of multiple
#' TreeSummarizedExperiment objects and merges them into a single DataFrame of
#' rowData.
#' @param merge_list List of TreeSummarizedExperiment objects
#' @param assay_list List of TreeSummarizedExperiment assays or similarly
#' formatted tables
#' @return DataFrame formatted as a TreeSummarizedExperiment rowData object
#' @examples
#' fpath <- file.path(system.file("extdata",
#'                                package = "parkinsonsMetagenomicData"),
#'                    "sample_experiment_list.Rds")
#' sample_experiment_list <- readRDS(fpath)
#' assay_list <- merge_assays(sample_experiment_list)
#' rowData <- merge_rowdata(sample_experiment_list, assay_list)
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[purrr]{reduce}}
#'  \code{\link[SummarizedExperiment]{SummarizedExperiment-class}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[dplyr]{mutate-joins}}
#'  \code{\link[S4Vectors]{DataFrame-class}}
#' @rdname merge_rowdata
#' @noRd
#' @importFrom purrr map reduce
#' @importFrom SummarizedExperiment rowData
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom dplyr full_join
#' @importFrom S4Vectors DataFrame
merge_rowdata <- function(merge_list, assay_list) {
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

    rowData <- rowData[match(rownames(assay_list[[1]]), rownames(rowData)),]

    return(rowData)
}

#' @title Retrieve the URL or filepath of the data source from a lazy DuckDB
#' connection
#' @description 'get_view_source' takes a DuckDB connection object and a lazy
#' table using one of the connection's views/tables as a source and returns the
#' source URL or filepath.
#' @param con DuckDB connection object of class 'duckdb_connection'. This
#' connection contains the source view/table
#' @param lazy Lazy table using one of the DuckDB connection's views/tables as a
#' source.
#' @return String: the URL or filepath of the data source used
#' @examples
#' \donttest{
#'  con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
#'                           data_types = "pathcoverage_unstratified")
#'  lazy <- dplyr::tbl(con, "pathcoverage_unstratified_pathway") |>
#'              dplyr::filter(grepl("UMP biosynthesis", pathway))
#'
#'  get_view_source(con, lazy)
#' }
#'
#' fpaths <- c(file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_uuid.parquet"),
#'             file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_pathway.parquet"))
#'
#' con <- accessParquetData(local_files = fpaths,
#'                               data_types = "pathcoverage_unstratified")
#'
#' lazy <- dplyr::tbl(con, "pathcoverage_unstratified_pathway") |>
#'             dplyr::filter(grepl("UMP biosynthesis", pathway))
#'
#' get_view_source(con, lazy)
#' @seealso
#'  \code{\link[stringr]{str_extract}}
#'  \code{\link[dbplyr]{lazy_multi_join_query}}
#'  \code{\link[DBI]{dbGetQuery}}
#' @rdname get_view_source
#' @noRd
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
    proj_path <- stringr::str_match(proj_source, "['\"]([^'\"]+)['\"]")[,2]

    return(proj_path)
}

#' @title Submit and parse a GET request to the Hugging Face API
#' @description 'get_hf_api' composes, submits, and parses a GET request to the
#' Hugging Face API for a particular repo.
#' @param repo_name String: name of the Hugging Face repo to get info for
#' @return List of response elements
#' @examples
#' \donttest{
#'  get_hf_api("waldronlab/metagenomics_mac")
#' }
#' @seealso
#'  \code{\link[httr2]{request}}, \code{\link[httr2]{req_perform}},
#'  \code{\link[httr2]{resp_status}}, \code{\link[httr2]{resp_body_json}}
#' @rdname get_hf_api
#' @noRd
#' @importFrom httr2 request req_perform resp_status resp_body_json
get_hf_api <- function(repo_name) {
    # --- Step 1: Construct API URL and get repo info ---
    repo_api_url <- paste0("https://huggingface.co/api/datasets/", repo_name)

    # Build and perform request
    response <- httr2::request(repo_api_url) |>
        httr2::req_perform()

    # Check status code (maintaining current error message format)
    if (httr2::resp_status(response) != 200) {
        stop(
            "Failed to get repo info from Hugging Face API for '", repo_name,
            "'.\n",
            "Status code: ", httr2::resp_status(response), ".\n",
            "Please check if the repository name is correct and public. ",
            "The server may also be rate-limiting your IP."
        )
    }

    # Parse JSON response directly (httr2 handles conversion internally)
    repo_info <- httr2::resp_body_json(response, simplifyVector = TRUE)

    return(repo_info)
}

#' @title Filter API response for parquet files
#' @description 'check_for_parquet' filters the response obtained from calling
#' get_hf_api() to find parquet files.
#' @param repo_info List: API response elements from get_hf_api()
#' @param repo_name String: name of Hugging face repo
#' @return String vector: names of parquet files in repo
#' @examples
#' \donttest{
#'  repo_info <- get_hf_api("waldronlab/metagenomics_mac")
#'  check_for_parquet(repo_info, "waldronlab/metagenomics_mac")
#' }
#'
#' sample_response <- list(siblings = data.frame(rfilename = c(
#'                   "clade_name_ref.parquet",
#'                   "gene_family_ref.parquet",
#'                   "genefamilies_cpm_gene_family_uniref.parquet",
#'                   "genefamilies_cpm_stratified_gene_family_uniref.parquet")))
#' check_for_parquet(sample_response, "waldronlab/metagenomics_mac")
#' @rdname check_for_parquet
#' @noRd
check_for_parquet <- function(repo_info, repo_name) {
    # --- Step 2: Filter for Parquet files ---
    if (is.null(repo_info$siblings) || is.null(repo_info$siblings$rfilename)) {
        stop("Could not find file listing in the API response for '", repo_name,
             "'.")
    }

    all_files <- repo_info$siblings$rfilename
    parquet_files <- all_files[endsWith(all_files, ".parquet")]

    return(parquet_files)
}
#' @title Add file definitions to a list of file names
#' @description 'add_defs' uses biobakery_file_definitions.csv to assign
#' definitions to each parquet file provided.
#' @param result_df Data frame: must have the column 'filename'
#' @param verbose Boolean: should output be verbose, Default: FALSE
#' @return Data frame
#' @examples
#' result_df <- data.frame(filename = c("clade_name_ref.parquet",
#'                                "gene_family_ref.parquet",
#'                                "genefamilies_cpm_gene_family_uniref.parquet"),
#'                         url = c("https://huggingface.co/datasets/waldronlab/metagenomics_mac/resolve/main/clade_name_ref.parquet",
#'                           "https://huggingface.co/datasets/waldronlab/metagenomics_mac/resolve/main/gene_family_ref.parquet",
#'                           "https://huggingface.co/datasets/waldronlab/metagenomics_mac/resolve/main/genefamilies_cpm_gene_family_uniref.parquet"
#'                         ))
#'
#' def_df <- add_defs(result_df, verbose = FALSE)
#' @seealso
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{mutate-joins}}
#'  \code{\link[utils]{read.table}}
#' @rdname add_defs
#' @noRd
#' @importFrom dplyr mutate left_join
#' @importFrom utils read.csv
add_defs <- function(result_df, verbose) {
    # --- Step 5: Read definitions and join with file list ---
    def_path <- system.file(
        "extdata", "biobakery_file_definitions.csv",
        package = "parkinsonsMetagenomicData"
    )

    # Create data_type column for joining
    result_df <- dplyr::mutate(
        result_df,
        data_type = detect_data_type(.data$filename)
    )

    if (nzchar(def_path) && file.exists(def_path)) {
        if (verbose) message("Found definitions file. Joining metadata.")
        definitions <- utils::read.csv(def_path, stringsAsFactors = FALSE)

        # Perform the join
        result_df <- dplyr::left_join(result_df, definitions, by = "data_type")

    } else {
        if (verbose) message(
            "Data type definition file not found. ",
            "Install 'parkinsonsMetagenomicData' to add full metadata."
        )
        # Add empty columns so the function always returns the same structure
        result_df$tool <- NA_character_
        result_df$description <- NA_character_
        result_df$units_normalization <- NA_character_
    }

    return(result_df)
}
