### Access and format data from parquet files hosted on Hugging Face
### Refactored to delegate DuckDB plumbing, filtering, collection, and
### experiment assembly to curatedCore.

# --------------------------------------------------------------------------
# Internal: load the PMD curatedCore SchemaSpec (lazy singleton)
# --------------------------------------------------------------------------
#' @noRd
.pmd_env <- new.env(parent = emptyenv())
.pmd_env$schema <- NULL

#' @noRd
.get_pmd_schema <- function() {
    if (is.null(.pmd_env$schema)) {
        csv <- system.file("extdata", "curatedcore_schema.csv",
                           package = "parkinsonsMetagenomicData")
        .pmd_env$schema <- curatedCore::readSchemaSpec(csv, id_col = "uuid")
    }
    .pmd_env$schema
}

# --------------------------------------------------------------------------
# Internal: build a CuratedSource from PMD-style arguments
# --------------------------------------------------------------------------
#' Build a CuratedSource from PMD repo / local_files arguments
#'
#' Translates the legacy PMD \code{repo} and \code{local_files} arguments
#' into a \code{\link[curatedCore]{CuratedSource}} object.
#'
#' @param repo Character string (optional): Hugging Face repo name.
#' @param local_files Character vector (optional): paths to local parquet files.
#' @param data_types Character vector (optional): data types to load.
#' @return A \code{CuratedSource} object.
#' @noRd
.build_source <- function(repo = NULL, local_files = NULL,
                          data_types = NULL) {
    if (!is.null(local_files)) {
        ## Local parquet files → localParquetSource
        return(curatedCore::localParquetSource(local_files))
    }

    ## Remote Hugging Face repo → parquetRepoSource
    ri <- get_repo_info()
    if (is.null(repo)) {
        repo_row <- ri[ri$default == "Y", ]
    } else {
        repo_row <- ri[ri$repo_name == repo, ]
    }

    url_tbl <- get_hf_parquet_urls(repo_row$repo_name, verbose = FALSE)

    if (is.null(data_types)) {
        data_types <- output_file_types()$data_type
    }

    selected <- url_tbl[url_tbl$data_type %in% data_types, ]

    ## Notify of data types not present
    missing_types <- setdiff(data_types, url_tbl$data_type)
    if (length(missing_types) != 0) {
        miss_message <- paste(missing_types, collapse = ", ")
        message("The following data types are not present in the repo ",
                repo_row$repo_name, " and will be skipped:\n",
                miss_message)
    }

    ## Convert URLs to hf:// protocol and build named views vector
    hf_urls <- file_to_hf(selected$url)
    view_names <- selected$filename |>
        gsub(pattern = "\\.parquet", replacement = "") |>
        gsub(pattern = "\\.", replacement = "_")

    names(hf_urls) <- view_names
    curatedCore::parquetRepoSource(hf_urls)
}

# --------------------------------------------------------------------------
# Exported: accessParquetData (public API preserved)
# --------------------------------------------------------------------------

#' @title Set up DuckDB connection with views for available data types
#' @description 'accessParquetData' is a wrapper function that establishes a
#' DuckDB connection and creates views for either all provided local files or
#' all data types available in a repo of interest (see
#' inst/extdata/parquet_repos.csv). When using a remote repo, a vector of
#' specific data types can be supplied as doing this for all data types can
#' take longer.
#'
#' Internally, connection management is delegated to
#' \code{\link[curatedCore]{connectSource}} via the appropriate
#' \code{CuratedSource} constructor.
#'
#' @param dbdir Kept for API compatibility. Ignored — connections are always
#' in-memory. Default: ':memory:'
#' @param repo String (optional): Hugging Face repo where the parquet files are
#' stored. If NULL and local_files is also NULL, the repo listed as the default
#' in get_repo_info() will be selected. Default: NULL
#' @param local_files String or vector of strings (optional): path(s) to parquet
#' file(s). If the elements are named, those names will be used for the created
#' views instead of imputing from file names. Default: NULL
#' @param data_types Character vector (optional): when using a remote repo, a
#' list of data types to establish database views for. If NULL, views will be
#' created for all available data types. Default: NULL
#' @return DuckDB connection object of class 'duckdb_connection'
#' @details Files stored remotely and locally cannot be combined in the same
#' connection.
#' @examples
#' \donttest{
#'  prepared_db <- accessParquetData()
#'  DBI::dbListTables(prepared_db)
#' }
#' \donttest{
#'  single_type <- accessParquetData(data_types = "pathcoverage_unstratified")
#'  DBI::dbListTables(single_type)
#' }
#'
#' fpaths <- c(file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_uuid.parquet"),
#'             file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_pathway.parquet"))
#'
#' local_db <- accessParquetData(local_files = fpaths,
#'                               data_types = "pathcoverage_unstratified")
#' DBI::dbListTables(local_db)
#' @rdname accessParquetData
#' @export
accessParquetData <- function(dbdir = ":memory:",
                                repo = NULL,
                                local_files = NULL,
                                data_types = NULL) {
    ## Check input
    confirm_repo(repo)
    for (dt in data_types) confirm_data_type(dt)

    if (!is.null(local_files) && !is.null(repo)) {
        stop("Please provide a value for either 'repo' or 'local_files'",
             ", but not both.")
    }

    ## Build a CuratedSource and connect via curatedCore
    src <- .build_source(repo = repo, local_files = local_files,
                         data_types = data_types)
    con <- curatedCore::connectSource(src)

    return(con)
}

# --------------------------------------------------------------------------
# Exported: loadParquetData
# --------------------------------------------------------------------------

#' @title Retrieve data from a DuckDB view and convert to Summarized Experiment
#' @description 'loadParquetData' accesses a DuckDB view created by
#' 'accessParquetData' and loads it into R as a Summarized Experiment object.
#' Arguments can be provided to filter or transform the DuckDB view. To filter,
#' provide a named list (format: name = column name, value = single exact
#' value or vector of exact values) to filter_values. To further transform the
#' view, provide a saved sequence of function calls starting with dplyr::tbl to
#' custom_view.
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as part of the name of a view found in
#' DBI::dbListTables(con), indicating which views to consider when collecting
#' data.
#' @param filter_values Named list: element name equals the column name to be
#' filtered and element value equals a vector of exact column values.
#' Default: NULL
#' @param custom_view Saved object with the initial class
#' 'tbl_duckdb_connection' (optional): DuckDB tables/views can be accessed with
#' with the 'dplyr::tbl' function, and piped into additional functions such as
#' 'dplyr::filter' prior to loading into memory with 'dplyr::collect'. A
#' particular sequence of function calls can be saved and provided to this
#' function for collection and formatting as a Summarized Experiment. See the
#' function example. Default: NULL
#' @param include_empty_samples Boolean (optional): should samples provided via
#' a 'uuid' argument within 'filter_values' be included in the final
#' TreeSummarizedExperiment if they do not show up in the results from filtering
#' the source parquet data file. Default: FALSE
#' @param dry_run Boolean (optional): if TRUE, the function will return the
#' tbl_duckdb_connection object prior to calling 'dplyr::collect'. Default:
#' FALSE
#' @return A TreeSummarizedExperiment object with process metadata, row data,
#' column names, and relevant assays. If dry_run = TRUE, a tbl_duckdb_connection
#' object.
#' @details If 'custom_view' is provided, it must use one of the views indicated
#' by data_type'.
#' @examples
#' \donttest{
#'  con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
#'                           data_types = "pathcoverage_unstratified")
#'
#'  custom_filter <- dplyr::tbl(con, "pathcoverage_unstratified_pathway") |>
#'                   dplyr::filter(grepl("UMP biosynthesis", pathway))
#'
#'  uuids <- c("8793b1dc-3ba1-4591-82b8-4297adcfa1d7",
#'             "cc1f30a0-45d9-41b1-b592-7d0892919ee7",
#'             "fb7e8210-002a-4554-b265-873c4003e25f",
#'             "d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
#'             "4985aa08-6138-4146-8ae3-952716575395",
#'             "8eb9f7ae-88c2-44e5-967e-fe7f6090c7af")
#'
#'  custom_tse <- loadParquetData(con,
#'                                data_type = "pathcoverage_unstratified",
#'                                filter_values = list(uuid = uuids),
#'                                custom_view = custom_filter)
#'  custom_tse
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
#' custom_tse <- loadParquetData(con,
#'                               data_type = "pathcoverage_unstratified",
#'                               filter_values = list(uuid = uuids),
#'                               custom_view = custom_filter)
#' custom_tse
#' @seealso
#'  \code{\link[DBI]{dbListTables}}
#'  \code{\link[dplyr]{tbl}}
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[dplyr]{compute}}
#' @rdname loadParquetData
#' @export
#' @importFrom DBI dbListTables
#' @importFrom dplyr filter
loadParquetData <- function(con, data_type, filter_values = NULL,
                            custom_view = NULL, include_empty_samples = FALSE,
                            dry_run = FALSE) {
    ## Check input
    confirm_data_type(data_type)
    if (!is.null(filter_values)) { confirm_filter_values(filter_values) }
    if (!methods::is(include_empty_samples, "logical")) {
        stop("Invalid value of 'include_empty_samples'. Please provide ",
             "TRUE or FALSE.")
    } else if (include_empty_samples && !"uuid" %in% names(filter_values)) {
        message("'include_empty_samples' is TRUE but 'filter_values' ",
                "does not contain a UUID argument.")
    }

    ## Apply any requested filtering, incorporating custom view if provided
    prep <- prepare_view(con, data_type, filter_values, custom_view,
                        include_empty_samples)

    ## Return just view if dry_run = TRUE
    if (dry_run) { return(prep$working_view) }

    ## Collect view (delegate to curatedCore)
    collected_view <- curatedCore::collectView(prep$working_view, notify = TRUE)

    if (nrow(collected_view) == 0) {
        if (!is.null(prep$sample_headers)) {
            message("0 rows returned but empty samples exist. ",
                    "TreeSummarizedExperiment will include colData as applicable.")
        } else {
            message("0 rows returned and any empty samples are not ",
                    "kept. TreeSummarizedExperiment is empty.")
            return(TreeSummarizedExperiment::TreeSummarizedExperiment())
        }
    }

    ## Resolve the general_data_type for schema lookup
    gen_type <- output_file_types(filter_col = "data_type",
                filter_string = data_type)$general_data_type |>
        unique()

    ## Build colData by joining sampleMetadata
    utils::data("sampleMetadata", package = "parkinsonsMetagenomicData",
                envir = environment())
    
    ## Filter to only the requested samples to prevent buildExperiment from padding all 3000+
    target_uuids <- unique(collected_view$uuid)
    if (!is.null(prep$sample_headers)) {
        target_uuids <- unique(c(target_uuids, prep$sample_headers$uuid))
    }
    col_data <- sampleMetadata[sampleMetadata$uuid %in% target_uuids, , drop = FALSE]

    ## Handle empty samples
    if (!is.null(prep$sample_headers)) {
        empty_samples <- dplyr::filter(prep$sample_headers,
                                        !.data$uuid %in% collected_view$uuid)
        ## Merge collected + empty sample headers
        if (nrow(empty_samples) > 0) {
            empty_ids <- empty_samples$uuid
            ## Add empty sample rows to col_data
            missing_in_meta <- setdiff(empty_ids, col_data$uuid)
            if (length(missing_in_meta) > 0) {
                emat <- as.data.frame(matrix(NA, length(missing_in_meta),
                                             ncol(col_data),
                                             dimnames = list(NULL,
                                                     colnames(col_data))))
                emat$uuid <- missing_in_meta
                col_data <- rbind(col_data, emat)
            }
        }
    }

    ## Build the experiment using curatedCore::buildExperiment
    schema <- .get_pmd_schema()
    exp <- curatedCore::buildExperiment(
        table = collected_view,
        schema = schema,
        data_type = gen_type,
        col_data = col_data,
        experiment_class = "TreeSummarizedExperiment"
    )

    return(exp)
}

# --------------------------------------------------------------------------
# Exported: returnSamples
# --------------------------------------------------------------------------

#' @title Return a TreeSummarizedExperiment with data based on sample data and
#' feature data tables
#' @description 'returnSamples' takes tables with sample and feature information
#' and retrieves the relevant data as a TreeSummarizedExperiment.
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
#' @param repo String (optional): Hugging Face repo where the parquet files are
#' stored. If NULL and local_files is also NULL, the repo listed as the default
#' in get_repo_info() will be selected. Default: NULL
#' @param local_files String or vector of strings (optional): path(s) to parquet
#' file(s). If the elements are named, those names will be used for the created
#' views instead of imputing from file names. Default: NULL
#' @param include_empty_samples Boolean (optional): should samples provided via
#' a 'uuid' argument within 'filter_values' be included in the final
#' TreeSummarizedExperiment if they do not show up in the results from filtering
#' the source parquet data file. Default: TRUE
#' @param dry_run Boolean (optional): if TRUE, the function will return the
#' tbl_duckdb_connection object prior to calling 'dplyr::collect'. Default:
#' FALSE
#' @return A TreeSummarizedExperiment object with process metadata, row data,
#' column names, and relevant assays. If dry_run = TRUE, a tbl_duckdb_connection
#' object.
#' @details Files stored remotely and locally cannot be combined in the same
#' connection.
#' @examples
#' \donttest{
#'  if (!exists("sampleMetadata", envir = environment())) {
#'      utils::data("sampleMetadata", package = "parkinsonsMetagenomicData",
#'      envir = environment())
#'  }
#'
#'  table(sampleMetadata$control, useNA = "ifany")
#'  sample_data <- sampleMetadata |>
#'      dplyr::filter(control %in% c("Case", "Study Control") &
#'                     age >= 16 &
#'                     !is.na(sex))
#'  sample_data_small <- sample_data[seq(15),]
#'
#'  clade_name_ref <- load_ref("clade_name_ref")
#'  feature_data_genus <- clade_name_ref |>
#'      dplyr::filter(grepl("Faecalibacterium", clade_name_genus)) |>
#'      dplyr::select(clade_name_genus)
#'
#'  genus_ex <- returnSamples(data_type = "relative_abundance",
#'                            sample_data = sample_data_small,
#'                            feature_data = feature_data_genus)
#'  genus_ex
#' }
#'
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
#' sample_data <- sampleMetadata |>
#'     dplyr::filter(uuid %in% uuids) |>
#'     dplyr::select(where(~ !any(is.na(.x))))
#'
#' fpaths <- c(file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_uuid.parquet"),
#'             file.path(system.file("extdata",
#'                                   package = "parkinsonsMetagenomicData"),
#'                       "pathcoverage_unstratified_pathway.parquet"))
#'
#' refpath <- file.path(system.file("extdata",
#'                                  package = "parkinsonsMetagenomicData"),
#'                      "pathway_ref.parquet")
#'
#' pathway_ref <- load_ref("pathway_ref", file_path = refpath)
#' feature_data_genus <- pathway_ref |>
#'     dplyr::filter(grepl("Faecalibacterium", pathway_genus)) |>
#'     dplyr::select(pathway_uniref) |>
#'     dplyr::rename(pathway = pathway_uniref)
#'
#' genus_ex <- returnSamples(data_type = "pathcoverage_unstratified",
#'                           sample_data = sample_data,
#'                           feature_data = feature_data_genus,
#'                           local_files = fpaths,
#'                           include_empty_samples = FALSE)
#' genus_ex
#' @seealso
#'  \code{\link[curatedCore]{closeSource}}
#' @rdname returnSamples
#' @export
returnSamples <- function(data_type, sample_data = NULL, feature_data = NULL,
                            repo = NULL, local_files = NULL,
                            include_empty_samples = TRUE, dry_run = FALSE) {
    ## Check input
    confirm_repo(repo)
    confirm_data_type(data_type)
    confirm_sample_feature_data(sample_data, feature_data)

    ## Create database connection and load views
    con <- accessParquetData(repo = repo, local_files = local_files,
                            data_types = data_type)

    ## Convert sample_data and feature_data to filter_values
    filter_values <- convert_to_filter_values(con, data_type, sample_data,
                                                feature_data)

    if (length(filter_values) == 0) {
        filter_values <- NULL
    }

    ## Load data
    tse <- loadParquetData(con = con, data_type = data_type,
                            filter_values = filter_values,
                            include_empty_samples = include_empty_samples,
                            dry_run = dry_run)

    ## Close connection (using curatedCore::closeSource)
    curatedCore::closeSource(con)

    return(tse)
}

# --------------------------------------------------------------------------
# Internal: prepare_view (kept, delegates filtering to curatedCore)
# --------------------------------------------------------------------------
#' @title Apply filtering and custom view transformations to a DuckDB view
#' @description 'prepare_view' accesses a DuckDB view created by
#' 'accessParquetData' and applies requested filtering and transformations.
#' Filtering is delegated to \code{\link[curatedCore]{filterView}}.
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param data_type Single string: data type to filter.
#' @param filter_values Named list of filter arguments.
#' @param custom_view Pre-built lazy tbl_duckdb_connection object.
#' @param include_empty_samples Logical: include empty samples in output.
#' @return Named list with 'working_view' and 'sample_headers'.
#' @rdname prepare_view
#' @noRd
#' @importFrom dplyr tbl filter
prepare_view <- function(con, data_type, filter_values, custom_view,
                         include_empty_samples) {
    sample_headers <- NULL
    if (!is.null(filter_values)) {
        if (!is.null(custom_view)) {
            ## Custom view + filter: apply filter_parquet_view-style
            ## filtering directly on the custom view using curatedCore
            working_view <- .filter_lazy_view(custom_view, filter_values)
        } else {
            ## Standard path: use curatedCore::filterView
            working_view <- curatedCore::filterView(con, data_type,
                                                    filter_values)

            if ("uuid" %in% names(filter_values) && include_empty_samples) {
                sample_headers <- get_cdata_only(con, data_type,
                                                 filter_values$uuid)
                full_empties <- setdiff(filter_values$uuid,
                                        sample_headers$uuid)
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
            ## No filters: use curatedCore::filterView with NULL filter_values
            working_view <- curatedCore::filterView(con, data_type)
        }
    }

    wv_list <- list(working_view = working_view,
                    sample_headers = sample_headers)

    return(wv_list)
}

# --------------------------------------------------------------------------
# Internal: filter a pre-existing lazy view (for custom_view + filter_values)
# --------------------------------------------------------------------------
#' Apply filter_values to an existing lazy tbl
#'
#' This replicates the PMD filter_parquet_view logic for cases where the user
#' has already built a custom lazy view and wants to apply additional filters.
#' For the standard code path, curatedCore::filterView is used instead.
#'
#' @param view A lazy tbl_duckdb_connection.
#' @param filter_values Named list of column = values.
#' @return A filtered lazy tbl.
#' @noRd
#' @importFrom dplyr filter union_all collapse
#' @importFrom rlang sym
.filter_lazy_view <- function(view, filter_values) {
    col_order <- names(filter_values)
    first_col <- col_order[1]
    first_vals <- filter_values[[first_col]]

    if (length(first_vals) == 1) {
        result <- dplyr::filter(view, !!rlang::sym(first_col) == first_vals) |>
            dplyr::collapse()
        remaining_cols <- setdiff(col_order, first_col)
    } else if (length(first_vals) <= 10) {
        result <- lapply(first_vals, function(val) {
            dplyr::filter(view, !!rlang::sym(first_col) == val)
        }) |>
            Reduce(dplyr::union_all, x = _)
        remaining_cols <- setdiff(col_order, first_col)
    } else {
        result <- view
        remaining_cols <- col_order
    }

    for (col in remaining_cols) {
        vals <- filter_values[[col]]
        if (length(vals) == 1) {
            result <- dplyr::filter(result, !!rlang::sym(col) == vals)
        } else {
            result <- dplyr::filter(result, !!rlang::sym(col) %in% vals)
        }
    }

    return(result)
}

# --------------------------------------------------------------------------
# Internal: get_cdata_only (kept for empty-sample support)
# --------------------------------------------------------------------------
#' @title Return unique colData columns for a data type
#' @description 'get_cdata_only' takes a data type and vector of UUIDs, filters
#' the relevant parquet file available in a provided database connection, and
#' returns a single row for each uuid containing only the data marked as 'cdata'
#' in the 'se_role' column of 'parquet_colinfo()'.
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param data_type Single string: data type indicator.
#' @param uuids Character vector: UUIDs to return information for.
#' @return A data frame with colData columns.
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
#' uuids <- c("8793b1dc-3ba1-4591-82b8-4297adcfa1d7",
#'            "cc1f30a0-45d9-41b1-b592-7d0892919ee7",
#'            "fb7e8210-002a-4554-b265-873c4003e25f",
#'            "d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
#'            "4985aa08-6138-4146-8ae3-952716575395",
#'            "8eb9f7ae-88c2-44e5-967e-fe7f6090c7af")
#'
#' get_cdata_only(con, data_type = "pathcoverage_unstratified", uuids)
#' @rdname get_cdata_only
#' @noRd
#' @importFrom dplyr select filter distinct collect tbl
#' @importFrom rlang sym
#' @importFrom tidyselect all_of
get_cdata_only <- function(con, data_type, uuids) {
    ## Get column info
    colinfo <- parquet_colinfo(data_type)

    uuid_col <- colinfo$col_name[colinfo$se_role == "cname"]
    cdata_cols <- colinfo$col_name[colinfo$se_role == "cdata"]

    ## Use curatedCore::pickProjection to find best view
    proj <- curatedCore::pickProjection(con, data_type, feature = "uuid")
    edat <- dplyr::tbl(con, proj) |>
        dplyr::select(tidyselect::all_of(c(uuid_col, cdata_cols))) |>
        dplyr::filter(!!rlang::sym(uuid_col) %in% uuids) |>
        dplyr::distinct() |>
        dplyr::collect()

    return(edat)
}

# --------------------------------------------------------------------------
# Exported: get_hf_parquet_urls (kept — PMD-specific HuggingFace logic)
# --------------------------------------------------------------------------

#' @title Get Parquet File URLs and Metadata from a Hugging Face Repository
#' @description This function queries the Hugging Face Hub API to find all
#' Parquet files within a specified dataset repository. It constructs the direct
#' download URLs and then joins this information with a local file containing
#' definitions for BioBakery data types.
#' @param repo_name A character string specifying the Hugging Face dataset
#' repository name in the format "user/repo" or "org/repo". If NULL, the repo
#' listed as the default in get_repo_info() will be selected. Default: NULL
#' @param verbose Boolean: should output be verbose, Default: FALSE
#' @return A data.frame with the following columns:
#'   \describe{
#'     \item{filename}{The name of the Parquet file.}
#'     \item{URL}{The full download URL for the file.}
#'     \item{DataType}{The base name of the file, used for joining with
#'                      metadata.}
#'     \item{Tool}{The bioBakery tool that typically produces the data type.}
#'     \item{Description}{A brief description of the data type.}
#'     \item{Units.Normalization}{The units or normalization method used.}
#'   }
#' @details The metadata is sourced from the "biobakery-file-definitions.csv"
#' file, which is expected to be in the `inst/extdata` directory of the
#' `parkinsonsMetagenomicData` package. If this package is not available,
#' the metadata columns will be populated with `NA`.
#' @examples
#' \donttest{
#'  file_info <- get_hf_parquet_urls()
#'  head(file_info)
#' }
#' @export
#' @importFrom dplyr left_join mutate
#' @importFrom utils read.csv
get_hf_parquet_urls <- function(repo_name = NULL, verbose = FALSE) {
    ## Check input
    confirm_repo(repo_name)

    ## Get repo information
    ri <- get_repo_info()
    if (is.null(repo_name)) {
        repo_row <- ri[ri$default == "Y",]
    } else {
        repo_row <- ri[ri$repo_name == repo_name,]
    }
    repo_name <- repo_row$repo_name

    # --- Step 1: Construct API URL and get repo info ---
    repo_info <- get_hf_api(repo_name)

    # --- Step 2: Filter for Parquet files ---
    parquet_files <- check_for_parquet(repo_info, repo_name)

    if (length(parquet_files) == 0) {
        if (verbose) message("No Parquet files found in the '", repo_name,
                            "' repository.")
        # Return an empty data.frame with the correct structure
        return(data.frame(
            filename = character(0), URL = character(0),
            DataType = character(0),
            Tool = character(0), Description = character(0),
            Units.Normalization = character(0),
            stringsAsFactors = FALSE
        ))
    }

    # --- Step 3: Create the full URLs for each file ---
    base_url <- paste0("https://huggingface.co/datasets/", repo_name,
                        "/resolve/main/")
    parquet_urls <- paste0(base_url, parquet_files)

    if (verbose) message("Found ", length(parquet_urls),
                        " Parquet file(s) in '", repo_name, "'.")

    # --- Step 4: Create initial data.frame ---
    result_df <- data.frame(filename = parquet_files, url = parquet_urls,
                            stringsAsFactors = FALSE)

    # --- Step 5: Read definitions and join with file list ---
    result_df <- add_defs(result_df, verbose)

    return(result_df)
}

# --------------------------------------------------------------------------
# Exported: load_ref (kept — PMD-specific reference file loading)
# --------------------------------------------------------------------------

#' @title Load a single parquet reference file
#' @description 'load_ref' retrieves a single parquet file by name from a
#' designated repo and loads it into a table in R.
#' @param ref String: the name of a reference file as found in get_ref_info()
#' @param repo String (optional): Hugging Face repo where the parquet files are
#' stored. If NULL, the repo listed as the default in get_repo_info() will be
#' selected. Default: NULL
#' @param file_path String (optional): path to locally stored parquet file.
#' Default: NULL
#' @return A table of reference information
#' @examples
#' \donttest{
#'  load_ref("clade_name_ref")
#' }
#'
#' refpath <- file.path(system.file("extdata",
#'                                  package = "parkinsonsMetagenomicData"),
#'                      "pathway_ref.parquet")
#'
#' load_ref("pathway_ref", file_path = refpath)
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{pull}}
#'  \code{\link[arrow]{read_parquet}}
#' @rdname load_ref
#' @export
#' @importFrom dplyr filter pull
#' @importFrom arrow read_parquet
load_ref <- function(ref, repo = NULL, file_path = NULL) {
    ## Check input
    # repo/file_path
    if (!is.null(repo) & !is.null(file_path)) {
        stop("Values for both 'repo' and 'file_path' have been ",
             "provided. Please choose only one.")
    }

    # ref
    confirm_ref(ref)

    # repo
    confirm_repo(repo)

    if (is.null(repo) & is.null(file_path)) {
        ri <- get_repo_info()
        repo <- ri$repo_name[ri$default == "Y"]
    }

    ## retrieve URL
    if (!is.null(repo)) {
        rurl <- get_hf_parquet_urls(repo, verbose = FALSE) |>
            dplyr::filter(.data$data_type == "reference") |>
            dplyr::filter(.data$filename == paste0(ref, ".parquet")) |>
            dplyr::pull(url)
    } else {
        rurl <- file_path
    }

    ## Collect ref file
    ref_tbl <- arrow::read_parquet(rurl)

    return(ref_tbl)
}
