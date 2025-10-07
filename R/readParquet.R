### Access and format data from parquet files hosted on Hugging Face

#' @title Connect to DuckDB database instance
#' @description 'db_connect' establishes a DuckDB connection in the location
#' specified or in memory, and confirms that the 'httpfs' extension is installed.
#' @param dbdir Location for database files. Should be a path to an existing
#' directory in the file system or the value ':memory:' to keep data in RAM.
#' Default: ':memory:'
#' @return DuckDB connection object of class 'duckdb_connection'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  connection <- db_connect(dbdir = ":memory:")
#'  class(connection)
#'  }
#' }
#' @seealso
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbExecute}}
#'  \code{\link[duckdb]{duckdb}}
#' @rdname db_connect
#' @export
#' @importFrom DBI dbConnect dbExecute
#' @importFrom duckdb duckdb
db_connect <- function(dbdir = ":memory:") {
    ## Establish DuckDB connection and confirm that httpfs is installed
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir)
    DBI::dbExecute(con, "INSTALL httpfs")

    return(con)
}

#' @title Create a database view of a specific parquet file
#' @description 'view_parquet' creates a database view with the provided DuckDB
#' connection object. The view is created from a parquet file hosted at a
#' repo of interest (see inst/extdata/parquet_repos.csv). The specific
#' file is specified via the httpfs-compatible URL. See
#' \href{https://duckdb.org/docs/stable/core_extensions/httpfs/hugging_face.html}{DuckDB Docs}.
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param httpfs_url String: httpfs-compatible URL referencing a specific
#' parquet file hosted in a repo of interest.
#' @param view_name String: name of the database view to be created. If not
#' provided, it will be generated from the name of the file indicated by
#' 'httpfs_url'.
#' @return NULL
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- db_connect()
#'  view_parquet(con,
#'               "hf://datasets/waldronlab/metagenomics_mac/relative_abundance_uuid.parquet",
#'               "relative_abundance_uuid")
#'
#'  DBI::dbListTables(con)
#'  }
#' }
#' @seealso
#'  \code{\link[DBI]{dbExecute}}
#' @rdname view_parquet
#' @export
#' @importFrom DBI dbExecute
view_parquet <- function(con, httpfs_url = NULL, file_path  = NULL,
                         view_name = NULL) {
    ## Check input
    # con
    confirm_duckdb_con(con)

    # httpfs_url/file_path
    if (!is.null(httpfs_url) & !is.null(file_path)) {
        stop("Values for both 'httpfs_url' and 'file_path' have been provided. Please choose only one.")
    } else if (is.null(httpfs_url) & is.null(file_path)) {
        stop("No value was provided for either 'httpfs_url' or 'file_path'. Please provide one.")
    }

    ## Create view_name from URL if not provided
    if (is.null(view_name)) {
        view_name <- httpfs_url |>
            gsub(pattern = "^.*\\/", replacement = "") |>
            gsub(pattern = "\\.parquet", replacement = "") |>
            gsub(pattern = "\\.", replacement = "_")
    }

    ## Create data_type-specific view
    if (!is.null(httpfs_url)) {
        statement <- paste0("CREATE VIEW IF NOT EXISTS ", view_name,
                            " AS (SELECT * FROM read_parquet('", httpfs_url, "'));")
    } else if (!is.null(file_path)) {
        statement <- paste0("CREATE VIEW IF NOT EXISTS ", view_name,
                             " AS (SELECT * FROM '", file_path, "');")
    }
    DBI::dbExecute(con, statement)
}

#' @title Create database views for all available or requested data types
#' @description 'retrieve_views' creates database views for all of the data
#' types available in a repo of interest (see inst/extdata/parquet_repos.csv).
#' an individual type or vector of types may also be requested to avoid unwanted
#' views.
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param repo String (optional): Hugging Face repo where the parquet files are
#' stored. If NULL, the repo listed as the default in get_repo_info() will be
#' selected. Default: NULL
#' @param data_types Character vector (optional): list of data types to
#' establish database views for. If NULL, views will be created for all available
#' data types. Default: NULL
#' @return NULL
#' @details 'retrieve_views' uses 'output_file_types' as the initial list of
#' data types to retrieve, and checks if they exist as parquet files in the repo
#' of interest. If they do not, they are simply skipped and the user is notified.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- db_connect()
#'
#'  retrieve_views(con, repo = "waldronlab/metagenomics_mac",
#'                 data_types = c("relative_abundance",
#'                                "viral_clusters",
#'                                "pathcoverage_unstratified"))
#'  DBI::dbListTables(con)
#'  }
#' }
#' @rdname retrieve_views
#' @export
retrieve_views <- function(con, repo = NULL, data_types = NULL) {
    ## Check input
    # con
    confirm_duckdb_con(con)

    # repo
    confirm_repo(repo)

    # data_types
    for (dt in data_types) confirm_data_type(dt)

    ## Get repo information
    ri <- get_repo_info()

    if (is.null(repo)) {
        repo_row <- ri[ri$default == "Y",]
    } else {
        repo_row <- ri[ri$repo_name == repo,]
    }

    ## Get repo file information
    url_tbl <- suppressMessages(get_hf_parquet_urls(repo_row$repo_name))

    if (is.null(data_types)) {
        data_types <- output_file_types()$data_type
    }

    selected_files <- url_tbl %>%
        filter(data_type %in% data_types)

    ## Notify of data types not present
    missing_types <- setdiff(data_types, url_tbl$data_type)

    if (length(missing_types) != 0) {
        message(paste0("The following data types are not present in the repo ",
                       repo_row$repo_name, " and will be skipped:\n",
                       paste(missing_types, collapse = ", ")))
    }

    ## Convert URLs to httpfs protocol
    hf_urls <- file_to_hf(selected_files$url)

    ## Create view names
    view_names <- selected_files$filename |>
        gsub(pattern = "\\.parquet", replacement = "") |>
        gsub(pattern = "\\.", replacement = "_")

    ## Create view for each file
    for (i in seq_along(hf_urls)) {
        view_parquet(con, httpfs_url = hf_urls[i], view_name = view_names[i])
    }
}

retrieve_local_views <- function(con, local_files, view_names = NULL) {
    ## Check input
    # con
    confirm_duckdb_con(con)

    # local_files/view_names
    if (length(view_names) != 0 & length(view_names) != length(local_files)) {
        stop("The lengths of 'local_files' and 'view_names' do not match.")
    }

    ## Create view names
    if (is.null(view_names)){
        view_names <- local_files |>
            basename() |>
            gsub(pattern = "\\.parquet", replacement = "") |>
            gsub(pattern = "\\.", replacement = "_")
    }

    ## Create view for each file
    for (i in seq_along(local_files)) {
        view_parquet(con, file_path = local_files[i], view_name = view_names[i])
    }
}

#' @title Filter a database view by any number of column:value argument pairs
#' @description 'filter_parquet_view' takes a named list of exact value filter
#' arguments and applies them to a DuckDB database view or table. This function
#' applies the filters in the order provided, ensuring that the most efficient
#' filtering method is used for the first provided condition.
#' @param view DuckDB database view or table: obtained by calling
#' tbl(duckdb_connection, view_name).
#' @param filter_values Named list: element name equals the column name to be
#' filtered and element value equals a vector of exact column values.
#' @return A filtered DuckDB database view or table. This is still lazy until
#' collect() is called.
#' @details Optimization for large files is done by filtering first by the
#' column with the least provided values. This is the most selective filtering
#' step and will therefore reduce the amount of data that is being filtered with
#' subsequent column:value arguments. Because of this, it is ideal to ensure
#' that the provided DuckDB view or table is sorted by the column involved in
#' the first filter condition. If you have multiple views with different sorting
#' schemas, interpret_and_filter() will select the appropriate view and apply
#' the filter for you.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
#'                           data_types = "genefamilies_stratified")
#'  fvalues <- list(uuid = c("d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
#'                           "38d449c8-1462-4d30-ba87-d032d95942ce",
#'                           "5f8d4254-7653-46e3-814e-ed72cdfcb4d0"),
#'                  gene_family_uniref = c("UniRef90_R6K8T6",
#'                                        "UniRef90_B0PDE3"))
#'
#'  filter_parquet_view(view = tbl(con, "genefamilies_stratified_uuid"),
#'                 filter_values = fvalues)
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{setops}}
#'  \code{\link[rlang]{sym}}
#' @rdname filter_parquet_view
#' @export
#' @importFrom dplyr filter union_all collapse
#' @importFrom rlang sym
filter_parquet_view <- function(view, filter_values) {
    ## Check input
    # view
    confirm_duckdb_view(view)

    # filter_values
    confirm_filter_values(filter_values, colnames(view))

    ## Separate first filter condition
    col_order <- names(filter_values)
    first_col <- col_order[1]
    first_vals <- filter_values[[first_col]]

    ## Filter first column using dplyr::union_all or dplyr::collapse
    if (length(first_vals) == 1) {
        result <- dplyr::filter(view, !!rlang::sym(first_col) == first_vals) %>%
            dplyr::collapse()
        remaining_cols <- setdiff(col_order, first_col)
    } else if (length(first_vals) <= 10) {
        result <- lapply(first_vals, function(val) {
            dplyr::filter(view, !!rlang::sym(first_col) == val)
        }) %>%
            Reduce(dplyr::union_all, .)
        remaining_cols <- setdiff(col_order, first_col)
    } else {
        result <- view
        remaining_cols <- col_order
    }

    ## Filter remaining columns (on smaller table)
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

#' @title Select the view with the most appropriate sorting schema and filter
#' @description 'interpret_and_filter' takes a named list of exact value filter
#' arguments and applies them to a DuckDB database view or table. The exact view
#' or table is additionally selected by this function in order to optimize
#' filtering.
#' @param con DuckDB connection object of class 'duckdb_connection'.
#' @param data_type Character string: the main data type to filter.
#' @param filter_values Named list: element name equals the column name to be
#' filtered and element value equals a vector of exact column values.
#' @return A filtered DuckDB database view or table. This is still lazy until
#' collect() is called.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
#'                           data_types = "relative_abundance")
#'  fvalues <- list(clade_name_species = c("s__GGB52130_SGB14966",
#'                                         "s__Streptococcus_mutans"),
#'                  uuid = c("d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
#'                           "38d449c8-1462-4d30-ba87-d032d95942ce",
#'                           "5f8d4254-7653-46e3-814e-ed72cdfcb4d0"))
#'  interpret_and_filter(con, "relative_abundance", fvalues)
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{tbl}}
#' @rdname interpret_and_filter
#' @export
#' @importFrom dplyr tbl
interpret_and_filter <- function(con, data_type, filter_values) {
    ## Check input
    # con
    confirm_duckdb_con(con)

    # data_type
    confirm_data_type(data_type)

    # filter_values
    confirm_filter_values(filter_values)

    ## Order filter columns by selectivity and select projection
    # Retrieve available projections
    projs <- DBI::dbListTables(con) |>
        gsub(pattern = paste0(data_type, "_"),
             replacement = "")

    # Determine primary filter column and values
    sorted_inds <- order(sapply(filter_values, length))
    sorted_args <- filter_values[sorted_inds]

    fcols <- intersect(projs, names(filter_values))
    proj_ind <- which(names(sorted_args) %in% fcols)[1]

    arranged_args <- c(sorted_args[proj_ind], sorted_args[-proj_ind])

    projection <- pick_projection(con, data_type, names(arranged_args)[1])

    ## Load the chosen view
    chosen_view <- dplyr::tbl(con, projection)

    ## Filter parquet by .values
    queried_view <- filter_parquet_view(chosen_view, arranged_args)

    return(queried_view)
}

#' @title Convert tabulated parquet file data to a Summarized Experiment
#' @description 'parquet_to_tse' takes tabulated data from a parquet file to a
#' Summarized Experiment object. Associated sample metadata is automatically
#' attached as colData.
#' @param parquet_table Table or data frame: data taken directly from a parquet
#' file found in the repo of interest (see inst/extdata/parquet_repos.csv).
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as part of the name of a file in the repo of
#' interest.
#' @return A TreeSummarizedExperiment object with process metadata, row data, column
#' names, and relevant assays.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
#'                           data_types = "pathcoverage_unstratified")
#'  parquet_tbl <- tbl(con, "pathcoverage_unstratified_uuid") |> collect()
#'
#'  se <- parquet_to_tse(parquet_tbl, "pathcoverage_unstratified")
#'  se
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{c("rowwise", "rowwise")}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[S4Vectors]{DataFrame-class}}, \code{\link[S4Vectors]{S4VectorsOverview}}
#'  \code{\link[TreeSummarizedExperiment]{TreeSummarizedExperiment-class}}, \code{\link[TreeSummarizedExperiment]{TreeSummarizedExperiment}}
#' @rdname parquet_to_tse
#' @export
#' @importFrom dplyr rowwise mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom S4Vectors DataFrame
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
parquet_to_tse <- function(parquet_table, data_type, empty_data = NULL) {
    ## Check input
    # parquet_table
    if (!is.data.frame(parquet_table)) {
        stop("'parquet_table' should be a data.frame.")
    }

    # data_type
    confirm_data_type(data_type)

    ## Get parameters by data type
    colinfo <- parquet_colinfo(data_type)

    cnames_col <- colinfo$col_name[colinfo$se_role == "cname"]
    cdata_cols <- colinfo$col_name[colinfo$se_role == "cdata"]
    rnames_col <- colinfo$col_name[colinfo$se_role == "rname"]
    rdata_cols <- colinfo$col_name[colinfo$se_role == "rdata"]
    assay_cols <- colinfo$col_name[colinfo$se_role == "assay"]

    ## Account for row ordering issues
    if ("additional_species" %in% colnames(parquet_table)) {
        parquet_table$additional_species <- standardize_ordering(parquet_table$additional_species, ",")
    }

    ## Confirm empty samples
    esamps <- setdiff(empty_data$uuid,
                      unique(dplyr::pull(parquet_table[,cnames_col])))

    ## Create rowData table
    rdata <- parquet_table %>%
        select(any_of(c(rnames_col, rdata_cols))) %>%
        dplyr::distinct() %>%
      as.data.frame()
    rownames(rdata) <- rdata[[rnames_col]]

    ## Create assay table(s)
    alist <- sapply(assay_cols, function(acol) {
      pdata <- parquet_table %>%
        select(all_of(c(rnames_col, acol, "uuid"))) %>%
        tidyr::pivot_wider(
          names_from  = all_of(cnames_col),
          values_from = all_of(acol),
          values_fill = 0
        ) %>%
        tibble::column_to_rownames({{rnames_col}}) %>%
        as.matrix()

      edata <- matrix(NA, nrow(pdata), length(esamps),
                      dimnames = list(NULL, esamps))

      cbind(pdata, edata)
    }, USE.NAMES = TRUE, simplify = FALSE)

    ## Create colData table with sampleMetadata added
    if (!is.null(empty_data)) {
        etab <- empty_data %>%
            filter(uuid %in% esamps) %>%
            select(any_of(c(cnames_col, cdata_cols))) %>%
            as.data.frame()
    }

    cdata <- parquet_table %>%
        select(any_of(c(cnames_col, cdata_cols))) %>%
        dplyr::distinct() %>%
        as.data.frame()

    if (exists("etab")) {
        cdata <- rbind(cdata, etab)
    }

    cdata <- cdata %>%
        dplyr::left_join(sampleMetadata, dplyr::join_by(uuid))
    rownames(cdata) <- cdata[[cnames_col]]

    ## Confirm rows and columns are in the same order
    rowids <- intersect(rownames(rdata), unlist(lapply(alist, rownames)))
    colids <- intersect(rownames(cdata), unlist(lapply(alist, colnames)))

    rdata <- rdata[rowids,, drop = FALSE]
    cdata <- cdata[colids,, drop = FALSE]
    alist <- lapply(alist, function(x) x[rowids, colids, drop = FALSE])

    ## Create and return Summarized Experiment object
    ex <- TreeSummarizedExperiment::TreeSummarizedExperiment(assays = alist,
                                                             rowData = DataFrame(rdata),
                                                             colData = DataFrame(cdata))

    return(ex)
}

#' @title Set up DuckDB connection with views for available data types
#' @description 'accessParquetData' is a wrapper function for 'db_connect' and
#' 'retrieve_views'. A DuckDB connection is established and views are created for
#' all data types available in a repo of interest
#' (see inst/extdata/parquet_repos.csv). A vector of specific data types can be
#' supplied as doing this for all data types can take longer.
#' @param dbdir Location for database files. Should be a path to an existing
#' directory in the file system or the value ':memory:' to keep data in RAM.
#' Default: ':memory:'
#' @param repo String (optional): Hugging Face repo where the parquet files are
#' stored. If NULL, the repo listed as the default in get_repo_info() will be
#' selected. Default: NULL
#' @param data_types Character vector (optional): list of data types to
#' establish database views for. If NULL, views will be created for all available
#' data types. Default: NULL
#' @return DuckDB connection object of class 'duckdb_connection'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  prepared_db <- accessParquetData()
#'  DBI::dbListTables(prepared_db)
#'
#'  single_type <- accessParquetData(data_types = "pathcoverage_unstratified")
#'  DBI::dbListTables(single_type)
#'  }
#' }
#' @rdname accessParquetData
#' @export
accessParquetData <- function(dbdir = ":memory:",
                              repo = NULL,
                              local_files = NULL,
                              data_types = NULL) {
    ## Check input
    # repo
    confirm_repo(repo)

    # data_types
    for (dt in data_types) confirm_data_type(dt)

    ## Connect to database
    con <- db_connect(dbdir)

    ## Create views from Hugging Face repo
    if (is.null(local_files)) {
        retrieve_views(con, repo, data_types)
    } else if (!is.null(local_files)) {
        retrieve_local_views(con, local_files)
    }

    ## Return connection
    return(con)
}

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
#' \dontrun{
#' if(interactive()){
#'  con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
#'                           data_types = "pathcoverage_unstratified")
#'
#'  custom_filter <- tbl(con, "pathcoverage_unstratified_pathway") |>
#'                   filter(grepl("UMP biosynthesis", pathway))
#'
#'  custom_tse <- loadParquetData(con,
#'                                data_type = "pathcoverage_unstratified",
#'                                filter_values = list(uuid = c("8793b1dc-3ba1-4591-82b8-4297adcfa1d7",
#'                                                              "cc1f30a0-45d9-41b1-b592-7d0892919ee7",
#'                                                              "fb7e8210-002a-4554-b265-873c4003e25f",
#'                                                              "d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
#'                                                              "4985aa08-6138-4146-8ae3-952716575395",
#'                                                              "8eb9f7ae-88c2-44e5-967e-fe7f6090c7af")),
#'                                custom_view = custom_filter)
#'  custom_tse
#'  }
#' }
#' @seealso
#'  \code{\link[DBI]{dbListTables}}
#'  \code{\link[dplyr]{tbl}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{compute}}
#' @rdname loadParquetData
#' @export
#' @importFrom DBI dbListTables
#' @importFrom dplyr tbl filter collect
loadParquetData <- function(con, data_type, filter_values = NULL,
                            custom_view = NULL, include_empty_samples = FALSE,
                            dry_run = FALSE) {
    ## Check input
    # con
    confirm_duckdb_con(con)

    # data_type
    confirm_data_type(data_type)

    # filter_values
    if (!is.null(filter_values)) {
        confirm_filter_values(filter_values)
    }

    # custom_view
    if (!is.null(custom_view)) {
        confirm_duckdb_view(custom_view)
    }

    # include_empty_samples
    if (class(include_empty_samples) != "logical") {
        stop("Invalid value of 'include_empty_samples'. Please provide TRUE or FALSE.")
    } else if (include_empty_samples && !"uuid" %in% names(filter_values)) {
        message("'include_empty_samples' is TRUE but 'filter_values' does not contain a UUID argument.")
    }

    ## Apply any requested filtering, incorporating custom view if provided
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
            working_view <- tbl(con, proj)
        }
    }

    ## Return just view if dry_run = TRUE
    if (dry_run) {
        return(working_view)
    }

    ## Collect view
    collected_view <- working_view |>
        collect()

    if (nrow(collected_view) == 0) {
        if (exists("sample_headers")) {
            message("0 rows returned but empty samples exist. TreeSummarizedExperiment will include colData as applicable.")
        } else {
            message("0 rows returned and any empty samples are not kept. TreeSummarizedExperiment is empty.")
            return(TreeSummarizedExperiment::TreeSummarizedExperiment())
        }
    }

    ## Transform into TreeSummarizedExperiment
    if (exists("sample_headers")) {
        empty_samples <- dplyr::filter(sample_headers, !uuid %in% collected_view$uuid)
        exp <- parquet_to_tse(collected_view, data_type, empty_samples)
    } else {
        exp <- parquet_to_tse(collected_view, data_type)
    }

    return(exp)
}

#' @title Return a TreeSummarizedExperiment with data based on sample data and
#' feature data tables
#' @description 'returnSamples' takes tables with sample and feature information
#' and retrieves the relevant data as a TreeSummarizedExperiment.
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as part of the name of a view found in
#' DBI::dbListTables(con), indicating which views to consider when collecting
#' data.
#' @param sample_data Data frame: a table of sample metadata with a 'uuid'
#' column. Often created by accessing 'sampleMetadata' and filtering or
#' otherwise transforming the result to only include samples of interest.
#' @param feature_data Data frame: a table of feature data. Each column will
#' become a filtering argument. Often created by accessing one of the files
#' listed in 'get_ref_info()' with 'load_ref()', then filtering or otherwise
#' transforming the result to only include feature combinations of interest.
#' @param repo String (optional): Hugging Face repo where the parquet files are
#' stored. If NULL, the repo listed as the default in get_repo_info() will be
#' selected. Default: NULL
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
#' @examples
#' \dontrun{
#' if(interactive()){
#'  table(sampleMetadata$control, useNA = "ifany")
#'  sample_data <- sampleMetadata %>%
#'      filter(control %in% c("Case", "Study Control") &
#'             age >= 16 &
#'             is.na(sex) != TRUE)
#'  sample_data_small <- sample_data[1:15,]
#'
#'  clade_name_ref <- load_ref("clade_name_ref")
#'  feature_data_genus <- clade_name_ref %>%
#'      filter(grepl("Faecalibacterium", clade_name_genus)) %>%
#'      select(clade_name_genus)
#'
#'  genus_ex <- returnSamples(data_type = "relative_abundance",
#'                            sample_data = sample_data_small,
#'                            feature_data = feature_data_genus)
#'  }
#' }
#' @seealso
#'  \code{\link[DBI]{dbListTables}}, \code{\link[DBI]{dbDisconnect}}
#' @rdname returnSamples
#' @export
#' @importFrom DBI dbListTables dbDisconnect
returnSamples <- function(data_type, sample_data, feature_data, repo = NULL,
                          include_empty_samples = TRUE, dry_run = FALSE) {
    ## Check input
    # repo
    confirm_repo(repo)

    # data_type
    confirm_data_type(data_type)

    # sample_data
    if (!is.data.frame(sample_data)) {
        stop("'sample_data' should be a data.frame.")
    } else if (!"uuid" %in% colnames(sample_data)) {
        message("'sample_data' does not have a 'uuid' column, all samples will be returned.")
    }

    # feature_data
    if (!is.data.frame(feature_data)) {
        stop("'feature_data' should be a data.frame.")
    }

    ## Create database connection and load views
    con <- accessParquetData(repo = repo, data_types = data_type)

    ## Convert sample_data and feature_data to filter_values
    filter_values <- list()

    if (!is.null(feature_data)) {
        # Retrieve available projections
        projs <- DBI::dbListTables(con) |>
            gsub(pattern = paste0(data_type, "_"),
                 replacement = "")

        # Determine primary filter column and values
        fcols <- colnames(feature_data)

        fsets <- vector(mode = "list", length = length(fcols))
        for (i in 1:length(fcols)) {
            cur_col <- fcols[i]
            names(fsets)[i] <- cur_col
            fsets[i] <- as.vector(unique(feature_data[,cur_col]))
        }

        filter_values <- c(filter_values, fsets)
    }

    if (!is.null(sample_data)) {
        # Add sample uuids
        uuid_arg <- list(uuid = sample_data$uuid)
        filter_values <- c(filter_values, uuid_arg)
    }

    ## Load data
    tse <- loadParquetData(con = con, data_type = data_type,
                           filter_values = filter_values,
                           include_empty_samples = include_empty_samples,
                           dry_run = dry_run)

    ## Close connection
    DBI::dbDisconnect(con)

    return(tse)
}

#' @title Return unique colData columns for a data type
#' @description 'get_cdata_only' takes a data type and vector of UUIDs, filters
#' the relevant parquet file available in a provided database connection, and
#' returns a single row for each uuid containing only the data marked as 'cdata'
#' in the 'se_role' column of 'parquet_colinfo()'.
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as part of the name of a view found in
#' DBI::dbListTables(con), indicating which views to consider when collecting
#' data.
#' @param uuids Character vector: UUIDs to return information for.
#' @return A data frame with a 'uuid' column as well as all columns marked as
#' 'cdata' in the 'se_role' column of 'parquet_colinfo()'.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- accessParquetData(data_types = "relative_abundance")
#'  uuids <- c("c3eb1e35-9a43-413d-8078-6a0a7ac064ba",
#'             "a82385f0-d1be-4d79-854c-a7fbfe4473e1",
#'             "a1444b37-d568-4575-a5c4-14c5eb2a5b89",
#'             "2a497dd7-b974-4f04-9e1f-16430c678f06",
#'             "496a2d0c-75ae-430f-b969-b15dedc16b3c",
#'             "4a786fd8-782f-4d43-937a-36b98e9c0ab6",
#'             "c789b8bc-ebe6-4b85-83e1-5cf1bbfa6111",
#'             "d311d028-a54b-4557-970c-eb5b77ec0050",
#'             "373a5ac4-161a-46c6-b7d8-4f28b854b386",
#'             "5a93179f-7ca7-41d8-96d7-dbed215894aa",
#'             "7a2e961b-2f13-4760-9392-c896c54e7ec3",
#'             "c6ecc460-33db-4032-9092-9148a134f5dc",
#'             "e4a83901-e130-4b64-9e7a-fea55bc5f3f2",
#'             "6a034c9f-f7c9-4ead-812b-123ee99b1e0b",
#'             "ee26b6f0-89fd-45d0-8af9-bc1d9647a700")
#'  get_cdata_only(con, data_type = "relative_abundance", uuids)
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{compute}}
#'  \code{\link[rlang]{sym}}
#' @rdname get_cdata_only
#' @export
#' @importFrom dplyr select filter distinct collect
#' @importFrom rlang sym
get_cdata_only <- function(con, data_type, uuids) {
    ## Get column info
    colinfo <- parquet_colinfo(data_type)

    uuid_col <- colinfo$col_name[colinfo$se_role == "cname"]
    cdata_cols <- colinfo$col_name[colinfo$se_role == "cdata"]

    ## Collect parquet data
    proj <- pick_projection(con, data_type, "uuid")
    edat <- tbl(con, proj) |>
        dplyr::select(all_of(c(uuid_col, cdata_cols))) |>
        dplyr::filter(!!rlang::sym(uuid_col) %in% uuids) |>
        dplyr::distinct() |>
        dplyr::collect()

    return(edat)
}

#' @title Get Parquet File URLs and Metadata from a Hugging Face Repository
#' @description This function queries the Hugging Face Hub API to find all Parquet files
#' within a specified dataset repository. It constructs the direct download URLs
#' and then joins this information with a local file containing definitions
#' for BioBakery data types.
#' @param repo_name A character string specifying the Hugging Face dataset
#' repository name in the format "user/repo" or "org/repo". If NULL, the repo
#' listed as the default in get_repo_info() will be selected. Default: NULL
#' @return A data.frame with the following columns:
#'   \describe{
#'     \item{filename}{The name of the Parquet file.}
#'     \item{URL}{The full download URL for the file.}
#'     \item{DataType}{The base name of the file, used for joining with metadata.}
#'     \item{Tool}{The bioBakery tool that typically produces the data type.}
#'     \item{Description}{A brief description of the data type.}
#'     \item{Units.Normalization}{The units or normalization method used.}
#'   }
#' @details The metadata is sourced from the "biobakery-file-definitions.csv"
#' file, which is expected to be in the `inst/extdata` directory of the
#' `parkinsonsMetagenomicData` package. If this package is not available,
#' the metadata columns will be populated with `NA`.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  file_info <- get_hf_parquet_urls()
#'  head(file_info)
#'  }
#' }
#' @export
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr left_join mutate
#' @importFrom utils read.csv
get_hf_parquet_urls <- function(repo_name = NULL) {
    ## Check input
    # repo
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
    repo_api_url <- paste0("https://huggingface.co/api/datasets/", repo_name)

    # Make the GET request
    response <- httr::GET(repo_api_url)

    # Check the status code before parsing
    if (httr::status_code(response) != 200) {
        stop(
            "Failed to get repo info from Hugging Face API for '", repo_name, "'.\n",
            "Status code: ", httr::status_code(response), ".\n",
            "Please check if the repository name is correct and public. ",
            "The server may also be rate-limiting your IP."
        )
    }

    # Parse the JSON response content
    repo_info <- jsonlite::fromJSON(rawToChar(response$content))

    # --- Step 2: Filter for Parquet files ---
    if (is.null(repo_info$siblings) || is.null(repo_info$siblings$rfilename)) {
        stop("Could not find file listing in the API response for '", repo_name, "'.")
    }

    all_files <- repo_info$siblings$rfilename
    parquet_files <- all_files[endsWith(all_files, ".parquet")]

    if (length(parquet_files) == 0) {
        message("No Parquet files found in the '", repo_name, "' repository.")
        # Return an empty data.frame with the correct structure
        return(data.frame(
            filename = character(0), URL = character(0), DataType = character(0),
            Tool = character(0), Description = character(0),
            Units.Normalization = character(0),
            stringsAsFactors = FALSE
        ))
    }

    # --- Step 3: Create the full URLs for each file ---
    base_url <- paste0("https://huggingface.co/datasets/", repo_name, "/resolve/main/")
    parquet_urls <- paste0(base_url, parquet_files)

    message("Found ", length(parquet_urls), " Parquet file(s) in '", repo_name, "'.")

    # --- Step 4: Create initial data.frame ---
    result_df <- data.frame(
        filename = parquet_files,
        url = parquet_urls,
        stringsAsFactors = FALSE
    )

    # --- Step 5: Read definitions and join with file list ---
    def_path <- system.file(
        "extdata", "biobakery_file_definitions.csv",
        package = "parkinsonsMetagenomicData"
    )

    # Create data_type column for joining
    result_df <- dplyr::mutate(
        result_df,
        data_type = detect_data_type(filename)
    )

    if (nzchar(def_path) && file.exists(def_path)) {
        message("Found definitions file. Joining metadata.")
        definitions <- utils::read.csv(def_path, stringsAsFactors = FALSE)

        # Perform the join
        result_df <- dplyr::left_join(result_df, definitions, by = "data_type")

    } else {
        message(
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

#' @title Load a single parquet reference file
#' @description 'load_ref' retrieves a single parquet file by name from a
#' designated repo and loads it into a table in R.
#' @param ref String: the name of a reference file as found in get_ref_info()
#' @param repo String (optional): Hugging Face repo where the parquet files are
#' stored. If NULL, the repo listed as the default in get_repo_info() will be
#' selected. Default: NULL
#' @return A table of reference information
#' @examples
#' \dontrun{
#' if(interactive()){
#'  load_ref("clade_name_ref")
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{pull}}
#'  \code{\link[arrow]{read_parquet}}
#' @rdname load_ref
#' @export
#' @importFrom dplyr filter pull
#' @importFrom arrow read_parquet
load_ref <- function(ref, repo = NULL) {
    ## Check input
    # ref
    confirm_ref(ref)

    # repo
    confirm_repo(repo)

    if (is.null(repo)) {
        ri <- get_repo_info()
        repo <- ri$repo_name[ri$default == "Y"]
    }

    ## retrieve URL
    rurl <- get_hf_parquet_urls(repo) |>
        suppressMessages() |>
        dplyr::filter(data_type == "reference") |>
        dplyr::filter(filename == paste0(ref, ".parquet")) |>
        dplyr::pull(url)

    ## Collect ref file
    ref_tbl <- arrow::read_parquet(rurl)

    return(ref_tbl)
}
