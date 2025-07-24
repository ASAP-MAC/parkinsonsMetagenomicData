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

#' @title Create a database view of a specific data type
#' @description 'view_parquet' creates a database view with the provided DuckDB
#' connection object. The view is created from a parquet file hosted at a
#' repo of interest (see inst/extdata/parquet_repos.csv). The specific
#' file is specified via the file name.
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as the name of a file in the repo of interest,
#' indicating which output file to retrieve.
#' @return NULL
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- db_connect()
#'
#'  view_parquet(con, "pathcoverage_unstratified")
#'  DBI::dbListTables(con)
#'  }
#' }
#' @seealso
#'  \code{\link[DBI]{dbExecute}}
#' @rdname view_parquet
#' @export
#' @importFrom DBI dbExecute
view_parquet <- function(con, httpfs_url = NULL, data_type) {
    ## Check input
    # con
    confirm_duckdb_con(con)

    # data_type
    confirm_data_type(data_type)

    ## Create data_type-specific view
    statement <- paste0("CREATE VIEW IF NOT EXISTS ", data_type,
                        " AS (SELECT * FROM read_parquet('", httpfs_url,
                        data_type, ".parquet'));")
    DBI::dbExecute(con, statement)

    ## Notify of success
    message(paste0("'", data_type, "' data has been saved as a database view named '", data_type, "'."))
}

#' @title Create database views for all available data types
#' @description 'retrieve_views' creates database views for all of the data types
#' available in a repo of interest (see inst/extdata/parquet_repos.csv).
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @return NULL
#' @details 'retrieve_views' uses 'output_file_types' as the initial list of data
#' types to retrieve, and checks if they exist as parquet files in the repo
#' of interest. If they do not, they are simply skipped.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- db_connect()
#'
#'  retrieve_views(con, repo_version = "latest")
#'  DBI::dbListTables(con)
#'  }
#' }
#' @seealso
#'  \code{\link[RCurl]{url.exists}}
#' @rdname retrieve_views
#' @export
#' @importFrom RCurl url.exists
retrieve_views <- function(con, repo_version = "latest", data_types = NULL) {
    ## Check input
    # con
    confirm_duckdb_con(con)

    # data_types
    for (dt in data_types) confirm_data_type(dt)

    ## Get all data types
    if (is.null(data_types)) {
        data_types <- output_file_types()$data_type
    }

    ## Get repo base URLs
    file_url <- get_parquet_url("file", repo_version)
    httpfs_url <- get_parquet_url("httpfs", repo_version)

    ## Create view for each type if it exists
    for (type in data_types) {
        test_url <- paste0(file_url, type, ".parquet")
        exists <- RCurl::url.exists(test_url)
        if (!exists) {
            message(paste0("'", type, "' is not currently available in the repo '", file_url, "', skipping."))
            next
        }
        view_parquet(con, httpfs_url, type)
    }
}

#' @title Convert tabulated parquet file data to a Summarized Experiment
#' @description 'parquet_to_tse' takes tabulated data from a parquet file to a
#' Summarized Experiment object.
#' @param parquet_table Table or data frame: data taken directly from a parquet
#' file found in the repo of interest (see inst/extdata/parquet_repos.csv).
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as the name of a file in the repo of interest.
#' @return A SummarizedExperiment object with process metadata, row data, column
#' names, and relevant assays.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  con <- db_connect()
#'  view_parquet(con, "pathcoverage_unstratified")
#'  p_tbl <- dplyr::tbl(con, "pathcoverage_unstratified") |>
#'           head() |>
#'           dplyr::collect()
#'
#'  se <- parquet_to_tse(p_tbl, "pathcoverage_unstratified")
#'  se
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{c("rowwise", "rowwise")}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[S4Vectors]{DataFrame-class}}, \code{\link[S4Vectors]{S4VectorsOverview}}
#'  \code{\link[SummarizedExperiment]{SummarizedExperiment-class}}, \code{\link[SummarizedExperiment]{SummarizedExperiment}}
#' @rdname parquet_to_tse
#' @export
#' @importFrom dplyr rowwise mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom S4Vectors DataFrame
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
parquet_to_tse <- function(parquet_table, data_type) {
    ## Check input
    # parquet_table
    if (!is.data.frame(parquet_table)) {
        stop("'parquet_table' should be a data.frame.")
    }

    # data_type
    confirm_data_type(data_type)

    ## Get parameters by data type
    colinfo <- parquet_colinfo(data_type)

    colnames(parquet_table) <- colinfo$col_name
    file_col <- colinfo$col_name[colinfo$se_role == "cname"]
    rnames_col <- colinfo$col_name[colinfo$se_role == "rname"]
    rdata_cols <- colinfo$col_name[colinfo$se_role == "rdata"]
    assay_cols <- colinfo$col_name[colinfo$se_role == "assay"]

    ## Convert filename to uuid
    converted_table <- parquet_table %>%
        dplyr::rowwise() %>%
        dplyr::mutate(uuid = unlist(strsplit(get(file_col), "/"))[6]) %>%
        dplyr::select(-all_of(file_col))

    ## Create rowData table
    rdata <- converted_table %>%
        select(any_of(c(rnames_col, rdata_cols))) %>%
        dplyr::distinct() %>% 
      as.data.frame()
    rownames(rdata) <- rdata[[rnames_col]]

    ## Create assay table(s)
    alist <- sapply(assay_cols, function(acol) {
      converted_table %>%
        select(all_of(c(rnames_col, acol, "uuid"))) %>%
        tidyr::pivot_wider(
          names_from  = uuid,
          values_from = all_of(acol),
          values_fill = 0
        ) %>%
        tibble::column_to_rownames({{rnames_col}}) %>%
        as.matrix()
    }, USE.NAMES = TRUE, simplify = FALSE)

    ## Set sample IDs as column name
    cdata <- sampleMetadata %>% 
      filter(uuid %in% unique(converted_table$uuid))
    rownames(cdata) <- cdata$uuid
    
    # make sure rows and columns are in the same order
    featureIDs <- intersect(rownames(rdata), unlist(lapply(alist, rownames)))
    uuids <- unique(converted_table$uuid)
    
    rdata <- rdata[featureIDs,, drop = FALSE]
    cdata <- cdata[uuids,, drop = FALSE]
    alist <- lapply(alist, function(x) x[featureIDs, uuids])
    
    ## Create and return Summarized Experiment object
    ex <- TreeSummarizedExperiment::TreeSummarizedExperiment(assays = alist,
                                                     rowData = DataFrame(rdata),
                                                     colData = DataFrame(cdata)
                                                     )

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
#' @param data_types Character vector (optional): list of data types to
#' estalish database views for. If NULL, views will be created for all available
#' data types. Default: NULL
#' @return DuckDB connection object of class 'duckdb_connection'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  prepared_db <- accessParquetData(dbdir = ":memory:")
#'  DBI::dbListTables(prepared_db)
#'  }
#' }
#' @rdname accessParquetData
#' @export
accessParquetData <- function(dbdir = ":memory:", repo_version = "latest",
                              data_types = NULL) {
    ## Check input
    # data_types
    for (dt in data_types) confirm_data_type(dt)

    ## Connect to database
    con <- db_connect(dbdir)

    ## Create views from Hugging Face repo
    retrieve_views(con, repo_version, data_types)

    ## Return connection
    return(con)
}

#' @title Retrieve data from a DuckDB view and convert to Summarized Experiment
#' @description 'loadParquetData' accesses a view created
#' @param con DuckDB connection object of class 'duckdb_connection'
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as the name of a view found in
#' DBI::dbListTables(con), indicating which view to collect data from.
#' @param uuids Vector of strings (optional): sample UUID(s) to get output for,
#' Default: NULL
#' @param custom_view Saved object with the initial class
#' 'tbl_duckdb_connection' (optional): DuckDB tables/views can be accessed with
#' with the 'dplyr::tbl' function, and piped into additional functions such as
#' 'dplyr::filter' prior to loading into memory with 'dplyr::collect'. A
#' particular sequence of function calls can be saved and provided to this
#' function for collection and formatting as a Summarized Experiment. See the
#' function example. Default: NULL
#' @return A TreeSummarizedExperiment object with process metadata, row data, column
#' names, and relevant assays.
#' @details If 'custom_view' is provided, it must use the view indicated by
#' 'data_type'.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  prepared_db <- accessParquetData(dbdir = ":memory:")
#'  DBI::dbListTables(prepared_db)
#'  custom_filter <- tbl(prepared_db, "pathcoverage_unstratified") |>
#'                 filter(`# Pathway` == "PWY-5686: UMP biosynthesis I")
#'
#'  se <- loadParquetData(con = prepared_db,
#'                        data_type = "pathcoverage_unstratified",
#'                        uuids = c("0807eb2a-a15e-4647-8e19-2600d8fda378",
#'                                  "e0fbb54f-0249-4917-a4d7-bd68acb89c62"),
#'                        custom_view = custom_filter)
#'  }
#' }
#' @seealso
#'  \code{\link[DBI]{dbListTables}}
#'  \code{\link[dplyr]{tbl}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{compute}}
#' @rdname loadParquetData
#' @export
#' @importFrom DBI dbListTables
#' @importFrom dplyr tbl filter collect
loadParquetData <- function(con, data_type, uuids = NULL, custom_view = NULL) {
    ## Check input
    # con
    confirm_duckdb_con(con)

    # data_type
    confirm_data_type(data_type)
    if (!data_type %in% DBI::dbListTables(con)) {
        stop(paste0("'", data_type, "' is not available as a database view."))
    }

    # uuids
    if (!is.null(uuids)) {
        confirm_uuids(uuids)
    }

    # custom_view
    if (!is.null(custom_view)) {
        if (class(custom_view)[1] != "tbl_duckdb_connection") {
            stop("'custom_view' should be of the class 'tbl_duckdb_connection'.")
        }
        if (custom_view$lazy_query$x$x != data_type) {
            stop(paste0("'custom_filter' uses the view '",
                        custom_filter$lazy_query$x$x,
                        "', but 'data_type' equals '", data_type, "'."))
        }
    }

    ## Load requested view into R object
    # Apply custom view if provided
    if (!is.null(custom_view)) {
        working_view <- custom_view
    } else {
        working_view <- dplyr::tbl(con, data_type)
    }
    # Filter for uuids if provided
    if (!is.null(uuids)) {
        patt <- paste(uuids, collapse = "|")
        collected_view <- working_view |>
            dplyr::filter(grepl(patt, filename)) |>
            dplyr::collect()
    } else {
        collected_view <- working_view |>
            dplyr::collect()
    }

    ## Transform into SummarizedExperiment
    exp <- parquet_to_tse(collected_view, data_type)

    return(exp)
}


