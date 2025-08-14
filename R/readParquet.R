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
#'
#'  view_parquet(con, "hf://datasets/waldronlab/metagenomics_mac/relative_abundance.parquet", "relative_abundance")
#'
#'  DBI::dbListTables(con)
#'  }
#' }
#' @seealso
#'  \code{\link[DBI]{dbExecute}}
#' @rdname view_parquet
#' @export
#' @importFrom DBI dbExecute
view_parquet <- function(con, httpfs_url = NULL, view_name = NULL) {
    ## Check input
    # con
    confirm_duckdb_con(con)

    ## Create view_name from URL if not provided
    if (is.null(view_name)) {
        view_name <- httpfs_url |>
            gsub(pattern = "^.*\\/", replacement = "") |>
            gsub(pattern = "\\.parquet", replacement = "") |>
            gsub(pattern = "\\.", replacement = "_")
    }

    ## Create data_type-specific view
    statement <- paste0("CREATE VIEW IF NOT EXISTS ", view_name,
                        " AS (SELECT * FROM read_parquet('", httpfs_url, "'));")
    DBI::dbExecute(con, statement)
}

#' @title Create database views for all available or requested data types
#' @description 'retrieve_views' creates database views for all of the data
#' types available in a repo of interest (see inst/extdata/parquet_repos.csv).
#' an individual type or vector of types may also be requested to avoid unwanted
#' views.
#' @param con DuckDB connection object of class 'duckdb_connection'
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
    url_tbl <- get_hf_parquet_urls(repo_row$repo_name)

    if (is.null(data_types)) {
        data_types <- output_file_types()$data_type
    }

    #if (is.null(data_types)) {
    #    data_types <- url_tbl$DataType
    #}

    selected_files <- url_tbl %>%
        filter(DataType %in% data_types)

    ## Notify of data types not present
    missing_types <- setdiff(data_types, url_tbl$DataType)

    if (length(missing_types) != 0) {
        message(paste0("The following data types are not present in the repo ",
                       repo_row$repo_name, " and will be skipped:\n",
                       paste(missing_types, collapse = ", ")))
    }

    ## Convert URLs to httpfs protocol
    hf_urls <- file_to_hf(selected_files$URL)

    ## Create view names
    view_names <- selected_files$filename |>
        gsub(pattern = "\\.parquet", replacement = "") |>
        gsub(pattern = "\\.", replacement = "_")

    ## Create view for each file
    for (i in seq_along(hf_urls)) {
        view_parquet(con, hf_urls[i], view_names[i])
    }
}

#' @title Convert tabulated parquet file data to a Summarized Experiment
#' @description 'parquet_to_tse' takes tabulated data from a parquet file to a
#' Summarized Experiment object. Associated sample metadata is automatically
#' attached as colData.
#' @param parquet_table Table or data frame: data taken directly from a parquet
#' file found in the repo of interest (see inst/extdata/parquet_repos.csv).
#' @param data_type Single string: value found in the data_type' column of
#' output_file_types() and also as the name of a file in the repo of interest.
#' @return A TreeSummarizedExperiment object with process metadata, row data, column
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
#'  \code{\link[TreeSummarizedExperiment]{TreeSummarizedExperiment-class}}, \code{\link[TreeSummarizedExperiment]{TreeSummarizedExperiment}}
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
accessParquetData <- function(dbdir = ":memory:", repo = NULL ,
                              data_types = NULL) {
    ## Check input
    # data_types
    for (dt in data_types) confirm_data_type(dt)

    ## Connect to database
    con <- db_connect(dbdir)

    ## Create views from Hugging Face repo
    retrieve_views(con, repo, data_types)

    ## Return connection
    return(con)
}

#' @title Retrieve data from a DuckDB view and convert to Summarized Experiment
#' @description 'loadParquetData' accesses a DuckDB view created by
#' 'accessParquetData' and loads it into R as a Summarized Experiment object.
#' Arguments can be provided to filter the DuckDB view, either by providing a
#' list of UUIDs or a saved sequence of function calls using dplyr::tbl to
#' access the view.
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
loadParquetData <- function(con, data_type, uuids = NULL, feature_name = NULL,
                            feature_value = NULL, custom_view = NULL) {
    ## Check input
    # con
    confirm_duckdb_con(con)

    # data_type
    confirm_data_type(data_type)
    if (!any(grepl(data_type, DBI::dbListTables(con)))) {
        stop(paste0("'", data_type, "' is not available as a database view."))
    }

    # uuids
    if (!is.null(uuids)) {
        confirm_uuids(uuids)
    }

    # feature_name
    list_struct_cols(con, data_type)

    # custom_view
    if (!is.null(custom_view)) {
        if (class(custom_view)[1] != "tbl_duckdb_connection") {
            stop("'custom_view' should be of the class 'tbl_duckdb_connection'.")
        }
        if (custom_view$lazy_query$x$x != data_type) {
            stop(paste0("'custom_view' uses the view '",
                        custom_view$lazy_query$x$x,
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
        dt_info <- output_file_types("data_type", data_type)
        full_paths <- paste0("gs://metagenomics-mac/results/cMDv4/", uuids, "/",
                             dt_info$subdir, dt_info$file_name)
        collected_views <- vector("list", length(full_paths))
        for (i in seq_along(full_paths)) {
            p <- full_paths[i]
            collected_views[[i]] <- working_view |>
                dplyr::filter(filename == p) |>
                dplyr::collect()
        }
        collected_view <- bind_rows(collected_views)
    } else {
        collected_view <- working_view |>
            dplyr::collect()
    }

    ## Transform into TreeSummarizedExperiment
    exp <- parquet_to_tse(collected_view, data_type)

    return(exp)
}


#' Get Parquet File URLs and Metadata from a Hugging Face Repository
#'
#' This function queries the Hugging Face Hub API to find all Parquet files
#' within a specified dataset repository. It constructs the direct download URLs
#' and then joins this information with a local file containing definitions
#' for bioBakery data types.
#'
#' @details The metadata is sourced from the "biobakery-file-definitions.csv"
#'   file, which is expected to be in the `inst/extdata` directory of the
#'   `parkinsonsMetagenomicData` package. If this package is not available,
#'   the metadata columns will be populated with `NA`.
#'
#' @param repo_name A character string specifying the Hugging Face dataset
#'   repository name in the format "user/repo" or "org/repo".
#'   Defaults to "waldronlab/metagenomics_mac".
#' @return A data.frame with the following columns:
#'   \describe{
#'     \item{filename}{The name of the Parquet file.}
#'     \item{URL}{The full download URL for the file.}
#'     \item{DataType}{The base name of the file, used for joining with metadata.}
#'     \item{Tool}{The bioBakery tool that typically produces the data type.}
#'     \item{Description}{A brief description of the data type.}
#'     \item{Units.Normalization}{The units or normalization method used.}
#'   }
#' @export
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr left_join mutate
#' @importFrom utils read.csv
#'
#' @examples
#'   # Get file URLs and metadata from the default repository
#'   file_info <- get_hf_parquet_urls()
#'   head(file_info)
get_hf_parquet_urls <- function(repo_name = "waldronlab/metagenomics_mac") {
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
        URL = parquet_urls,
        stringsAsFactors = FALSE
    )

    # --- Step 5: Read definitions and join with file list ---
    def_path <- system.file(
        "extdata", "biobakery-file-definitions.csv",
        package = "parkinsonsMetagenomicData"
    )

    # Create DataType column for joining
    result_df <- dplyr::mutate(
        result_df,
        DataType = sub("\\..*parquet$", "", filename)
    )

    if (nzchar(def_path) && file.exists(def_path)) {
        message("Found definitions file. Joining metadata.")
        definitions <- utils::read.csv(def_path, stringsAsFactors = FALSE)

        # Perform the join
        result_df <- dplyr::left_join(result_df, definitions, by = "DataType")

    } else {
        message(
            "Data type definition file not found. ",
            "Install 'parkinsonsMetagenomicData' to add full metadata."
        )
        # Add empty columns so the function always returns the same structure
        result_df$Tool <- NA_character_
        result_df$Description <- NA_character_
        result_df$Units.Normalization <- NA_character_
    }

    return(result_df)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param con PARAM_DESCRIPTION
#' @param table PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[DBI]{dbGetQuery}}
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}
#'  \code{\link[stringr]{str_remove}}, \code{\link[stringr]{str_split}}, \code{\link[stringr]{str_extract}}
#'  \code{\link[tidyr]{unnest}}
#' @rdname list_struct_cols
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_remove str_split str_extract
#' @importFrom tidyr unnest
list_struct_cols <- function(con, table) {
    ## Get table schema
    schema <- DBI::dbGetQuery(con, paste0("PRAGMA table_info('", table, "')"))

    ## Extract struct info
    struct_cols <- schema %>%
        dplyr::filter(grepl("^STRUCT", type)) %>%
        dplyr::mutate(type_clean = stringr::str_remove(type, "^STRUCT\\(") %>%
                          stringr::str_remove("\\)$"),
               fields = stringr::str_split(type_clean, ",\\s*")) %>%
        dplyr::select(name, fields) %>%
        tidyr::unnest(fields) %>%
        dplyr::mutate(subfield = stringr::str_extract(fields, "^[^ ]+"),
                      subfield_type = stringr::str_extract(fields, "(?<= )[^ ]+")) %>%
        dplyr::select(struct_column = name, subfield, subfield_type)

    return(struct_cols)
}
