# https://huggingface.co/datasets/waldronlab/metagenomics_mac

#library(DBI)
#library(duckdb)
#library(dplyr)

#con <-
#    dbConnect(
#        duckdb(
#            dbdir = "curatedMetagenomicData.duckdb"
#        )
#    )

#dbExecute(con, "INSTALL httpfs")

#dbExecute(con, "CREATE VIEW IF NOT EXISTS train AS (SELECT * FROM read_parquet('hf://datasets/waldronlab/metagenomics_mac@~parquet/default/train/*.parquet'));")
#dbExecute(con, "CREATE VIEW IF NOT EXISTS pc_un AS (SELECT * FROM read_parquet('hf://datasets/waldronlab/metagenomics_mac/pathcoverage_unstratified.parquet'));")

#dbListTables(con)

#tbl(con, "train") |>
#    colnames()

#tbl(con, "train") |>
#    head(10L)

#tbl(con, "train") |>
#    head(10L) |>
#    collect()

#tbl(con, "train") |>
#    filter(`# Gene Family` == "UNMAPPED") |>
#    collect()

#tbl(con, "pc_un") |>
#    colnames()

#tbl(con, "pc_un") |>
#    filter(`# Pathway` == "PWY-5686: UMP biosynthesis I") |>
#    head(10L) |>
#    collect()

#for (dt in dbListTables(con)) {
#    tbl(con, dt) |>
#        colnames() |>
#        print()
#}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
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
    #con <- DBI::dbConnect(duckdb::duckdb(dbdir = "curatedMetagenomicData.duckdb"))
    DBI::dbExecute(con, "INSTALL httpfs")

    return(con)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param con PARAM_DESCRIPTION
#' @param data_type PARAM_DESCRIPTION, Default: 'pathcoverage_unstratified'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[DBI]{dbExecute}}
#' @rdname view_parquet
#' @export
#' @importFrom DBI dbExecute
view_parquet <- function(con, data_type = "pathcoverage_unstratified") {
    ## Create data_type-specific view
    statement <- paste0("CREATE VIEW IF NOT EXISTS ", data_type,
                        " AS (SELECT * FROM read_parquet('hf://datasets/waldronlab/metagenomics_mac/",
                        data_type, ".parquet'));")
    DBI::dbExecute(con, statement)

    ## Notify of success
    message(paste0("Data view can be found at '", data_type, "'"))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param con PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[RCurl]{url.exists}}
#' @rdname retrieve_all
#' @export
#' @importFrom RCurl url.exists
retrieve_all <- function(con) {
    ## Get all data types
    data_types <- output_file_types()$data_type

    ## Create view for each type if it exists
    for (type in data_types) {
        test_url <- paste0("https://huggingface.co/datasets/waldronlab/metagenomics_mac/resolve/main/", type, ".parquet")
        exists <- RCurl::url.exists(test_url)
        if (!exists) {
            message(paste0("'", type, "' is not currently available in the dataset waldronlab/metagenomics_mac, skipping."))
            next
        }
        view_parquet(con, type)
    }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param con PARAM_DESCRIPTION
#' @param db_table PARAM_DESCRIPTION
#' @param filter_column PARAM_DESCRIPTION, Default: NULL
#' @param filter_string PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{tbl}}, \code{\link[dplyr]{compute}}, \code{\link[dplyr]{filter}}
#' @rdname load_parquet
#' @export
#' @importFrom dplyr tbl collect filter
load_parquet <- function(con, db_table, filter_column = NULL, filter_string = NULL) {
    if (is.null(filter_column) && is.null(filter_string)) {
        tb <- dplyr::tbl(con, db_table) |>
            dplyr::collect()

        return(tb)
    } else if (!is.null(filter_column) && !is.null(filter_string)) {
        patt <- paste(filter_string, collapse = "|")
        set <- dplyr::tbl(con, db_table) |>
            dplyr::filter(grepl(patt, .data[[filter_column]])) |>
            dplyr::collect()

        return(set)
    } else {
        stop("Please either provide values for both 'filter_column' and 'filter_string' or leave them both as NULL.")
    }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param parquet_table PARAM_DESCRIPTION
#' @param data_type PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{c("rowwise", "rowwise")}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[S4Vectors]{DataFrame-class}}, \code{\link[S4Vectors]{S4VectorsOverview}}
#'  \code{\link[SummarizedExperiment]{SummarizedExperiment-class}}, \code{\link[SummarizedExperiment]{SummarizedExperiment}}
#' @rdname transform_parquet
#' @export
#' @importFrom dplyr rowwise mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment SummarizedExperiment
transform_parquet <- function(parquet_table, data_type) {
    ## Get parameters by data type
    cnames <- colnames(parquet_table)

    htypes <- output_file_types("tool", "humann")$data_type
    if (data_type %in% htypes) {
        rnames_col <- gsub("# ", "", cnames[1]) |> tolower()
        assay_col <- gsub("out_", "", cnames[2]) |> tolower()
        file_col <- cnames[3]
        colnames(parquet_table) <- c(rnames_col, assay_col, file_col)
    }

    ## Expand parquet by filename
    se <- parquet_table %>%
        dplyr::rowwise() %>%
        dplyr::mutate(uuid = unlist(strsplit(get(file_col), "/"))[6]) %>%
        dplyr::select(-all_of(file_col)) %>%
        tidyr::pivot_wider(names_from = uuid, values_from = all_of(assay_col)) %>%
        tibble::column_to_rownames({{rnames_col}})

    ## Separate out row data
    rdata <- S4Vectors::DataFrame(matrix(nrow = nrow(se), ncol = 0))
    rownames(rdata) <- rownames(se)

    ## Set sample ID as column name
    cdata <- S4Vectors::DataFrame(matrix(nrow = ncol(se), ncol = 0))
    rownames(cdata) <- colnames(se)

    ## Set relative abundance as assay
    upcoverage <- as.matrix(se)
    alist <- list(upcoverage)
    names(alist) <- data_type

    ex <- SummarizedExperiment::SummarizedExperiment(assays = alist,
                                                     rowData = rdata,
                                                     colData = cdata)
}


