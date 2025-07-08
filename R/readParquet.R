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

filter_parquet <- function(con, db_table, filter_column, filter_string) {
    ## Build and execute query
    #statement <- paste0("SELECT * FROM ", db_table, " WHERE ", filter_column, " LIKE '%", filter_string, "%';")
    #set <- DBI::dbGetQuery(con, statement)

    #return(set)

    set <- dplyr::tbl(con, db_table) |>
        dplyr::filter(grepl(filter_string, .data[[filter_column]])) |>
        dplyr::collect()

    return(set)
}

load_parquet <- function(con, db_table) {
    tb <- dplyr::tbl(con, db_table) |>
        dplyr::collect()

    return(tb)
}

transform_parquet <- function(parquet_table, data_type) {
    #transformed <- preview %>%
    #    mutate(uuid = unlist(strsplit(filename, "/"))[6])

    #raw <- tbl(con, "pathcoverage_unstratified") |> collect()
    #colnames(raw)[1] <- "pathway"

    ## Get parameters by data type

    ## Expand parquet by filename
    se <- parquet_table %>%
        rowwise() %>%
        mutate(uuid = unlist(strsplit(filename, "/"))[6]) %>%
        select(-filename) %>%
        tidyr::pivot_wider(names_from = uuid, values_from = out_Coverage) %>%
        column_to_rownames("# Pathway")

    ## Separate out row data
    rdata <- S4Vectors::DataFrame(matrix(nrow = nrow(se), ncol = 0))
    rownames(rdata) <- rownames(se)

    ## Set sample ID as column name
    cdata <- S4Vectors::DataFrame(matrix(nrow = ncol(se), ncol = 0))
    rownames(cdata) <- colnames(se)

    ## Set relative abundance as assay
    upcoverage <- as.matrix(se)
    alist <- list(upcoverage)
    names(alist) <- "pathcoverage_unstratified"

    ex <- SummarizedExperiment::SummarizedExperiment(assays = alist,
                                                     rowData = rdata,
                                                     colData = cdata)
}


