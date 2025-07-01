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
db_connect <- function() {
    ## Establish DuckDB connection and confirm that httpfs is installed
    con <- DBI::dbConnect(duckdb::duckdb(dbdir = "curatedMetagenomicData.duckdb"))
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
#' @rdname access_parquet
#' @export
#' @importFrom DBI dbExecute
access_parquet <- function(con, data_type = "pathcoverage_unstratified") {
    ## Create data_type-specific view
    statement <- paste0("CREATE VIEW IF NOT EXISTS ", data_type,
                        " AS (SELECT * FROM read_parquet('hf://datasets/waldronlab/metagenomics_mac/",
                        data_type, ".parquet'));")
    DBI::dbExecute(con, statement)

    ## Notify of success
    print(paste0("Data view can be found at '", data_type, "'"))
}


