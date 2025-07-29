## db_connect
test_that("database connection established", {
    con <- db_connect(":memory:")
    expect_s4_class(con, "duckdb_connection")

    con <- db_connect("test.db")
    expect_s4_class(con, "duckdb_connection")
})

test_that("httpfs installed", {
    con <- db_connect(":memory:")
    exts <- DBI::dbGetQuery(con, "SELECT extension_name, installed, description FROM duckdb_extensions();")

    expect_true("httpfs" %in% exts$extension_name)
    expect_true(exts$installed[exts$extension_name == "httpfs"])
})

## view_parquet
test_that("view_parquet creates a single view", {
    con <- db_connect(":memory:")
    data_type <- "pathcoverage_unstratified"
    httpfs_url <- get_parquet_url("httpfs", "latest")
    view_parquet(con, httpfs_url, data_type)
    views <- DBI::dbListTables(con)

    expect_true(data_type %in% views)
})

## retrieve_views
test_that("retrieve_views creates expected views", {
    con <- db_connect(":memory:")
    data_types <- c("pathcoverage_unstratified", "pathabundance_unstratified")
    retrieve_views(con, data_types = data_types) |>
        suppressMessages()
    views <- DBI::dbListTables(con)

    expect_true(all(data_types %in% views))
})


## parquet_to_tse - humann
con <- accessParquetData(data_types = "pathabundance_unstratified")
test_meta <- dplyr::filter(sampleMetadata, study_name == "MazmanianS_DeCastroFonsecaM_1")
tse_basic <- loadParquetData(con, uuids = test_meta$uuid, data_type = "pathabundance_unstratified")

test_that("parquet to TreeSummarizedExperiment (parquet_to_tse())", {
    expect_true(inherits(tse_basic, "TreeSummarizedExperiment"))
})

test_that("TSE assays don't have NAs", {
    expect_true(all(sapply(SummarizedExperiment::assays(tse_basic), function(x) all(!is.na(x)))))
})

test_that("TSE assays are matrices", {
    expect_true(all(sapply(SummarizedExperiment::assays(tse_basic), function(x) inherits(x, "matrix"))))
})

## parquet_to_tse - metaphlan

con <- accessParquetData(data_types = "relative_abundance")
test_meta <- dplyr::filter(sampleMetadata, study_name == "MazmanianS_DeCastroFonsecaM_1")
tse_basic <- loadParquetData(con, uuids = test_meta$uuid, data_type = "relative_abundance")

test_that("parquet to TreeSummarizedExperiment with MetaPhlAn data", {
    expect_true(inherits(tse_basic, "TreeSummarizedExperiment"))
})

test_that("TSE assays don't have NAs with MetaPhlAn data", {
    expect_true(all(sapply(SummarizedExperiment::assays(tse_basic), function(x) all(!is.na(x)))))
})

test_that("TSE assays are matrices with MetaPhlAn data", {
    expect_true(all(sapply(SummarizedExperiment::assays(tse_basic), function(x) inherits(x, "matrix"))))
})

## accessParquetData
test_that("accessParquetData returns a valid DuckDB connection", {
    con <- accessParquetData(data_types = "pathcoverage_unstratified")

    expect_s4_class(con, "duckdb_connection")
})

test_that("accessParquetData creates expected views", {
    con <- accessParquetData(data_types = "pathcoverage_unstratified")
    views <- DBI::dbListTables(con)

    expect_true("pathcoverage_unstratified" %in% views)
})

## loadParquetData
test_that("loadParquetData returns valid TreeSummarizedExperiment", {
    data_type <- "pathcoverage_unstratified"
    con <- accessParquetData(data_types = data_type)
    uuids <- sampleMetadata |>
        dplyr::filter(!is.na(uuid)) %>%
        .$uuid |>
        head(2)
    tse <- loadParquetData(con, data_type = data_type, uuids = uuids)

    expect_true(all(uuids %in% colnames(tse)))
})
