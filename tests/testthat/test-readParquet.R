## db_connect
test_that("database connection established", {
    con <- db_connect(":memory:")
    expect_equal(class(con)[1], "duckdb_connection")

    con <- db_connect("test.db")
    expect_equal(class(con)[1], "duckdb_connection")
    file.remove("test.db")
})

test_that("httpfs installed", {
    con <- db_connect(":memory:")
    exts <- DBI::dbGetQuery(con, "SELECT extension_name, installed, description FROM duckdb_extensions();")
    expect_in("httpfs", exts$extension_name)
    expect_true(exts$installed[exts$extension_name == "httpfs"])
})

## view_parquet


## retrieve_views


## parquet_to_tse - humann
test_that("parquet to TreeSummarizedExperiment (parquet_to_tse())", {
  con <- suppressMessages(accessParquetData())
  test_meta <- dplyr::filter(sampleMetadata, study_name == "MazmanianS_DeCastroFonsecaM_1")
  tse_basic <- loadParquetData(con, uuids = test_meta$uuid, data_type = "pathabundance_unstratified")
  expect_true(
    inherits(tse_basic, "TreeSummarizedExperiment")
  )
})
# feature tables must not have NAs
test_that("TSE assays don't have NAs", {
  con <- suppressMessages(accessParquetData())
  test_meta <- dplyr::filter(sampleMetadata, study_name == "MazmanianS_DeCastroFonsecaM_1")
  tse_basic <- loadParquetData(con, uuids = test_meta$uuid, data_type = "pathabundance_unstratified")
  
  expect_true(
    all(sapply(assays(tse_basic), function(x) all(!is.na(x))))
  )
})
# assays should be matrices
test_that("TSE assays don't have NAs", {
  con <- suppressMessages(accessParquetData())
  test_meta <- dplyr::filter(sampleMetadata, study_name == "MazmanianS_DeCastroFonsecaM_1")
  tse_basic <- loadParquetData(con, uuids = test_meta$uuid, data_type = "pathabundance_unstratified")
  
  expect_true(
    all(sapply(assays(tse_basic), function(x) inherits(x, "matrix")))
  )
})

## accessParquetData


## loadParquetData
