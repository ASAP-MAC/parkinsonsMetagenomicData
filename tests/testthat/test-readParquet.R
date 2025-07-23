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


## parquet_to_se


## accessParquetData


## loadParquetData
