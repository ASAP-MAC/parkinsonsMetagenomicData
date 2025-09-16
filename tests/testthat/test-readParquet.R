## db_connect
test_that("database connection established", {
    con <- db_connect(":memory:")
    expect_s4_class(con, "duckdb_connection")

    con <- db_connect("test.db")
    expect_s4_class(con, "duckdb_connection")
    file.remove("test.db")
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
    view_parquet(con,
                 "hf://datasets/waldronlab/metagenomics_mac/relative_abundance_uuid.parquet",
                 "relative_abundance_uuid")
    views <- DBI::dbListTables(con)

    expect_true("relative_abundance_uuid" %in% views)
})

## retrieve_views
test_that("retrieve_views creates expected views", {
    con <- db_connect(":memory:")
    retrieve_views(con, data_types = "relative_abundance") |>
        suppressMessages()
    views <- DBI::dbListTables(con)

    expect_true("relative_abundance_uuid" %in% views)
})

## filter_parquet_view
test_that("filter_parquet_view works with single and multiple filters", {
    con <- db_connect(":memory:")
    DBI::dbExecute(con, "CREATE TABLE test_table (id INTEGER, grp TEXT, color TEXT)")
    DBI::dbExecute(con, "INSERT INTO test_table VALUES (1, 'A', 'red'), (2, 'B', 'blue'), (3, 'A', 'green')")

    view <- dplyr::tbl(con, "test_table")

    res_single <- filter_parquet_view(view, list(grp = "A")) |> collect()
    expect_equal(nrow(res_single), 2)

    res_multi <- filter_parquet_view(view, list(grp = c("A", "B"))) |> collect()
    expect_equal(nrow(res_multi), 3)

    res_multi_col <- filter_parquet_view(view, list(grp = "A", color = "green")) |> collect()
    expect_equal(nrow(res_multi_col), 1)
})

## interpret_and_filter
test_that("interpret_and_filter applies uuid filtering correctly", {
    con <- db_connect(":memory:")
    DBI::dbExecute(con, "CREATE TABLE example (uuid TEXT, val INTEGER)")
    DBI::dbExecute(con, "
        INSERT INTO example VALUES
        ('550e8400-e29b-41d4-a716-446655440000', 10),
        ('550e8400-e29b-41d4-a716-446655440001', 20)
    ")
    DBI::dbExecute(con, "CREATE VIEW relative_abundance_uuid AS SELECT * FROM example")

    res <- interpret_and_filter(
        con,
        "relative_abundance",
        list(uuid = "550e8400-e29b-41d4-a716-446655440000")
    ) |> collect()

    expect_equal(nrow(res), 1)
    expect_equal(res$uuid, "550e8400-e29b-41d4-a716-446655440000")
    expect_equal(res$val, 10)
})

## parquet_to_tse - humann
con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
                         data_types = "pathabundance_unstratified")
parquet_tbl <- tbl(con, "pathabundance_unstratified_uuid") |> collect()
tse_basic <- parquet_to_tse(parquet_tbl, data_type = "pathabundance_unstratified")

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
con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
                         data_types = "relative_abundance")
parquet_tbl <- tbl(con, "relative_abundance_uuid") |> collect()
tse_basic <- parquet_to_tse(parquet_tbl, data_type = "relative_abundance")

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

    expect_true(all(grepl("pathcoverage_unstratified", views)))
})

## loadParquetData
data_type <- "pathcoverage_unstratified"
con <- accessParquetData(repo = "waldronlab/metagenomics_mac_examples",
                         data_types = data_type)

uuids <- c("8793b1dc-3ba1-4591-82b8-4297adcfa1d7",
           "cc1f30a0-45d9-41b1-b592-7d0892919ee7",
           "fb7e8210-002a-4554-b265-873c4003e25f",
           "d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
           "4985aa08-6138-4146-8ae3-952716575395",
           "8eb9f7ae-88c2-44e5-967e-fe7f6090c7af")
custom_filter <- tbl(con, "pathcoverage_unstratified_pathway") |>
               filter(grepl("UMP biosynthesis", pathway))

custom_tse <- loadParquetData(con,
                              data_type = data_type,
                              filter_values = list(uuid = uuids),
                              custom_view = custom_filter)

test_that("loadParquetData returns TreeSummarizedExperiment", {
    expect_true(inherits(custom_tse, "TreeSummarizedExperiment"))
})

test_that("custom_view was applied", {
    expect_true(all(grepl("UMP biosynthesis", rownames(custom_tse))))
})

test_that("filter_values were applied", {
    expect_true(all(colnames(custom_tse) %in% uuids))
})

test_that("sampleMetadata was added", {
    meta_age <- sampleMetadata |>
        dplyr::filter(uuid %in% uuids) |>
        dplyr::pull(age) |>
        sort()
    cdata_age <- colData(custom_tse)$age |>
        sort()

    expect_equal(meta_age, cdata_age)
})


## get_hf_parquet_urls
test_that("get_hf_parquet_urls pulls correct columns", {
    func_cols <- colnames(get_hf_parquet_urls())
    expect_cols <- c("filename", "url", "data_type", "tool", "description",
                     "units_normalization")

    expect_equal(func_cols, expect_cols)
})
