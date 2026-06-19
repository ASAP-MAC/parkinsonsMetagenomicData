## Tests for readParquet.R — updated after curatedCore integration
## Removed tests reference old internal functions (db_connect, view_parquet,
## retrieve_views, retrieve_local_views, filter_parquet_view,
## interpret_and_filter, parquet_to_tse). The underlying functionality is
## now covered by curatedCore's own test suite.

## Shared local parquet fixtures
local_pathcoverage_files <- c(
    file.path(system.file("extdata", package = "parkinsonsMetagenomicData"),
              "pathcoverage_unstratified_uuid.parquet"),
    file.path(system.file("extdata", package = "parkinsonsMetagenomicData"),
              "pathcoverage_unstratified_pathway.parquet")
)

## accessParquetData
test_that("accessParquetData returns a valid DuckDB connection", {
    con <- accessParquetData(local_files = local_pathcoverage_files,
                             data_types = "pathcoverage_unstratified")

    expect_s4_class(con, "duckdb_connection")
    curatedCore::closeSource(con)
})

test_that("accessParquetData creates expected views", {
    con <- accessParquetData(local_files = local_pathcoverage_files,
                             data_types = "pathcoverage_unstratified")
    views <- DBI::dbListTables(con)

    expect_true("pathcoverage_unstratified_uuid" %in% views)
    expect_true("pathcoverage_unstratified_pathway" %in% views)
    curatedCore::closeSource(con)
})

test_that("accessParquetData errors if both repo and local_files provided", {
    expect_error(
        accessParquetData(repo = "waldronlab/metagenomics_mac",
                         local_files = local_pathcoverage_files),
        "Please provide a value for either"
    )
})

## loadParquetData
data_type <- "pathcoverage_unstratified"
con <- accessParquetData(local_files = local_pathcoverage_files,
                         data_types = data_type)

# UUIDs from inst/extdata/pathcoverage_unstratified_uuid.parquet fixture
uuids <- c(
    "0a73759e-825f-4276-9348-66fb6a6e2f86",
    "38d449c8-1462-4d30-ba87-d032d95942ce",
    "4985aa08-6138-4146-8ae3-952716575395",
    "5f8d4254-7653-46e3-814e-ed72cdfcb4d0",
    "6821bf5f-ad59-4204-9d78-9cf9cac97329",
    "8793b1dc-3ba1-4591-82b8-4297adcfa1d7"
)
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
    data("sampleMetadata", package = "parkinsonsMetagenomicData",
        envir = environment())
    meta_age <- sampleMetadata |>
        dplyr::filter(uuid %in% uuids) |>
        dplyr::pull(age) |>
        sort()
    cdata_age <- SummarizedExperiment::colData(custom_tse)$age |>
        sort()

    expect_equal(meta_age, cdata_age)
})

curatedCore::closeSource(con)

## returnSamples
# UUIDs from inst/extdata/pathcoverage_unstratified_uuid.parquet fixture
sample_data <- data.frame(uuid = c(
    "0a73759e-825f-4276-9348-66fb6a6e2f86",
    "38d449c8-1462-4d30-ba87-d032d95942ce",
    "4985aa08-6138-4146-8ae3-952716575395",
    "5f8d4254-7653-46e3-814e-ed72cdfcb4d0",
    "6821bf5f-ad59-4204-9d78-9cf9cac97329",
    "8793b1dc-3ba1-4591-82b8-4297adcfa1d7",
    "00000000-0000-0000-0000-000000000001"  # Non-existent UUID for testing
))

pathway_ref <- load_ref(
    "pathway_ref",
    file_path = file.path(
        system.file("extdata", package = "parkinsonsMetagenomicData"),
        "pathway_ref.parquet"
    )
)

feature_data_pathway <- pathway_ref |>
    dplyr::distinct(pathway_uniref) |>
    head(5) |>
    dplyr::rename(pathway = pathway_uniref)

genus_ex <- returnSamples(data_type = "pathcoverage_unstratified",
                          sample_data = sample_data,
                          feature_data = feature_data_pathway,
                          repo = NULL,
                          local_files = local_pathcoverage_files,
                          include_empty_samples = FALSE)
ex_empty <- returnSamples(data_type = "pathcoverage_unstratified",
                          sample_data = sample_data,
                          feature_data = feature_data_pathway,
                          repo = NULL,
                          local_files = local_pathcoverage_files,
                          include_empty_samples = TRUE)
ex_dry <- returnSamples(data_type = "pathcoverage_unstratified",
                        sample_data = sample_data,
                        feature_data = feature_data_pathway,
                        repo = NULL,
                        local_files = local_pathcoverage_files,
                        dry_run = TRUE)

test_that("returnSamples returns TreeSummarizedExperiment", {
    expect_true(inherits(genus_ex, "TreeSummarizedExperiment"))
})

test_that("Expected samples were returned, ignoring empty ones", {
    expect_true(all(colnames(genus_ex) %in% sample_data$uuid))
    expect_false("00000000-0000-0000-0000-000000000001" %in% colnames(genus_ex))
})

test_that("Expected samples were returned, including empty ones", {
    expect_true("00000000-0000-0000-0000-000000000001" %in% colnames(ex_empty))
})

test_that("Expected features were returned", {
    expect_true(all(rownames(genus_ex) %in% feature_data_pathway$pathway))
})

test_that("dry_run returns tbl_duckdb_connection", {
    expect_true(inherits(ex_dry, "tbl_duckdb_connection"))
})

## get_cdata_only
test_that("get_cdata_only retrieves requested samples", {
    con <- accessParquetData(local_files = local_pathcoverage_files,
                             data_types = "pathcoverage_unstratified")
    # UUIDs from inst/extdata/pathcoverage_unstratified_uuid.parquet fixture
    uuids <- c(
        "0a73759e-825f-4276-9348-66fb6a6e2f86",
        "38d449c8-1462-4d30-ba87-d032d95942ce",
        "4985aa08-6138-4146-8ae3-952716575395",
        "5f8d4254-7653-46e3-814e-ed72cdfcb4d0",
        "6821bf5f-ad59-4204-9d78-9cf9cac97329",
        "8793b1dc-3ba1-4591-82b8-4297adcfa1d7",
        "8eb9f7ae-88c2-44e5-967e-fe7f6090c7af",
        "cc1f30a0-45d9-41b1-b592-7d0892919ee7",
        "d9cc81ea-c39e-46a6-a6f9-eb5584b87706",
        "fb7e8210-002a-4554-b265-873c4003e25f"
    )
    cdata <- get_cdata_only(con, data_type = "pathcoverage_unstratified", uuids)

    expect_setequal(uuids, cdata$uuid)
    curatedCore::closeSource(con)
})

## get_hf_parquet_urls
test_that("get_hf_parquet_urls pulls correct columns", {
    testthat::skip_if_offline()
    func_cols <- colnames(get_hf_parquet_urls())
    expect_cols <- c("filename", "url", "data_type", "tool", "description",
                     "units_normalization")

    expect_equal(func_cols, expect_cols)
})

## load_ref
cn_ref <- load_ref(
    "pathway_ref",
    file_path = file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "pathway_ref.parquet")
)

test_that("load_ref returns a table", {
    expect_true(inherits(cn_ref, "tbl"))
})

test_that("load_ref returns expected columns", {
    ex_cols <- parquet_colinfo("pathcoverage_unstratified") |>
        dplyr::filter(ref_file == "pathway_ref") |>
        dplyr::pull(col_name)

    expect_setequal(ex_cols, colnames(cn_ref))
})
