## pMD_get_cache
test_that("pMD_get_cache creates and returns a BiocFileCache", {
    cache <- pMD_get_cache()
    expect_s4_class(cache, "BiocFileCache")
    expect_true(dir.exists(cache@cache))
})

## output_file_types
test_that("correct file is read", {
    fpath <- system.file("extdata", "output_files.csv",
                         package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE) |>
        as.data.frame()

    test_table <- output_file_types()

    expect_equal(test_table, ftable)
})

test_that("simple filter works", {
    ftable <- output_file_types() |>
        dplyr::filter(grepl("rel", data_type))

    test_table <- output_file_types(filter_col = "data_type",
                                    filter_string = "rel")

    expect_equal(test_table, ftable)
})

## biobakery_files
test_that("biobakery_files reads extdata/biobakery_file_definitions.csv", {
    fpath <- system.file("extdata", "biobakery_file_definitions.csv",
                         package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE)

    test_table <- biobakery_files()
    expect_equal(test_table, ftable)
})

## parquet_colinfo
test_that("parquet_colinfo returns expected column info for known data_type", {
    colinfo <- parquet_colinfo("relative_abundance")
    expect_true(is.data.frame(colinfo))
    expect_true(colinfo[colinfo$se_role == "assay", "col_name"] == "relative_abundance")
})

test_that("parquet_colinfo errors on invalid data_type", {
    expect_error(parquet_colinfo("horse"))
})

## detect_data_type
test_that("detect_data_type finds longest matching type", {
    res <- detect_data_type("genefamilies_cpm_but_sorted")
    expect_equal(res, "genefamilies_cpm")
})

test_that("detect_data_type returns NA for no matches", {
    res <- detect_data_type("not_a_type")
    expect_true(is.na(res))
})

## pick_projection
test_that("pick_projection selects exact matching view", {
    con <- db_connect(":memory:")
    DBI::dbExecute(con, "CREATE TABLE relative_abundance_uuid (id INTEGER)")
    tbls <- DBI::dbListTables(con)
    expect_true("relative_abundance_uuid" %in% tbls)

    res <- pick_projection(con, "relative_abundance", "uuid")
    expect_equal(res, "relative_abundance_uuid")
})

test_that("pick_projection falls back to uuid view when exact not found", {
    con <- db_connect(":memory:")
    DBI::dbExecute(con, "CREATE TABLE relative_abundance_uuid (id INTEGER)")
    res <- pick_projection(con, "relative_abundance", "nonexistent_feature")
    expect_equal(res, "relative_abundance_uuid")
})

test_that("pick_projection selects view that matches data_type if no uuid or exact views", {
    con <- db_connect(":memory:")
    DBI::dbExecute(con, "CREATE TABLE relative_abundance_horse (id INTEGER)")
    res <- pick_projection(con, "relative_abundance")
    expect_equal(res, "relative_abundance_horse")
})

test_that("pick_projection errors if no matching views", {
    con <- db_connect(":memory:")
    expect_error(pick_projection(con, "relative_abundance"))
})

## get_repo_info
test_that("get_repo_info reads extdata/parquet_repos.csv", {
    fpath <- system.file("extdata", "parquet_repos.csv",
                         package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE) |> as.data.frame()
    test_table <- get_repo_info()
    expect_equal(test_table, ftable)
})

## get_ref_info
test_that("get_ref_info reads extdata/ref_file_definitions.csv", {
    fpath <- system.file("extdata", "ref_file_definitions.csv",
                         package="parkinsonsMetagenomicData")
    ftable <- readr::read_csv(fpath, show_col_types = FALSE) |> as.data.frame()
    test_table <- get_ref_info()
    expect_equal(test_table, ftable)
})

## file_to_hf
test_that("file_to_hf converts huggingface URL correctly", {
    url <- "https://huggingface.co/datasets/waldronlab/metagenomics_mac/resolve/main/file.parquet"
    hf_url <- file_to_hf(url)
    expect_equal(hf_url, "hf://datasets/waldronlab/metagenomics_mac/file.parquet")
})

## get_exts
test_that("get_exts returns simple and compound extensions", {
    expect_equal(get_exts("path/file.tsv.gz"), ".tsv.gz")
    expect_equal(get_exts("file.csv"), ".csv")
})

## confirm_uuids
test_that("valid uuids succeed", {
    expect_no_error(confirm_uuids("56aa2ad5-007d-407c-a644-48aac1e9a8f0"))
    expect_no_error(confirm_uuids("56AA2AD5-007D-407C-A644-48AAC1E9A8F0"))
    expect_no_error(confirm_uuids(c("56aa2ad5-007d-407c-a644-48aac1e9a8f0",
                                    "ffe9146b-cf91-442a-9dac-438ea4006f0d")))
})

test_that("invalid uuids throw error", {
    expect_error(confirm_uuids("horse"),
                 regexp = "One or more values are not valid UUIDs.")
    expect_error(confirm_uuids(c("56aa2ad5-007d-407c-a644-48aac1e9a8f0",
                                 "horse")),
                 regexp = "One or more values are not valid UUIDs.")
})

## confirm_data_type
test_that("single valid data_type succeeds", {
    expect_no_error(confirm_data_type("relative_abundance"))
})

test_that("multiple or invalid data_type values throw error", {
    expect_error(confirm_data_type(c("relative_abundance", "viral_clusters")))
    expect_error(confirm_data_type("horse"))
})

test_that("filtering works", {
    expect_no_error(confirm_data_type("pathcoverage_stratified", "tool", "humann"))
    expect_error(confirm_data_type("relative_abundance", "tool", "humann"))
    expect_error(confirm_data_type("pathcoverage_stratified", "horse", "humann"))
})

## confirm_filter_values
test_that("confirm_filter_values accepts valid named list", {
    l <- list(uuid = "56aa2ad5-007d-407c-a644-48aac1e9a8f0", animal = "frog")
    expect_no_error(confirm_filter_values(l, available_features = c("uuid","animal")))
})

test_that("confirm_filter_values rejects invalid inputs", {
    expect_error(confirm_filter_values("notalist"))
    expect_error(confirm_filter_values(list(uuid = "not_a_uuid")))
    expect_error(confirm_filter_values(list(badname = "val"), available_features = "goodname"))
})

## confirm_duckdb_con
test_that("confirm_duckdb_con accepts valid duckdb_connection", {
    con <- db_connect(":memory:")
    expect_no_error(confirm_duckdb_con(con))
})

test_that("confirm_duckdb_con errors on invalid input", {
    expect_error(confirm_duckdb_con("not_a_connection"))
    expect_error(confirm_duckdb_con(NULL))
})

## confirm_duckdb_view
test_that("confirm_duckdb_view accepts tbl_duckdb_connection", {
    con <- db_connect(":memory:")
    DBI::dbExecute(con, "CREATE TABLE test (id INTEGER)")
    view <- dplyr::tbl(con, "test")
    expect_no_error(confirm_duckdb_view(view))
})

test_that("confirm_duckdb_view errors on wrong class", {
    expect_error(confirm_duckdb_view("not_a_view"))
})

## confirm_repo
test_that("confirm_repo accepts NULL or valid repo", {
    ri <- get_repo_info()
    expect_no_error(confirm_repo(NULL))
    expect_no_error(confirm_repo(ri$repo_name[1]))
})

test_that("confirm_repo errors on invalid repo", {
    expect_error(confirm_repo("horse"))
})

## confirm_ref
test_that("confirm_ref accepts valid ref", {
    expect_no_error(confirm_ref("clade_name_ref"))
})

test_that("confirm_ref errors on invalid ref", {
    expect_error(confirm_ref("horse"))
})

## standardize_ordering
test_that("standardize_ordering correctly orders strings", {
    vec <- c("horse|gecko|frog",
             "cow|camel|fish",
             "frog|gecko|horse")

    expect_vec <- c("frog|gecko|horse",
                    "camel|cow|fish",
                    "frog|gecko|horse")

    new_vec <- standardize_ordering(vec, delim = "|")

    expect_equal(new_vec, expect_vec)
})
