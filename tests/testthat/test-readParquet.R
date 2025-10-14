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
                 httpfs_url = "hf://datasets/waldronlab/metagenomics_mac/relative_abundance_uuid.parquet",
                 view_name = "relative_abundance_uuid")
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

## retrieve_local_views
test_that("retrieve_local_views creates expected views", {
    con <- db_connect(dbdir = ":memory:")
    fpath <- file.path(system.file("extdata",
                                   package = "parkinsonsMetagenomicData"),
                       "sample_table.parquet")
    retrieve_local_views(con, fpath)
    views <- DBI::dbListTables(con)

    expect_true("sample_table" %in% views)
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
    cdata_age <- SummarizedExperiment::colData(custom_tse)$age |>
        sort()

    expect_equal(meta_age, cdata_age)
})

## returnSamples
human_samples <- sampleMetadata |>
    dplyr::filter(control %in% c("Case", "Study Control") &
                  age >= 16 &
                  is.na(sex) != TRUE) |>
    dplyr::filter(host_species == "Homo sapiens") |>
    head(10)

mouse_samples <- sampleMetadata |>
    dplyr::filter(control %in% c("Case", "Study Control") &
                  age >= 16 &
                  is.na(sex) != TRUE) |>
    dplyr::filter(host_species == "Mus musculus") |>
    head(10)

sample_data <- rbind(human_samples, mouse_samples)

clade_name_ref <- load_ref("clade_name_ref")
feature_data_genus <- clade_name_ref |>
    dplyr::filter(grepl("Faecalibacterium", clade_name_genus))
genus_ex <- returnSamples(data_type = "relative_abundance",
                          sample_data = sample_data,
                          feature_data = feature_data_genus,
                          include_empty_samples = FALSE)
ex_empty <- returnSamples(data_type = "relative_abundance",
                          sample_data = sample_data,
                          feature_data = feature_data_genus,
                          include_empty_samples = TRUE)
ex_dry <- returnSamples(data_type = "relative_abundance",
                        sample_data = sample_data,
                        feature_data = feature_data_genus,
                        dry_run = TRUE)

test_that("returnSamples returns TreeSummarizedExperiment", {
    expect_true(inherits(genus_ex, "TreeSummarizedExperiment"))
})

test_that("Expected samples were returned, ignoring empty ones", {
    expect_in(colnames(genus_ex), sample_data$uuid)
    expect_false(all(sample_data$uuid %in% colnames(genus_ex)))
})

test_that("Expected samples were returned, including empty ones", {
    expect_setequal(colnames(ex_empty), sample_data$uuid)
})

test_that("Expected features were returned", {
    expect_in(rownames(genus_ex), feature_data_genus$clade_name)
})

test_that("dry_run returns tbl_duckdb_connection", {
    expect_true(inherits(ex_dry, "tbl_duckdb_connection"))
})

#dt_info <- output_file_types()
#random_data_type <- dt_info[sample(nrow(dt_info), 1),]
#ri_info <- get_ref_info(filter_col = "general_data_type", filter_string = random_data_type$general_data_type)
#relevant_ref <- load_ref(ri_info$ref_file)
#random_sample <- sampleMetadata[sample(nrow(sampleMetadata), 1),]
#random_feature <- relevant_ref[sample(nrow(relevant_ref), 1),]

#test_that("random data_type returns data for single feature across all samples", {
#    expect_no_error({
#        returnSamples(data_type = random_data_type$data_type,
#                      sample_data = NULL,
#                      feature_data = random_feature,
#                      include_empty_samples = FALSE)
#    })
#})

#test_that("random data_type returns data for single sample across all features", {
#    expect_no_error({
#        returnSamples(data_type = random_data_type$data_type,
#                      sample_data = random_sample,
#                      feature_data = NULL,
#                      include_empty_samples = FALSE)
#    })
#})

## get_cdata_only
test_that("get_cdata_only retrieves requested samples", {
    con <- accessParquetData(data_types = "relative_abundance")
    uuids <- c("c3eb1e35-9a43-413d-8078-6a0a7ac064ba",
               "a82385f0-d1be-4d79-854c-a7fbfe4473e1",
               "a1444b37-d568-4575-a5c4-14c5eb2a5b89",
               "2a497dd7-b974-4f04-9e1f-16430c678f06",
               "496a2d0c-75ae-430f-b969-b15dedc16b3c",
               "4a786fd8-782f-4d43-937a-36b98e9c0ab6",
               "c789b8bc-ebe6-4b85-83e1-5cf1bbfa6111",
               "d311d028-a54b-4557-970c-eb5b77ec0050",
               "373a5ac4-161a-46c6-b7d8-4f28b854b386",
               "5a93179f-7ca7-41d8-96d7-dbed215894aa",
               "7a2e961b-2f13-4760-9392-c896c54e7ec3",
               "c6ecc460-33db-4032-9092-9148a134f5dc",
               "e4a83901-e130-4b64-9e7a-fea55bc5f3f2",
               "6a034c9f-f7c9-4ead-812b-123ee99b1e0b",
               "ee26b6f0-89fd-45d0-8af9-bc1d9647a700")
    cdata <- get_cdata_only(con, data_type = "relative_abundance", uuids)

    expect_setequal(uuids, cdata$uuid)
})

## get_hf_parquet_urls
test_that("get_hf_parquet_urls pulls correct columns", {
    func_cols <- colnames(get_hf_parquet_urls())
    expect_cols <- c("filename", "url", "data_type", "tool", "description",
                     "units_normalization")

    expect_equal(func_cols, expect_cols)
})

## load_ref
cn_ref <- load_ref("clade_name_ref")

test_that("load_ref returns a table", {
    expect_true(inherits(cn_ref, "tbl"))
})

test_that("load_ref returns expected columns", {
    ex_cols <- parquet_colinfo("relative_abundance") |>
        dplyr::filter(ref_file == "clade_name_ref") |>
        dplyr::pull(col_name)

    expect_setequal(ex_cols, colnames(cn_ref))
})
