## pMD_get_cache


## output_file_types


## get_exts


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
