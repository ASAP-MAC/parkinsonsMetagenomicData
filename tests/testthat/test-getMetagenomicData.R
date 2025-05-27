## get_bucket_locators
test_that("'uuids' is validated", {
    expect_error(get_bucket_locators("sample-uuid",
                                     data_type = "relative_abundance"),
                 regexp = "One or more values are not valid UUIDs.")
})

test_that("'data_type' is validated", {
    expect_error(get_bucket_locators("56aa2ad5-007d-407c-a644-48aac1e9a8f0",
                                     data_type = c("relative_abundance", "viral_clusters")),
                 regexp = "'data_type' should be a single value.")
    expect_error(get_bucket_locators("56aa2ad5-007d-407c-a644-48aac1e9a8f0",
                                    data_type = "horse"),
                 regexp = "'.*' is not an allowed value for 'data_type'. Please enter a value found in output_file_types\\(\\).")
})

## cache_gcb


## listMetagenomicData


## cacheMetagenomicData


## loadMetagenomicData


## parse_metaphlan_list


## add_metadata


## mergeExperiments
