test_meta <- filter(sampleMetadata, study_name == "MazmanianS_DeCastroFonsecaM_1")
data_type = "relative_abundance"

huggingFace_mpa.tse <- returnSamples(data_type,
                                     sample_data = test_meta,
                                     feature_data = NULL,
                                     include_empty_samples = TRUE)

cache <- suppressMessages(cacheMetagenomicData(uuids = test_meta$uuid, data_type = data_type))
gcBucket_mpa.tse <- suppressMessages(loadMetagenomicData(cache_table = cache))

test_that("hugging face rownames are all in google cloud bucket", {
    expect_equal(
        setdiff(rownames(huggingFace_mpa.tse), rownames(gcBucket_mpa.tse)), expected = character(0)
    )
})

test_that("google cloud bucket rownames all in hugging face rownames", {
    expect_equal(
        setdiff(rownames(gcBucket_mpa.tse), rownames(huggingFace_mpa.tse)), expected = character(0)
    )
})
