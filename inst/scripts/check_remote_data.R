## A script to verify that the same data is accessible from both Google Cloud
## Storage and Hugging Face

data("sampleMetadata", package = "parkinsonsMetagenomicData",
     envir = environment())

test_meta <- filter(sampleMetadata,
                    study_name == "MazmanianS_DeCastroFonsecaM_1")
data_type = "relative_abundance"

huggingFace_mpa.tse <- returnSamples(data_type,
                                     sample_data = test_meta,
                                     feature_data = NULL,
                                     include_empty_samples = TRUE)

cache <- suppressMessages(cacheMetagenomicData(uuids = test_meta$uuid, data_type = data_type))
gcBucket_mpa.tse <- suppressMessages(loadMetagenomicData(cache_table = cache))

# hugging face rownames are all in google cloud bucket
stopifnot(
    length(setdiff(rownames(huggingFace_mpa.tse),
                   rownames(gcBucket_mpa.tse))) == 0
)

# google cloud bucket rownames all in hugging face rownames
stopifnot(
    length(setdiff(rownames(gcBucket_mpa.tse),
                   rownames(huggingFace_mpa.tse))) == 0
)
