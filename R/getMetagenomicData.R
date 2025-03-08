
# set up file caching
# set up google cloud project/bucket access: gs://metagenomics-mac

get_locator <- function(uuid, data_type) {
    # check that data_type %in% c("bugs", "viruses", "unknown")
    # add ifelse for multiple selection
    # allow vector input?

    locator <- paste0("gs://metagenomics-mac/results/cMDv4/",
                      uuid,
                      "/metaphlan_lists/metaphlan_",
                      data_type,
                      "_list.tsv.gz")
    return(locator)
}

# find package or use API to cp locator to cache location
