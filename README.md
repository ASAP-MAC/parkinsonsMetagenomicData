parkinsonsMetagenomicData
================

## Sample Metadata

Metadata for all samples present within `parkinsonsMetagenomicData` is
available through the included `data.frame` `sampleMetadata`. Both
curated and uncurated features are included in this `data.frame`, with
uncurated features being prefixed by “uncurated\_”. Curated features
include the following at this time:

- curation_id
- study_name
- sample_id
- subject_id
- target_condition
- target_condition_ontology_term_id
- control
- control_ontology_term_id
- age
- age_group
- age_group_ontology_term_id
- age_unit
- age_unit_ontology_term_id
- sex
- sex_ontology_term_id
- disease
- disease_ontology_term_id
- curator
- BioProject
- BioSample
- NCBI_accession
- uuid

``` r
selected_samples <- sampleMetadata |>
    filter(study_name == "ZhangM_2023") |>
    select(where(~ !any(is.na(.x))))
```

## Output files

The UUID(s) of any sample(s) of interest may be then used to access
output files. Available data types and information about their full file
paths can be found with `output_file_types()`

``` r
ftypes <- output_file_types()
```

At the moment, only the `metaphlan_lists` data types, `viral_clusters`
and `relative_abundance`, as well as the `humann` data types, have
parsing functions for automatically loading into SummarizedExperiment
objects.

## Google Bucket Setup

Output files are contained in the private Google Cloud Bucket
`gs://metagenomics-mac` at the moment, the user must authenticate with a
service account keyfile in the following way:

``` r
googleCloudStorageR::gcs_auth("full/path/to/keyfile.json")
```

To obtain this keyfile, the owner of a service account affiliated with
the Google Cloud Project containing `gs://metagenomics-mac` must create
it according to the process detailed in the Google Cloud Guide “[Create
and delete service account
keys](https://cloud.google.com/iam/docs/keys-create-delete)”.

Additionally, the default bucket for `GoogleCloudStorageR` must be set
to the Google Bucket name, in this case `metagenomics-mac`. This is done
automatically upon loading the package, but if an error occurs can be
manually done through the following:

``` r
googleCloudStorageR::gcs_global_bucket("metagenomics-mac")
```

## Data Retrieval

All objects stored in `gs://metagenomics-mac` can then be viewed with
the following. This operation can take several minutes due to the number
of objects stored in `gs://metagenomics-mac`.

``` r
googleCloudStorageR::gcs_list_objects()
```

Alternatively, `listMetagenomicData` will provide a table of objects
that are compatible with the `cacheMetagenomicData` streamlined
downloading and caching function.

``` r
file_tbl <- listMetagenomicData()
```

To access output files, use `cacheMetagenomicData`. This function takes
UUID and data type arguments, downloads the corresponding output files,
and stores them in a local cache. If the same files are requested again
through this function, they will not be re-downloaded unless explicitly
specified, in order to reduce excessive downloads.
`cacheMetagenomicData` returns a tibble of cached file paths and cache
IDs for each requested file.

``` r
cache_tbl <- cacheMetagenomicData(uuids = selected_samples$uuid,
                                  data_type = "relative_abundance")
#| Resource with rname = 'results/cMDv4/0807eb2a-a15e-4647-8e19-2600d8fda378/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/e0fbb54f-0249-4917-a4d7-bd68acb89c62/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/25172837-2849-4db3-be91-d54d6a815d00/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/39ddb5e7-97f6-4d3c-812b-9653b03f99b3/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/7b152a7d-e244-4e2b-b924-7195c7ecfb10/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/dd30f93b-7999-47a4-93fb-21971b899939/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/1406666f-04a8-43c9-983b-4ed62fd6da4a/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/fe3de3ca-3a14-4bd8-ae1c-0dad69edc9cd/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/8707e374-5ddb-4220-8cbf-364b8b0e7be1/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/22848a9c-66a6-4993-9058-cb6464edb42f/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/08e2b754-78e2-4cb4-8ff2-95fd7b0ff44a/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/9baef0b2-93d2-4a40-8082-d357c7f8156a/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/09a9303d-d87d-4556-9672-04cbbcaf3d37/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/ac9f3532-90d8-412c-9c80-491037f0bcc2/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/eda61949-02dc-40ae-8dbe-bea2add85a52/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/1f007260-be6c-4a21-800a-ad9c36129a0d/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/e47a59bb-443a-405f-9c5d-02659d80e9e5/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/b3eaf3ab-43ef-4830-ab6d-12bafed3c61e/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/28f7352f-fe23-4003-93e1-41f4fedc6232/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/b07e2362-5851-4181-ba9a-15d9109ee4dd/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/677be4e3-722b-4e43-bd5a-36d8fbed6f86/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/0c817272-f873-475f-a401-dfe46a679a9f/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/7a3945d9-21bb-434a-9a4e-bfcdeb6194de/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/56aa2ad5-007d-407c-a644-48aac1e9a8f0/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
```

## Data Handling

### Automatic Experiment Setup (MetaPhlAn and HUMAnN output files only)

The above table can then be supplied to `loadMetagenomicData` and the
cached files will be parsed into a single SummarizedExperiment object
with sample metadata. At this point, only the `metaphlan_lists` data
types, `viral_clusters` and `relative_abundance`, as well as all
`humann` data_types, are compatible with this function.

``` r
merged_experiment <- loadMetagenomicData(cache_tbl)
```

### Stepwise Experiment Setup

Alternatively, we can parse the files, add metadata, and merge the
SummarizedExperiment objects in separate steps.

`parse_metaphlan_list` completes the first step for files with
‘data_type’ equal to “relative_abundance” or “viral_clusters”.
`parse_humann` is also available for HUMAnN output files.

``` r
parsed_rel_ab_list <- vector("list", nrow(cache_tbl))
names(parsed_rel_ab_list) <- cache_tbl$UUID

for (i in 1:nrow(cache_tbl)) {
    parsed_rel_ab_list[[i]] <- parse_metaphlan_list(sample_id = cache_tbl$UUID[i],
                                                file_path = cache_tbl$cache_path[i],
                                                data_type = cache_tbl$data_type[i])
    
}
```

Once the files have been loaded as SummarizedExperiment objects,
matching assays from multiple samples can be merged together with
`mergeExperiments`.

``` r
merged_rel_abs <- mergeExperiments(parsed_rel_ab_list)
```

    #| class: SummarizedExperiment 
    #| dim: 2915 24 
    #| metadata(0):
    #| assays(1): relative_abundance
    #| rownames(2915): UNCLASSIFIED k__Bacteria ...
    #|   k__Bacteria|p__Firmicutes|c__CFGB3009|o__OFGB3009|f__FGB3009|g__GGB31234|s__GGB31234_SGB14869|t__SGB14869
    #|   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Eubacteriales_unclassified|g__Eubacteriales_unclassified|s__Clostridiales_bacterium_CHKCI006|t__SGB7261
    #| rowData names(2): ncbi_tax_id additional_species
    #| colnames(24): 0807eb2a-a15e-4647-8e19-2600d8fda378
    #|   e0fbb54f-0249-4917-a4d7-bd68acb89c62 ...
    #|   7a3945d9-21bb-434a-9a4e-bfcdeb6194de
    #|   56aa2ad5-007d-407c-a644-48aac1e9a8f0
    #| colData names(0):

The corresponding metadata from sampleMetadata is then added as colData
with the function `add_metadata`.

``` r
rel_abs_with_metadata <- add_metadata(sample_ids = colnames(merged_rel_abs),
                                      id_col = "uuid",
                                      experiment = merged_rel_abs)
```

    #| class: SummarizedExperiment 
    #| dim: 2915 24 
    #| metadata(0):
    #| assays(1): relative_abundance
    #| rownames(2915): UNCLASSIFIED k__Bacteria ...
    #|   k__Bacteria|p__Firmicutes|c__CFGB3009|o__OFGB3009|f__FGB3009|g__GGB31234|s__GGB31234_SGB14869|t__SGB14869
    #|   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Eubacteriales_unclassified|g__Eubacteriales_unclassified|s__Clostridiales_bacterium_CHKCI006|t__SGB7261
    #| rowData names(2): ncbi_tax_id additional_species
    #| colnames(24): 0807eb2a-a15e-4647-8e19-2600d8fda378
    #|   e0fbb54f-0249-4917-a4d7-bd68acb89c62 ...
    #|   7a3945d9-21bb-434a-9a4e-bfcdeb6194de
    #|   56aa2ad5-007d-407c-a644-48aac1e9a8f0
    #| colData names(521): study_name BioProject ... date_of_birth cage
