# Working With Data in Parquet Format

------------------------------------------------------------------------

## Parquet File Overview and Setup

### Hugging Face

Individual pipeline output files have been combined into parquet files
and hosted in the
[metagenomics_mac](https://huggingface.co/datasets/waldronlab/metagenomics_mac)
repo on Hugging Face. Smaller versions of those same files, comprising
only 10 samples per file, are available in the Hugging Face repo
[metagenomics_mac_examples](https://huggingface.co/datasets/waldronlab/metagenomics_mac_examples).
These are publicly accessible, and are able to be easily read using
DuckDB.

### Parquet Creation

To create the individual parquet files, one needs credentials to access
the `gs://metagenomics-mac` Google Bucket. Instructions on obtaining
these can be found in the [Google Cloud Storage
vignette](https://asap-mac.github.io/parkinsonsMetagenomicData/docs/vignettes/google_cloud_storage.html).
These credentials are then substituted into the scripts found in the
repo
[parquet_generation](https://github.com/ASAP-MAC/parquet_generation).
These scripts perform a number of transformations that increase
searchability before storing the combined data for each data type in
parquet files.

### DuckDB in R

DuckDB is very easy to use in R through the `duckdb` R package. The
`DBI` and `dplyr`/`dbplyr` packages combine with it to provide a
streamlined way to work with remote data by selectively querying it
before bringing it into your R session.

Relevant tools:

- [DuckDB R Client](https://duckdb.org/docs/stable/clients/r.html)
- [DBI](https://dbi.r-dbi.org/)
- [dplyr](https://dplyr.tidyverse.org/)/[dbplyr](https://dbplyr.tidyverse.org/)

## Standard Workflow

In the standard workflow, we use the main wrapper function
`returnSamples`. This function takes tables with sample and feature
information as well as a remote repo name or a vector of paths to
locally stored parquet files and retrieves the relevant data as a
TreeSummarizedExperiment.

Here is a brief overview of each argument to be provided to
`returnSamples`, please call
[`?returnSamples`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/returnSamples.md)
for additional info.

- **data_type**: the output file type of interest
- **sample_data**: a table of sample metadata, used to specify which
  samples to retrieve data for
- **feature_data**: a table of feature data, used to specify how to
  filter the raw data
- **repo**: the identifier of a remote repo where the raw parquet files
  are stored
- **local_files**: paths of locally stored parquet files, as an
  alternative to retrieval from a remote repo
- **include_empty_samples**: sometimes none of the features specified in
  `feature_data` are found in one or more samples specified in
  `sample_data`. Should the samples still be included in the result with
  NA values for each feature (TRUE) or should they be omitted (FALSE)?
  Omitting these samples may increase response speed when accessing
  remote repos.
- **dry_run**: a dry run returns a `tbl_duckdb_connection` object that
  contains all of the SQL code necessary to return the requested data.
  This SQL can be viewed by passing the object into
  [`dplyr::show_query()`](https://dplyr.tidyverse.org/reference/explain.html).
  This is useful for evaluating query efficiency prior to actually
  executing.

### File Selection

Both remote and local parquet files can be queried, though not at the
same time.
[`get_repo_info()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_repo_info.md)
gives the names and URLs of the available repos, and
[`get_hf_parquet_urls()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_hf_parquet_urls.md)
gives information on the files contained in those repos. Those files can
also be downloaded separately and then provided to the `local_files`
argument of
[`returnSamples()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/returnSamples.md).
This may be desirable if internet connection is unstable or limited, or
if a particular query runs into the repo’s rate limits.

At this point, we also want to make a note of which `data_type` values
we are interested in. The `data_type` associated with each parquet file
is listed in the output of
[`get_hf_parquet_urls()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_hf_parquet_urls.md).
For this example, we will be looking at the ‘relative_abundance’ data
type, which corresponds to the files “relative_abundance_uuid.parquet”
and “relative_abundance_clade_name_species.parquet”. The difference
between these two files is the column by which they are sorted:
“relative_abundance_uuid.parquet” is sorted by the ‘uuid’ column, while
“relative_abundance_clade_name_species.parquet” is sorted by the
‘clade_name_species’ column. Internal functions will choose which one to
use, we will simply provide “relative_abundance” as our chosen data
type. If you are downloading the files locally to avoid rate limits, you
would download all of the files associated with your chosen data type.

``` r
get_repo_info()
```

``` r
get_hf_parquet_urls(repo_name = "waldronlab/metagenomics_mac")
```

### Sample Table

We can then examine the available samples and optionally select a subset
to query. This is done by browsing the `sampleMetadata` object. Here is
an example of how a set of data might be selected for a meta-analysis
with some sampling parameters. Alternatively, this step can be left off
altogether. If the sample table is not provided when calling
[`returnSamples()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/returnSamples.md),
data for all samples will simply be returned.

``` r
sample_table <- sampleMetadata |>
    filter(study_name == "ZhangM_2023") |>
    select(where(~ !any(is.na(.x))))
```

### Feature Table

For the feature table, we first have to find the reference file that
goes with the data type we are interested in. To do this, call
[`get_ref_info()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_ref_info.md).
Search the ‘general_data_type’ column for your data type and identify
the reference file of interest. For a more granular view of what exactly
will be in each file, you can call
[`parquet_colinfo()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/parquet_colinfo.md)
with your data type and see which columns will be included. Here, we can
see that “clade_name_ref” is associated with “relative_abundance”.

``` r
get_ref_info()
```

And here we can see exactly which columns are in “clade_name_ref”.

``` r
parquet_colinfo("relative_abundance")
```

Once we have found the correct reference file, we can filter it to only
contain features we are interested in. Here, we are interested in values
for all bacteria in the genus “Faecalibacterium”. We load the reference
file with
[`load_ref()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/load_ref.md),
and use a basic [`grepl()`](https://rdrr.io/r/base/grep.html) filtering
method.

``` r
clade_name_ref <- load_ref("clade_name_ref")
feature_table <- clade_name_ref %>%
    filter(grepl("Faecalibacterium", clade_name_genus))
```

As an extra consideration, MetaPhlAn relative abundance output includes
aggregate values for each taxonomic level. We can see above that the
first row returned has a value of `NA` for the “clade_name_species”
column, and a few other rows have `NA` in the “clade_name_terminal”
column. You may want to remove these rows based on your analysis. To do
so, we would simply re-filter the file:

``` r
feature_table <- clade_name_ref %>%
    filter(grepl("Faecalibacterium", clade_name_genus)) %>%
    filter(!is.na(clade_name_species)) %>%
    filter(!is.na(clade_name_terminal))
```

### returnSamples()

We are now ready to retrieve our data. We simply pass our arguments into
[`returnSamples()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/returnSamples.md).
It may take a minute, depending on the amount of data requested or your
available resources.

``` r
experiment <- returnSamples(data_type = "relative_abundance",
                            sample_data = sample_table,
                            feature_data = feature_table,
                            repo = "waldronlab/metagenomics_mac",
                            local_files = NULL,
                            include_empty_samples = TRUE,
                            dry_run = FALSE)
experiment
#> class: TreeSummarizedExperiment 
#> dim: 9 24 
#> metadata(0):
#> assays(1): relative_abundance
#> rownames(9):
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_SGB15346|t__SGB15346
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15316
#>   ...
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15339
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_sp_CLA_AA_H233|t__SGB15315
#> rowData names(19): clade_name clade_name_kingdom ...
#>   NCBI_tax_id_terminal additional_species
#> colnames(24): eda61949-02dc-40ae-8dbe-bea2add85a52
#>   e47a59bb-443a-405f-9c5d-02659d80e9e5 ...
#>   09a9303d-d87d-4556-9672-04cbbcaf3d37
#>   677be4e3-722b-4e43-bd5a-36d8fbed6f86
#> colData names(524): uuid db_version ...
#>   uncurated_Day_of_stool_collection_digestion_issue
#>   uncurated_Day_of_stool_collection_constipation
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
#> rowLinks: NULL
#> rowTree: NULL
#> colLinks: NULL
#> colTree: NULL
```

If you are finding that this function is failing due to rate limiting
(HTTP 429 error), hanging, or simply taking longer than you would like,
you can download the files associated with the data type you are
interested in and supply their paths to the “local_files” argument in
lieu of specifying the “repo” argument. It would look something like
this:

``` r
local_files <- c("/path/to/relative_abundance_clade_name_species.parquet",
                 "/path/to/relative_abundance_uuid.parquet")

experiment <- returnSamples(data_type = "relative_abundance",
                            sample_data = sample_table,
                            feature_data = feature_table,
                            repo = NULL,
                            local_files = local_files,
                            include_empty_samples = TRUE,
                            dry_run = FALSE)
```

Finally, you can check exactly which query is being called on the raw
parquet data by passing “TRUE” to the “dry_run” argument. This returns a
`tbl_duckdb_connection` object that can be passed to
[`dplyr::show_query()`](https://dplyr.tidyverse.org/reference/explain.html).
To demonstrate:

``` r
query_only <- returnSamples(data_type = "relative_abundance",
                            sample_data = sample_table,
                            feature_data = feature_table,
                            repo = "waldronlab/metagenomics_mac",
                            local_files = NULL,
                            include_empty_samples = FALSE,
                            dry_run = TRUE)
dplyr::show_query(query_only)
#> <SQL>
#> SELECT q01.*
#> FROM (
#>   SELECT relative_abundance_clade_name_species.*
#>   FROM relative_abundance_clade_name_species
#>   WHERE (clade_name_species = 's__Faecalibacterium_SGB15346')
#> 
#>   UNION ALL
#> 
#>   SELECT relative_abundance_clade_name_species.*
#>   FROM relative_abundance_clade_name_species
#>   WHERE (clade_name_species = 's__Faecalibacterium_prausnitzii')
#> 
#>   UNION ALL
#> 
#>   SELECT relative_abundance_clade_name_species.*
#>   FROM relative_abundance_clade_name_species
#>   WHERE (clade_name_species = 's__Faecalibacterium_sp_An122')
#> 
#>   UNION ALL
#> 
#>   SELECT relative_abundance_clade_name_species.*
#>   FROM relative_abundance_clade_name_species
#>   WHERE (clade_name_species = 's__Faecalibacterium_sp_CLA_AA_H233')
#> 
#>   UNION ALL
#> 
#>   SELECT relative_abundance_clade_name_species.*
#>   FROM relative_abundance_clade_name_species
#>   WHERE (clade_name_species = 's__Faecalibacterium_sp_HTFF')
#> ) q01
#> WHERE
#>   (clade_name_kingdom = 'k__Bacteria') AND
#>   (clade_name_phylum = 'p__Firmicutes') AND
#>   (clade_name_class = 'c__Clostridia') AND
#>   (clade_name_order = 'o__Eubacteriales') AND
#>   (clade_name_family = 'f__Oscillospiraceae') AND
#>   (clade_name_genus = 'g__Faecalibacterium') AND
#>   (NCBI_tax_id_kingdom = '2') AND
#>   (NCBI_tax_id_phylum = '1239') AND
#>   (NCBI_tax_id_class = '186801') AND
#>   (NCBI_tax_id_order = '186802') AND
#>   (NCBI_tax_id_family = '216572') AND
#>   (NCBI_tax_id_genus = '216851') AND
#>   (NCBI_tax_id_terminal = '') AND
#>   (NCBI_tax_id IN ('2|1239|186801|186802|216572|216851||', '2|1239|186801|186802|216572|216851|853|', '2|1239|186801|186802|216572|216851|1965551|', '2|1239|186801|186802|216572|216851|2881266|', '2|1239|186801|186802|216572|216851|2929491|')) AND
#>   (NCBI_tax_id_species IN ('', '853', '1965551', '2881266', '2929491')) AND
#>   (clade_name IN ('k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_SGB15346|t__SGB15346', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15316', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15317', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15318', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15322', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15323', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15332', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15339', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15342', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_sp_An122|t__SGB15312', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_sp_CLA_AA_H233|t__SGB15315', 'k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_sp_HTFF|t__SGB15340')) AND
#>   (clade_name_terminal IN ('t__SGB15346', 't__SGB15316', 't__SGB15317', 't__SGB15318', 't__SGB15322', 't__SGB15323', 't__SGB15332', 't__SGB15339', 't__SGB15342', 't__SGB15312', 't__SGB15315', 't__SGB15340')) AND
#>   (uuid IN ('0807eb2a-a15e-4647-8e19-2600d8fda378', 'e0fbb54f-0249-4917-a4d7-bd68acb89c62', '25172837-2849-4db3-be91-d54d6a815d00', '39ddb5e7-97f6-4d3c-812b-9653b03f99b3', '7b152a7d-e244-4e2b-b924-7195c7ecfb10', 'dd30f93b-7999-47a4-93fb-21971b899939', '1406666f-04a8-43c9-983b-4ed62fd6da4a', 'fe3de3ca-3a14-4bd8-ae1c-0dad69edc9cd', '8707e374-5ddb-4220-8cbf-364b8b0e7be1', '22848a9c-66a6-4993-9058-cb6464edb42f', '08e2b754-78e2-4cb4-8ff2-95fd7b0ff44a', '9baef0b2-93d2-4a40-8082-d357c7f8156a', '09a9303d-d87d-4556-9672-04cbbcaf3d37', 'ac9f3532-90d8-412c-9c80-491037f0bcc2', 'eda61949-02dc-40ae-8dbe-bea2add85a52', '1f007260-be6c-4a21-800a-ad9c36129a0d', 'e47a59bb-443a-405f-9c5d-02659d80e9e5', 'b3eaf3ab-43ef-4830-ab6d-12bafed3c61e', '28f7352f-fe23-4003-93e1-41f4fedc6232', 'b07e2362-5851-4181-ba9a-15d9109ee4dd', '677be4e3-722b-4e43-bd5a-36d8fbed6f86', '0c817272-f873-475f-a401-dfe46a679a9f', '7a3945d9-21bb-434a-9a4e-bfcdeb6194de', '56aa2ad5-007d-407c-a644-48aac1e9a8f0'))
```

## Piecewise Workflow

In the piecewise workflow, we use the functions `accessParquetData` and
`loadParquetData` separately to have a little more control over our
data. First, `accessParquetData` sets up a DuckDB connection to the
parquet files that are either stored locally or hosted in a remote repo
(in this case, Hugging Face). `loadParquetData` then takes an argument
for data type as well as any filters and loads the requested data into R
as a Tree Summarized Experiment. We can even use `dplyr` functions to
customize the SQL query.

### File Selection

We can use the same resources of
[`get_repo_info()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_repo_info.md)
and
[`get_hf_parquet_urls()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_hf_parquet_urls.md)
to determine our files of interest and `data_type` value. As before, we
can download any files and alternatively supply them as local files.

We then run `accessParquetData`, which creates the DuckDB connection and
sets up DuckDB “VIEW” objects for each available file, whether remotely
hosted or locally stored. It returns a DuckDB connection object. If you
are using a remote repo, the `data_types` argument can be left blank to
access all available files, or a smaller number can be provided. DBI
functions can then be used to see which views are now available.

``` r
con <- accessParquetData(dbdir = ":memory:",
                         repo = "waldronlab/metagenomics_mac",
                         local_files = NULL,
                         data_types = "relative_abundance")
DBI::dbListTables(con)
#> [1] "relative_abundance_clade_name_species"
#> [2] "relative_abundance_uuid"
```

Supplying local files would look much as expected, and the “data_types”
argument need not be supplied:

``` r
local_files <- c("/path/to/relative_abundance_clade_name_species.parquet",
                 "/path/to/relative_abundance_uuid.parquet")

con <- accessParquetData(dbdir = ":memory:",
                         repo = NULL,
                         local_files = local_files,
                         data_types = NULL)
```

### Selecting Samples

The sample selection process is the same as in the standard workflow,
with the exception that we are only passing the UUID column to the
loading function. Here we will create the same table as earlier, then
pull just the UUIDS. This is again an optional step, as you may want to
get data across all samples for meta-analyses.

``` r
sample_table <- sampleMetadata |>
    filter(study_name == "ZhangM_2023") |>
    select(where(~ !any(is.na(.x))))
```

``` r
selected_uuids <- sample_table$uuid
selected_uuids
#>  [1] "0807eb2a-a15e-4647-8e19-2600d8fda378"
#>  [2] "e0fbb54f-0249-4917-a4d7-bd68acb89c62"
#>  [3] "25172837-2849-4db3-be91-d54d6a815d00"
#>  [4] "39ddb5e7-97f6-4d3c-812b-9653b03f99b3"
#>  [5] "7b152a7d-e244-4e2b-b924-7195c7ecfb10"
#>  [6] "dd30f93b-7999-47a4-93fb-21971b899939"
#>  [7] "1406666f-04a8-43c9-983b-4ed62fd6da4a"
#>  [8] "fe3de3ca-3a14-4bd8-ae1c-0dad69edc9cd"
#>  [9] "8707e374-5ddb-4220-8cbf-364b8b0e7be1"
#> [10] "22848a9c-66a6-4993-9058-cb6464edb42f"
#> [11] "08e2b754-78e2-4cb4-8ff2-95fd7b0ff44a"
#> [12] "9baef0b2-93d2-4a40-8082-d357c7f8156a"
#> [13] "09a9303d-d87d-4556-9672-04cbbcaf3d37"
#> [14] "ac9f3532-90d8-412c-9c80-491037f0bcc2"
#> [15] "eda61949-02dc-40ae-8dbe-bea2add85a52"
#> [16] "1f007260-be6c-4a21-800a-ad9c36129a0d"
#> [17] "e47a59bb-443a-405f-9c5d-02659d80e9e5"
#> [18] "b3eaf3ab-43ef-4830-ab6d-12bafed3c61e"
#> [19] "28f7352f-fe23-4003-93e1-41f4fedc6232"
#> [20] "b07e2362-5851-4181-ba9a-15d9109ee4dd"
#> [21] "677be4e3-722b-4e43-bd5a-36d8fbed6f86"
#> [22] "0c817272-f873-475f-a401-dfe46a679a9f"
#> [23] "7a3945d9-21bb-434a-9a4e-bfcdeb6194de"
#> [24] "56aa2ad5-007d-407c-a644-48aac1e9a8f0"
```

### Selecting Features

Our feature table is treated similarly, in that we are no longer
providing the whole table. Instead, we construct our filtering arguments
as a named list. The element name is the column to filter by, and the
element values will be exact matches. If this is our previous table:

``` r
feature_table <- clade_name_ref %>%
    filter(grepl("Faecalibacterium", clade_name_genus)) %>%
    filter(!is.na(clade_name_species)) %>%
    filter(!is.na(clade_name_terminal))
```

We would set up the filters like so. These two columns are the minimum
required to produce the same result as our prior example.

``` r
filter_values <- list(clade_name_species = unique(feature_table$clade_name_species),
                      clade_name_terminal = unique(feature_table$clade_name_terminal))
filter_values
#> $clade_name_species
#> [1] "s__Faecalibacterium_SGB15346"       "s__Faecalibacterium_prausnitzii"   
#> [3] "s__Faecalibacterium_sp_An122"       "s__Faecalibacterium_sp_CLA_AA_H233"
#> [5] "s__Faecalibacterium_sp_HTFF"       
#> 
#> $clade_name_terminal
#>  [1] "t__SGB15346" "t__SGB15316" "t__SGB15317" "t__SGB15318" "t__SGB15322"
#>  [6] "t__SGB15323" "t__SGB15332" "t__SGB15339" "t__SGB15342" "t__SGB15312"
#> [11] "t__SGB15315" "t__SGB15340"
```

Here is also where we supply our UUIDs.

``` r
filter_values <- c(list(uuid = selected_uuids), filter_values)
filter_values
#> $uuid
#>  [1] "0807eb2a-a15e-4647-8e19-2600d8fda378"
#>  [2] "e0fbb54f-0249-4917-a4d7-bd68acb89c62"
#>  [3] "25172837-2849-4db3-be91-d54d6a815d00"
#>  [4] "39ddb5e7-97f6-4d3c-812b-9653b03f99b3"
#>  [5] "7b152a7d-e244-4e2b-b924-7195c7ecfb10"
#>  [6] "dd30f93b-7999-47a4-93fb-21971b899939"
#>  [7] "1406666f-04a8-43c9-983b-4ed62fd6da4a"
#>  [8] "fe3de3ca-3a14-4bd8-ae1c-0dad69edc9cd"
#>  [9] "8707e374-5ddb-4220-8cbf-364b8b0e7be1"
#> [10] "22848a9c-66a6-4993-9058-cb6464edb42f"
#> [11] "08e2b754-78e2-4cb4-8ff2-95fd7b0ff44a"
#> [12] "9baef0b2-93d2-4a40-8082-d357c7f8156a"
#> [13] "09a9303d-d87d-4556-9672-04cbbcaf3d37"
#> [14] "ac9f3532-90d8-412c-9c80-491037f0bcc2"
#> [15] "eda61949-02dc-40ae-8dbe-bea2add85a52"
#> [16] "1f007260-be6c-4a21-800a-ad9c36129a0d"
#> [17] "e47a59bb-443a-405f-9c5d-02659d80e9e5"
#> [18] "b3eaf3ab-43ef-4830-ab6d-12bafed3c61e"
#> [19] "28f7352f-fe23-4003-93e1-41f4fedc6232"
#> [20] "b07e2362-5851-4181-ba9a-15d9109ee4dd"
#> [21] "677be4e3-722b-4e43-bd5a-36d8fbed6f86"
#> [22] "0c817272-f873-475f-a401-dfe46a679a9f"
#> [23] "7a3945d9-21bb-434a-9a4e-bfcdeb6194de"
#> [24] "56aa2ad5-007d-407c-a644-48aac1e9a8f0"
#> 
#> $clade_name_species
#> [1] "s__Faecalibacterium_SGB15346"       "s__Faecalibacterium_prausnitzii"   
#> [3] "s__Faecalibacterium_sp_An122"       "s__Faecalibacterium_sp_CLA_AA_H233"
#> [5] "s__Faecalibacterium_sp_HTFF"       
#> 
#> $clade_name_terminal
#>  [1] "t__SGB15346" "t__SGB15316" "t__SGB15317" "t__SGB15318" "t__SGB15322"
#>  [6] "t__SGB15323" "t__SGB15332" "t__SGB15339" "t__SGB15342" "t__SGB15312"
#> [11] "t__SGB15315" "t__SGB15340"
```

### Loading into R

We then provide our list of filtering arguments to `loadParquetData`
along with the database connection object and the data type we are
accessing and receive a Tree Summarized Experiment object. Loading the
data may take some time depending on the file type and queries.

``` r
basic_experiment <- loadParquetData(con = con,
                                    data_type = "relative_abundance",
                                    filter_values = filter_values,
                                    custom_view = NULL,
                                    include_empty_samples = TRUE,
                                    dry_run = FALSE)
basic_experiment
#> class: TreeSummarizedExperiment 
#> dim: 9 24 
#> metadata(0):
#> assays(1): relative_abundance
#> rownames(9):
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_SGB15346|t__SGB15346
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15316
#>   ...
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15339
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_sp_CLA_AA_H233|t__SGB15315
#> rowData names(19): clade_name clade_name_kingdom ...
#>   NCBI_tax_id_terminal additional_species
#> colnames(24): eda61949-02dc-40ae-8dbe-bea2add85a52
#>   e47a59bb-443a-405f-9c5d-02659d80e9e5 ...
#>   09a9303d-d87d-4556-9672-04cbbcaf3d37
#>   677be4e3-722b-4e43-bd5a-36d8fbed6f86
#> colData names(524): uuid db_version ...
#>   uncurated_Day_of_stool_collection_digestion_issue
#>   uncurated_Day_of_stool_collection_constipation
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
#> rowLinks: NULL
#> rowTree: NULL
#> colLinks: NULL
#> colTree: NULL
```

Performing a dry run in order to examine the SQL query would look much
as before:

``` r
query_only <- loadParquetData(con = con,
                              data_type = "relative_abundance",
                              filter_values = filter_values,
                              custom_view = NULL,
                              include_empty_samples = TRUE,
                              dry_run = TRUE)
dplyr::show_query(query_only)
#> <SQL>
#> SELECT q01.*
#> FROM (
#>   SELECT relative_abundance_clade_name_species.*
#>   FROM relative_abundance_clade_name_species
#>   WHERE (clade_name_species = 's__Faecalibacterium_SGB15346')
#> 
#>   UNION ALL
#> 
#>   SELECT relative_abundance_clade_name_species.*
#>   FROM relative_abundance_clade_name_species
#>   WHERE (clade_name_species = 's__Faecalibacterium_prausnitzii')
#> 
#>   UNION ALL
#> 
#>   SELECT relative_abundance_clade_name_species.*
#>   FROM relative_abundance_clade_name_species
#>   WHERE (clade_name_species = 's__Faecalibacterium_sp_An122')
#> 
#>   UNION ALL
#> 
#>   SELECT relative_abundance_clade_name_species.*
#>   FROM relative_abundance_clade_name_species
#>   WHERE (clade_name_species = 's__Faecalibacterium_sp_CLA_AA_H233')
#> 
#>   UNION ALL
#> 
#>   SELECT relative_abundance_clade_name_species.*
#>   FROM relative_abundance_clade_name_species
#>   WHERE (clade_name_species = 's__Faecalibacterium_sp_HTFF')
#> ) q01
#> WHERE
#>   (clade_name_terminal IN ('t__SGB15346', 't__SGB15316', 't__SGB15317', 't__SGB15318', 't__SGB15322', 't__SGB15323', 't__SGB15332', 't__SGB15339', 't__SGB15342', 't__SGB15312', 't__SGB15315', 't__SGB15340')) AND
#>   (uuid IN ('0807eb2a-a15e-4647-8e19-2600d8fda378', 'e0fbb54f-0249-4917-a4d7-bd68acb89c62', '25172837-2849-4db3-be91-d54d6a815d00', '39ddb5e7-97f6-4d3c-812b-9653b03f99b3', '7b152a7d-e244-4e2b-b924-7195c7ecfb10', 'dd30f93b-7999-47a4-93fb-21971b899939', '1406666f-04a8-43c9-983b-4ed62fd6da4a', 'fe3de3ca-3a14-4bd8-ae1c-0dad69edc9cd', '8707e374-5ddb-4220-8cbf-364b8b0e7be1', '22848a9c-66a6-4993-9058-cb6464edb42f', '08e2b754-78e2-4cb4-8ff2-95fd7b0ff44a', '9baef0b2-93d2-4a40-8082-d357c7f8156a', '09a9303d-d87d-4556-9672-04cbbcaf3d37', 'ac9f3532-90d8-412c-9c80-491037f0bcc2', 'eda61949-02dc-40ae-8dbe-bea2add85a52', '1f007260-be6c-4a21-800a-ad9c36129a0d', 'e47a59bb-443a-405f-9c5d-02659d80e9e5', 'b3eaf3ab-43ef-4830-ab6d-12bafed3c61e', '28f7352f-fe23-4003-93e1-41f4fedc6232', 'b07e2362-5851-4181-ba9a-15d9109ee4dd', '677be4e3-722b-4e43-bd5a-36d8fbed6f86', '0c817272-f873-475f-a401-dfe46a679a9f', '7a3945d9-21bb-434a-9a4e-bfcdeb6194de', '56aa2ad5-007d-407c-a644-48aac1e9a8f0'))
```

### Optional View Customization

Additionally, it is possible to use `dplyr` to preview the data view of
interest and perform more advanced functions on the data prior to using
`loadParquetData` or the `collect()` function to load it into R. Here we
use the `tbl()` function to access the data view of choice, and pipe it
into a [`filter()`](https://rdrr.io/r/stats/filter.html) call to select
only rows that have a value in the “additional_species” column. We can
then save these calls to a variable prior to loading them into R, and
supply this to `loadParquetData` either instead of or alongside our
other standard filtering arguments.

``` r
custom_filter <- tbl(con, "relative_abundance_uuid") %>%
    filter(!is.na(additional_species))
```

We can see a preview of the data by calling the saved view:

``` r
custom_filter
#> # Source:   SQL [?? x 26]
#> # Database: DuckDB 1.4.2 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#>    clade_name_kingdom clade_name_phylum clade_name_class clade_name_order    
#>    <chr>              <chr>             <chr>            <chr>               
#>  1 k__Bacteria        p__Firmicutes     c__Clostridia    o__Eubacteriales    
#>  2 k__Bacteria        p__Bacteroidota   c__Bacteroidia   o__Bacteroidales    
#>  3 k__Bacteria        p__Firmicutes     c__Bacilli       o__Lactobacillales  
#>  4 k__Bacteria        p__Bacteroidota   c__Bacteroidia   o__Bacteroidales    
#>  5 k__Bacteria        p__Bacteroidota   c__Bacteroidia   o__Bacteroidales    
#>  6 k__Bacteria        p__Firmicutes     c__Clostridia    o__Eubacteriales    
#>  7 k__Bacteria        p__Firmicutes     c__Clostridia    o__Eubacteriales    
#>  8 k__Bacteria        p__Firmicutes     c__Clostridia    o__Eubacteriales    
#>  9 k__Bacteria        p__Firmicutes     c__Clostridia    o__Eubacteriales    
#> 10 k__Bacteria        p__Actinobacteria c__Actinomycetia o__Bifidobacteriales
#> # ℹ more rows
#> # ℹ 22 more variables: clade_name_family <chr>, clade_name_genus <chr>,
#> #   clade_name_species <chr>, clade_name_terminal <chr>,
#> #   NCBI_tax_id_kingdom <chr>, NCBI_tax_id_phylum <chr>,
#> #   NCBI_tax_id_class <chr>, NCBI_tax_id_order <chr>, NCBI_tax_id_family <chr>,
#> #   NCBI_tax_id_genus <chr>, NCBI_tax_id_species <chr>,
#> #   NCBI_tax_id_terminal <chr>, clade_name <chr>, NCBI_tax_id <chr>, …
```

Or show the query prior to running:

``` r
dplyr::show_query(custom_filter)
#> <SQL>
#> SELECT relative_abundance_uuid.*
#> FROM relative_abundance_uuid
#> WHERE (NOT((additional_species IS NULL)))
```

We then simply provide that custom view to
[`loadParquetData()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/loadParquetData.md).

``` r
custom_experiment <- loadParquetData(con = con,
                                     data_type = "relative_abundance",
                                     filter_values = filter_values,
                                     custom_view = custom_filter,
                                     include_empty_samples = TRUE,
                                     dry_run = FALSE)
custom_experiment
#> class: TreeSummarizedExperiment 
#> dim: 7 24 
#> metadata(0):
#> assays(1): relative_abundance
#> rownames(7):
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15316
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15332
#>   ...
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii|t__SGB15323
#>   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Oscillospiraceae|g__Faecalibacterium|s__Faecalibacterium_sp_CLA_AA_H233|t__SGB15315
#> rowData names(19): clade_name clade_name_kingdom ...
#>   NCBI_tax_id_terminal additional_species
#> colnames(24): 0807eb2a-a15e-4647-8e19-2600d8fda378
#>   08e2b754-78e2-4cb4-8ff2-95fd7b0ff44a ...
#>   e47a59bb-443a-405f-9c5d-02659d80e9e5
#>   eda61949-02dc-40ae-8dbe-bea2add85a52
#> colData names(524): uuid db_version ...
#>   uncurated_Day_of_stool_collection_digestion_issue
#>   uncurated_Day_of_stool_collection_constipation
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
#> rowLinks: NULL
#> rowTree: NULL
#> colLinks: NULL
#> colTree: NULL
```

### Large File Considerations

Handling the larger data types, like “genefamilies_stratified” and
similar files, works the exact same way but must take a few factors into
consideration. There are two main limiters:

1.  Hugging Face rate limits
2.  Result size

To avoid running into Hugging Face’s rate limiting (HTTP 429 error), we
want to make sure that all filtering arguments are very selective and
only executed on sorted columns. Using “genefamilies_stratified” as an
example, we can filter by “uuid” and “gene_family_uniref”, since those
are the two parquet files available. Filtering by “gene_family_species”
*while we are still accessing the parquet in a lazy manner* can throw an
error since a lot of the values in that column show up much more often
in the dataset. To filter these non-sorted columns, it is recommended to
use alternate filters that leverage the sorted columns (e.g., uuid) to
create the initial subset and TreeSummarizedExperiment, then apply the
less selective filters to the now locally available data.

Alternatively, we can download these large files. This will completely
eliminate the rate limits since we are no longer accessing them
remotely.

These options do need to be balanced with your resources for storing and
handling both the raw and retrieved data, however. It may be helpful to
place a DuckDB database file in a location capable of large file
storage, or run R within a high-resource environment.

Below, a few example large file queries are executed. They may take a
few seconds to fully load the data.

``` r
# Establish connection to genefamilies_stratified files
con_gs <- accessParquetData(data_types = "genefamilies_stratified")

# Example query: we are looking for the below gene families within a particular
# study.
#
# UniRef90_T4BVE4 - present only in SPF mice
# UniRef90_A0A1B1SA57 - present only in WildR mice

# Pull study metadata
selected_samples <- sampleMetadata |>
    filter(study_name == "MazmanianS_DumitrescuDG") |>
    select(where(~ !any(is.na(.x))))

# Do as much as possible to narrow down sample IDs prior to filtering
wildr_ids <- selected_samples |>
    filter(uncurated_donor_microbiome_type == "WildR") |>
    dplyr::pull(uuid)

spf_ids <- selected_samples |>
    filter(uncurated_donor_microbiome_type == "SPF") |>
    dplyr::pull(uuid)

# Run the queries
spf_ex <- loadParquetData(con_gs, "genefamilies_stratified",
                          filter_values = list(gene_family_uniref = "UniRef90_T4BVE4",
                                               uuid = spf_ids))
spf_ex
#> class: TreeSummarizedExperiment 
#> dim: 4 14 
#> metadata(0):
#> assays(1): rpk_abundance
#> rownames(4): UniRef90_T4BVE4|g__Bacteroides.s__Bacteroides_vulgatus
#>   UniRef90_T4BVE4|unclassified
#>   UniRef90_T4BVE4|g__Bacteroides.s__Bacteroides_thetaiotaomicron
#>   UniRef90_T4BVE4|g__Parabacteroides.s__Parabacteroides_distasonis
#> rowData names(4): gene_family gene_family_uniref gene_family_genus
#>   gene_family_species
#> colnames(14): 1d949e2f-8bdb-48b7-bcf5-171c37d9ad66
#>   d2b9638a-8c15-4d1d-b5cd-3efeeeed0f2f ...
#>   01cae6dd-3b93-4abd-9f17-5fbc6a82863e
#>   ba26be3f-c2d9-4103-96e6-eacc0ff75afd
#> colData names(520): uuid humann_header ...
#>   uncurated_Day_of_stool_collection_digestion_issue
#>   uncurated_Day_of_stool_collection_constipation
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
#> rowLinks: NULL
#> rowTree: NULL
#> colLinks: NULL
#> colTree: NULL

wildr_ex <- loadParquetData(con_gs, "genefamilies_stratified",
                            filter_values = list(gene_family_uniref = "UniRef90_A0A1B1SA57",
                                                 uuid = wildr_ids))
wildr_ex
#> class: TreeSummarizedExperiment 
#> dim: 2 14 
#> metadata(0):
#> assays(1): rpk_abundance
#> rownames(2):
#>   UniRef90_A0A1B1SA57|g__Muribaculum.s__Muribaculum_intestinale
#>   UniRef90_A0A1B1SA57|unclassified
#> rowData names(4): gene_family gene_family_uniref gene_family_genus
#>   gene_family_species
#> colnames(14): 265da01f-bce1-4735-8184-43f42ba5f34b
#>   1e626f8b-ce4e-4a6a-ae29-82e7ec86b8b8 ...
#>   bfede8e1-e9fd-434f-9f74-7b1cd424edd7
#>   985f49c5-a0d2-428d-98f4-b458b0c2c0da
#> colData names(520): uuid humann_header ...
#>   uncurated_Day_of_stool_collection_digestion_issue
#>   uncurated_Day_of_stool_collection_constipation
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
#> rowLinks: NULL
#> rowTree: NULL
#> colLinks: NULL
#> colTree: NULL

# The full sample ID list also works, but is a bit slower
all_ex <- loadParquetData(con_gs, "genefamilies_stratified",
                          filter_values = list(gene_family_uniref = c("UniRef90_T4BVE4", "UniRef90_A0A1B1SA57"),
                                               uuid = selected_samples$uuid))
all_ex
#> class: TreeSummarizedExperiment 
#> dim: 6 28 
#> metadata(0):
#> assays(1): rpk_abundance
#> rownames(6): UniRef90_T4BVE4|g__Bacteroides.s__Bacteroides_vulgatus
#>   UniRef90_T4BVE4|unclassified ...
#>   UniRef90_A0A1B1SA57|g__Muribaculum.s__Muribaculum_intestinale
#>   UniRef90_A0A1B1SA57|unclassified
#> rowData names(4): gene_family gene_family_uniref gene_family_genus
#>   gene_family_species
#> colnames(28): 1d949e2f-8bdb-48b7-bcf5-171c37d9ad66
#>   d2b9638a-8c15-4d1d-b5cd-3efeeeed0f2f ...
#>   bfede8e1-e9fd-434f-9f74-7b1cd424edd7
#>   985f49c5-a0d2-428d-98f4-b458b0c2c0da
#> colData names(520): uuid humann_header ...
#>   uncurated_Day_of_stool_collection_digestion_issue
#>   uncurated_Day_of_stool_collection_constipation
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
#> rowLinks: NULL
#> rowTree: NULL
#> colLinks: NULL
#> colTree: NULL
```
