parkinsonsMetagenomicData
================

# Package Overview

This package is dedicated to retrieving, storing, and handling specific output
files produced with the [curatedMetagenomicsNextflow](https://github.com/seandavi/curatedMetagenomicsNextflow)
pipeline. For additional utility functions surrounding the analysis of the data
in these files, including the handling of taxonomy and statistical tests, go to
[biobakeryUtils](https://github.com/g-antonello/biobakeryUtils/tree/main)

## Available Data

The ASAP-MAC initiative has collected a number of Parkinson's Disease-focused
studies for metadata curation, uniform processing, and meta-analysis. The
ongoing process of the collection of these datasets can be followed in the
[parkinsons_data_search](https://github.com/ASAP-MAC/parkinsons_data_search)
repository. The majority of the studies listed in the table
[parkinson_shotgun_datasets.tsv](https://github.com/ASAP-MAC/parkinsons_data_search/blob/main/parkinson_shotgun_datasets.tsv)
are available for retrieval with this package.

To browse the available data, load the `sampleMetadata` object included in this
package. Additionally, calling `biobakery_files()` will provide a list of the
different output file types that are available for each sample.

### Sample Metadata

Metadata for all samples present within `parkinsonsMetagenomicData` is available
through the included `data.frame` `sampleMetadata`. Both curated and uncurated
features are included in this `data.frame`, with uncurated features being
prefixed by "uncurated_". Curated features include the following at this time:

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

### Output File Types

 - MetaPhlAn
   - viral_clusters
   - relative_abundance
   - marker_abundance
   - marker_presence
 - StrainPhlAn
   - strainphlan_markers
 - HUMAnN
   - genefamilies
   - genefamilies_cpm
   - genefamilies_relab
   - genefamilies_stratified
   - genefamilies_unstratified
   - genefamilies_cpm_stratified
   - genefamilies_relab_stratified
   - genefamilies_cpm_unstratified
   - genefamilies_relab_unstratified
   - pathabundance
   - pathabundance_cpm
   - pathabundance_relab
   - pathabundance_stratified
   - pathabundance_unstratified
   - pathabundance_cpm_stratified
   - pathabundance_relab_stratified
   - pathabundance_cpm_unstratified
   - pathabundance_relab_unstratified
   - pathcoverage_unstratified
   - pathcoverage_stratified
   - pathcoverage
 - FastQC
    - fastqc
 - KneadData
   - kneaddata_log

## Data Hosting

While the sample metadata are available within this package, the various output
files are hosted remotely due to their size and number. There are therefore two
options for data retrieval.

### Google Cloud Storage

The initial output location of the pipeline is the Google Cloud Bucket
`gs://metagenomics-mac`, which requires credentials for access. The creation of
these credentials is covered in the
[Google Cloud Storage vignette](https://asap-mac.github.io/parkinsonsMetagenomicData/docs/vignettes/google_cloud_storage.html), and you
will need the owner of the Google Cloud Project within which the Bucket is
contained to follow these steps and provide you with the resulting credentials.
Once you have access to the Bucket, the data will be stored in individual files
for each sample and output type, and can be accessed with the functions and
workflows detailed in the
[Google Cloud Storage vignette](https://asap-mac.github.io/parkinsonsMetagenomicData/docs/vignettes/google_cloud_storage.html).

### Hugging Face

While Google Cloud Storage is a good place to access the data as soon as they
have been processed, it requires credentialed access and more file wrangling.
As a simpler alternative, the data have been combined into parquet files and
hosted publicly on Hugging Face in the
[metagenomics_mac repo](https://huggingface.co/datasets/waldronlab/metagenomics_mac).
Smaller example files featuring data from 10 samples each can be found at
[metagenomics_mac_examples](https://huggingface.co/datasets/waldronlab/metagenomics_mac_examples).
These files are able to be easily accessed through the
[DuckDB R client](https://duckdb.org/docs/stable/clients/r.html) and the
functions and workflows detailed in the
[Parquet File vignette](https://asap-mac.github.io/parkinsonsMetagenomicData/docs/vignettes/parquet.html) streamline this process
even further.
