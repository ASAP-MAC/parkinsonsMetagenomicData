parkinsonsMetagenomicData
================

<!-- badges: start -->
[![R CMD Check + BiocCheck](https://github.com/ASAP-MAC/parkinsonsMetagenomicData/actions/workflows/pr_check.yml/badge.svg)](https://github.com/ASAP-MAC/parkinsonsMetagenomicData/actions/workflows/pr_check.yml)
[![Codecov test coverage](https://codecov.io/gh/ASAP-MAC/parkinsonsMetagenomicData/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/ASAP-MAC/parkinsonsMetagenomicData?branch=devel)
<!-- badges: end -->

# Package Overview

This package provides access to uniformly processed gut microbiome data from Parkinson's Disease studies. The gut-brain axis has emerged as an important factor in Parkinson's Disease, with multiple studies identifying differences in the gut microbiome composition between individuals with Parkinson's and healthy controls.

**What data is available?**

This package provides comprehensive microbiome profiling data from fecal samples, including:

- **Microbial composition**: Which bacteria, viruses, and other microorganisms are present and their relative abundances
- **Bacterial strain identification**: Fine-resolution tracking of specific bacterial strains
- **Functional profiles**: What genes and metabolic pathways are encoded by the gut microbiome
- **Quality metrics**: Technical information about sequencing depth and data quality

All data have been uniformly processed through the [curatedMetagenomicsNextflow](https://github.com/seandavi/curatedMetagenomicsNextflow) pipeline using standardized bioinformatics tools (MetaPhlAn, StrainPhlAn, HUMAnN, FastQC, KneadData) to ensure consistency across studies.

For additional analysis utilities including taxonomic handling and statistical tests, see [biobakeryUtils](https://github.com/g-antonello/biobakeryUtils/tree/main).

## Available Data

The ASAP-MAC initiative has collected a number of Parkinson's Disease-focused
studies for metadata curation, uniform processing, and meta-analysis. The
ongoing process of the collection of these datasets can be followed in the
[parkinsons_data_search](https://github.com/ASAP-MAC/parkinsons_data_search)
repository. The majority of the studies listed in the table
[parkinson_shotgun_datasets.tsv](https://github.com/ASAP-MAC/parkinsons_data_search/blob/main/parkinson_shotgun_datasets.tsv)
are available for retrieval with this package.

To browse the available data, load the `sampleMetadata` object included in this
package. To see which data types are available in the remote repositories, use
`get_repo_info()` and `get_hf_parquet_urls()`. See the vignettes for examples.

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

### Data Types

Data are organized into several categories based on biological content:

**Taxonomic Composition** (MetaPhlAn outputs)
- `relative_abundance`: Relative abundance of bacterial, archaeal, viral, and eukaryotic species
- `viral_clusters`: Viral community composition
- `marker_abundance`, `marker_presence`: Species-specific genetic markers for taxonomic identification

**Strain-Level Profiling** (StrainPhlAn outputs)
- `strainphlan_markers`: Strain-specific genetic markers for tracking bacterial variants

**Functional Profiling** (HUMAnN outputs)
- `genefamilies_*`: Abundance of gene families (groups of related genes)
- `pathabundance_*`: Abundance of metabolic pathways (e.g., carbohydrate metabolism, amino acid synthesis)
- `pathcoverage_*`: Coverage/completeness of metabolic pathways

*Normalization options:* Raw counts, relative abundance (relab), or copies per million (cpm); some data are stratified by contributing species, others are community totals (unstratified).

**Quality Control**
- `fastqc`: Sequencing quality metrics (read quality, GC content, adapter contamination)
- `kneaddata_log`: Pre-processing statistics (human DNA removal, quality filtering)

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
