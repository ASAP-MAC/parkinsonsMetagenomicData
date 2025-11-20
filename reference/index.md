# Package index

## All functions

- [`accessParquetData()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/accessParquetData.md)
  : Set up DuckDB connection with views for available data types
- [`add_metadata()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/add_metadata.md)
  : Add sample metadata to TreeSummarizedExperiment object as colData
- [`biobakery_files()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/biobakery_files.md)
  : Read in extdata/biobakery_file_definitions.csv
- [`cacheMetagenomicData()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/cacheMetagenomicData.md)
  : Retrieve and cache output files
- [`cache_gcb()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/cache_gcb.md)
  : Cache Google Bucket object
- [`confirm_data_type()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/confirm_data_type.md)
  : Validate 'data_type' argument
- [`confirm_duckdb_con()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/confirm_duckdb_con.md)
  : Validate DuckDB connection argument
- [`confirm_duckdb_view()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/confirm_duckdb_view.md)
  : Validate DuckDB view/table argument
- [`confirm_filter_values()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/confirm_filter_values.md)
  : Validate 'filter_values' argument
- [`confirm_repo()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/confirm_repo.md)
  [`confirm_ref()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/confirm_repo.md)
  : Validate 'repo' argument
- [`confirm_uuids()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/confirm_uuids.md)
  : Validate UUIDs
- [`db_connect()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/db_connect.md)
  : Connect to DuckDB database instance
- [`detect_data_type()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/detect_data_type.md)
  : Detect which accepted data type a string is referring to
- [`file_to_hf()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/file_to_hf.md)
  : Convert standard https:// URLs to httpfs-compatible hf:// URLs
- [`filter_parquet_view()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/filter_parquet_view.md)
  : Filter a database view by any number of column:value argument pairs
- [`get_bucket_locators()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_bucket_locators.md)
  : Retrieve Google Bucket locators for output
- [`get_cdata_only()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_cdata_only.md)
  : Return unique colData columns for a data type
- [`get_exts()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_exts.md)
  : Return all "extensions" from a file path
- [`get_hf_parquet_urls()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_hf_parquet_urls.md)
  : Get Parquet File URLs and Metadata from a Hugging Face Repository
- [`get_ref_info()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_ref_info.md)
  : Return a table with information about available parquet reference
  files.
- [`get_repo_info()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/get_repo_info.md)
  : Return a table with information about available Hugging Face repos.
- [`interpret_and_filter()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/interpret_and_filter.md)
  : Select the view with the most appropriate sorting schema and filter
- [`listMetagenomicData()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/listMetagenomicData.md)
  : List metagenomic data available for download
- [`loadMetagenomicData()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/loadMetagenomicData.md)
  : Load cached files into R as a merged TreeSummarizedExperiment object
- [`loadParquetData()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/loadParquetData.md)
  : Retrieve data from a DuckDB view and convert to Summarized
  Experiment
- [`load_ref()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/load_ref.md)
  : Load a single parquet reference file
- [`mergeExperiments()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/mergeExperiments.md)
  : Merge TreeSummarizedExperiment objects with the same assay types
  together
- [`output_file_types()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/output_file_types.md)
  : Read in extdata/output_files.csv
- [`pMD_get_cache()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/pMD_get_cache.md)
  : Get location of dedicated file cache
- [`parquet_colinfo()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/parquet_colinfo.md)
  : Retrieve column info for parquet files based on original file type
- [`parquet_to_tse()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/parquet_to_tse.md)
  : Convert tabulated parquet file data to a Summarized Experiment
- [`parse_fastqc_stats()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/parse_fastqc_stats.md)
  : Parse FastQC data file for a single sample as a named vector
- [`parse_humann()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/parse_humann.md)
  : Parse HUMAnN output for a single sample as a
  TreeSummarizedExperiment object
- [`parse_kneaddata_stats()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/parse_kneaddata_stats.md)
  : Parse KneadData log file for a single sample as a named vector
- [`parse_metaphlan_list()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/parse_metaphlan_list.md)
  : Parse basic MetaPhlAn output for a single sample as a
  TreeSummarizedExperiment object
- [`pick_projection()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/pick_projection.md)
  : Choose the most appropriate DuckDB view/table for filtering
- [`retrieve_local_views()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/retrieve_local_views.md)
  : Create database views from local parquet files
- [`retrieve_views()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/retrieve_views.md)
  : Create database views for all available or requested data types
- [`returnSamples()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/returnSamples.md)
  : Return a TreeSummarizedExperiment with data based on sample data and
  feature data tables
- [`sampleMetadata`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/sampleMetadata.md)
  : Manually Curated Sample Metadata
- [`standardize_ordering()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/standardize_ordering.md)
  : Standardize the order of a vector of delimited strings
- [`view_parquet()`](https://asap-mac.github.io/parkinsonsMetagenomicData/reference/view_parquet.md)
  : Create a database view of a specific parquet file
