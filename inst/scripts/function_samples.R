## Construction of sample files for function examples:
## sample_metaphlan_bugs_list.tsv.gz for parse_metaphlan_list()
## sample_experiment for add_metadata()
## sample_experiment_list for mergeExperiments()

library(parkinsonsMetagenomicData)

## sample_metaphlan_bugs_list
metadata_header <- c("#mpa_vJun23_CHOCOPhlAnSGB_202403",
                     "#/usr/local/bin/metaphlan --input_type fastq --index latest --bowtie2db metaphlan --samout sam.bz2 --bowtie2out bowtie2.out --nproc 16 --profile_vsc --vsc_breadth 0.75 --vsc_out metaphlan_viruses_list.tsv -o metaphlan_bugs_list.tsv out.fastq",
                     "#18304939 reads processed",
                     "#SampleID\tMetaphlan_Analysis")
bugs_list <- data.frame(clade_name = c("k__Bacteria",
                                       "k__Archaea",
                                       "k__Bacteria|p__Firmicutes",
                                       "k__Bacteria|p__Bacteroidota",
                                       "k__Bacteria|p__Actinobacteria",
                                       "k__Archaea|p__Euryarchaeota"),
                        NCBI_tax_id = c("2", "2157", "2|1239", "2|976",
                                        "2|201174", "2157|28890"),
                        relative_abundance = c(93.45547, 6.54453, 65.30563,
                                               10.38759, 9.47493, 6.54453),
                        additional_species = rep(NA, 6))
colnames(bugs_list)[1] <- "#clade_name"

# write to inst/extdata/sample_metaphlan_bugs_list.tsv.gz
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_metaphlan_bugs_list.tsv")
writeLines(metadata_header, extdata_path)
write.table(bugs_list, extdata_path, append = TRUE, sep = "\t",
            col.names = TRUE, row.names = FALSE, quote = FALSE)
R.utils::gzip(extdata_path, overwrite = TRUE)

## sample_experiment
fpath <- file.path(system.file("extdata",
                               package = "parkinsonsMetagenomicData"),
                   "sample_metaphlan_bugs_list.tsv.gz")
parsed_bugs_list <- parse_metaphlan_list(sample_id = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
                                         file_path = fpath,
                                         data_type = "bugs")

# write to inst/extdata/sample_experiment.Rds
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_experiment.Rds")
saveRDS(parsed_bugs_list, extdata_path)

## sample_experiment_list
sample_experiments <- getMetagenomicData(uuids = c("0001a4de-a907-46bd-8523-d7bfc1cdb544",
                                                   "004c5d07-ec87-40fe-9a72-6b23d6ec584e"),
                                         data_types = "bugs",
                                         load = TRUE)

# write to inst/extdata/sample_experiment_list.Rds
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_experiment_list.Rds")
saveRDS(sample_experiments, extdata_path)
