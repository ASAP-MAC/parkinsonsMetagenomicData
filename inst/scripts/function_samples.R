## Construction of sample files for function examples:
## sample_metaphlan_bugs_list.tsv.gz for parse_metaphlan_list()
## sample_experiment for add_metadata()
## sample_experiment_list for mergeExperiments()
## sample_parquet for retrieve_local_files()

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

## sample_fastc_data
basic_stats_header <- c("##FastQC	0.12.1",
                        ">>Basic Statistics	pass",
                        "#Measure	Value",
                        "Filename	out_sample.fastq",
                        "File type	Conventional base calls",
                        "Encoding	Sanger / Illumina 1.9",
                        "Total Sequences	50000",
                        "Total Bases	7.2 Mbp",
                        "Sequences flagged as poor quality	0",
                        "Sequence length	99-150",
                        "%GC	52",
                        ">>END_MODULE")
accessory_module <- c(">>Per base sequence quality	pass",
                      "#Base	Mean	Median	Lower Quartile	Upper Quartile	10th Percentile	90th Percentile",
                      "1	36.03172	37.0	37.0	37.0	37.0	37.0",
                      "2	36.21164	37.0	37.0	37.0	37.0	37.0",
                      "3	36.31168	37.0	37.0	37.0	37.0	37.0",
                      ">>END_MODULE")

# write to inst/extdata/sample_fastqc_data.txt
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_fastqc_data.txt")
writeLines(c(basic_stats_header, accessory_module), extdata_path)

## sample_out_kneaddata
lines_to_parse <- c("04/22/2025 05:35:37 AM - kneaddata.utilities - DEBUG: b\"TrimmomaticSE: Started with arguments: -threads 16 -phred33 /shares/CIBIO-Storage/CM/scratch/users/kaelyn.long/cmd_nf/work/d2/801204c79cabd605fb1b59819c46a4/kneaddata_output/reformatted_identifiers6p450jvr_decompressed_gvl7_alr_out /shares/CIBIO-Storage/CM/scratch/users/kaelyn.long/cmd_nf/work/d2/801204c79cabd605fb1b59819c46a4/kneaddata_output/out_kneaddata.trimmed.fastq MINLEN:60 ILLUMINACLIP:/usr/local/lib/python3.9/site-packages/kneaddata/adapters/NexteraPE-PE.fa:2:30:10 SLIDINGWINDOW:4:20 MINLEN:74Using PrefixPair: 'AGATGTGTATAAGAGACAG' and 'AGATGTGTATAAGAGACAG'Using Long Clipping Sequence: 'GTCTCGTGGGCTCGGAGATGTGTATAAGAGACAG'Using Long Clipping Sequence: 'TCGTCGGCAGCGTCAGATGTGTATAAGAGACAG'Using Long Clipping Sequence: 'CTGTCTCTTATACACATCTCCGAGCCCACGAGAC'Using Long Clipping Sequence: 'CTGTCTCTTATACACATCTGACGCTGCCGACGA'ILLUMINACLIP: Using 1 prefix pairs, 4 forward/reverse sequences, 0 forward only sequences, 0 reverse only sequencesInput Reads: 39080984 Surviving: 37985114 (97.20%) Dropped: 1095870 (2.80%)TrimmomaticSE: Completed successfully\"",
                    "04/22/2025 05:43:50 AM - kneaddata.run - INFO: Total contaminate sequences in file ( /shares/CIBIO-Storage/CM/scratch/users/kaelyn.long/cmd_nf/work/d2/801204c79cabd605fb1b59819c46a4/kneaddata_output/out_kneaddata_hg37dec_v0.1_bowtie2_contam.fastq ) : 70118.0",
                    "04/22/2025 05:43:51 AM - kneaddata.run - INFO: Total contaminate sequences in file ( /shares/CIBIO-Storage/CM/scratch/users/kaelyn.long/cmd_nf/work/d2/801204c79cabd605fb1b59819c46a4/kneaddata_output/out_kneaddata_SILVA_128_LSUParc_SSUParc_ribosomal_RNA_bowtie2_contam.fastq ) : 266052.0",
                    "04/22/2025 05:45:10 AM - kneaddata.utilities - INFO: READ COUNT: decontaminated hg37dec_v0.1 single : Total reads after removing those found in reference database ( /shares/CIBIO-Storage/CM/scratch/users/kaelyn.long/cmd_nf/work/d2/801204c79cabd605fb1b59819c46a4/kneaddata_output/out_kneaddata_hg37dec_v0.1_bowtie2_clean.fastq ): 37914996.0",
                    "04/22/2025 05:46:10 AM - kneaddata.utilities - INFO: READ COUNT: decontaminated SILVA_128_LSUParc_SSUParc_ribosomal_RNA single : Total reads after removing those found in reference database ( /shares/CIBIO-Storage/CM/scratch/users/kaelyn.long/cmd_nf/work/d2/801204c79cabd605fb1b59819c46a4/kneaddata_output/out_kneaddata_SILVA_128_LSUParc_SSUParc_ribosomal_RNA_bowtie2_clean.fastq ): 37719062.0",
                    "04/22/2025 05:51:18 AM - kneaddata.utilities - INFO: READ COUNT: final single : Total reads after merging results from multiple databases ( /shares/CIBIO-Storage/CM/scratch/users/kaelyn.long/cmd_nf/work/d2/801204c79cabd605fb1b59819c46a4/kneaddata_output/out_kneaddata.fastq ): 37909213.0")

# write to inst/extdata/sample_fastqc_data.txt
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_out_kneaddata.log")
writeLines(lines_to_parse, extdata_path)

## sample_parquet
p_table <- data.frame(number = c(1, 2, 3, 4),
                      letter = c("A", "B", "C", "D"))

# write to inst/extdata/sample_table.parquet
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_table.parquet")
arrow::write_parquet(p_table, extdata_path)
