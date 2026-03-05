## Construction of sample files for function examples:
## sample_metaphlan_bugs_list.tsv.gz for parse_relab_list()
## sample_metaphlan_viruses_list.tsv.gz for parse_viral_list()
## sample_out_pathabundance_unstratified.tsv.gz for parse_humann()
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

# write to inst/extdata/sample_metaphlan_bugs_list.tsv
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_metaphlan_bugs_list.tsv")
writeLines(metadata_header, extdata_path)
write.table(bugs_list, extdata_path, append = TRUE, sep = "\t",
            col.names = TRUE, row.names = FALSE, quote = FALSE)
R.utils::gzip(extdata_path, overwrite = TRUE)

## sample_metaphlan_viruses_list
metadata_header <- c("#mpa_vJun23_CHOCOPhlAnSGB_202403",
                     "#/usr/local/bin/metaphlan --input_type fastq --index latest --bowtie2db metaphlan --samout sam.bz2 --bowtie2out bowtie2.out --nproc 16 --profile_vsc --vsc_breadth 0.75 --vsc_out metaphlan_viruses_list.tsv -o metaphlan_bugs_list.tsv out.fastq",
                     "#SampleID\tMetaphlan_Analysis")
viruses_list <- data.frame('M-Group/Cluster' = c("M867", "M892", "M251", "M460",
                                                 "M351", "M638"),
                           genomeName = c("VDB|0021-00B6-0-0027|M867-c3230-c0-c8",
                                          "VDB|000E-000C-0-0002|M892-c3062-c0-c0",
                                          "VDB|0021-0030-0-000B|M251-c1586-c0-c0",
                                          "VDB|0002-0027-0-0005|M460-c1950-c0-c3",
                                          "VDB|0021-000A-0-0001|M351-c1836-c1-c4",
                                          "VDB|001F-0071-0-0007|M638-c3751-c0-c2"),
                           len = c(1977, 19587, 5485, 2774, 4614, 2914),
                           breadth_of_coverage = c(1, 1, 1, 1,
                                                   0.999783268313828,
                                                   0.999656829100892),
                           depth_of_coverage_mean = c(376.888720283257,
                                                      116.455301986011,
                                                      299.981586144029,
                                                      187.497116077866,
                                                      39.6668111857793,
                                                      12.266392035702),
                           depth_of_coverage_median = c(419, 119, 307, 186, 39, 12),
                           'M-Group-Type [k|u]' = rep("uVSG", 6),
                           'First Genome in Cluster' = c("-", "-",
                                                         "NC_001422_Coliphage_phiX174",
                                                         "-", "-", "-"),
                           'Other Genomes' = c("-", "-",
                                               "NC_0014221_Coliphage_phiX174",
                                               "-", "-", "-"),
                           check.names = FALSE)

# write to inst/extdata/sample_metaphlan_viruses_list.tsv
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_metaphlan_viruses_list.tsv")
writeLines(metadata_header, extdata_path)
write.table(viruses_list, extdata_path, append = TRUE, sep = "\t",
            col.names = TRUE, row.names = FALSE, quote = FALSE)
R.utils::gzip(extdata_path, overwrite = TRUE)

## sample_out_pathabundance_unstratified
pathcoverage_list <- data.frame(Pathway = c("UNMAPPED",
                                            "UNINTEGRATED",
                                            "TRNA-CHARGING-PWY: tRNA charging",
                                            "PWY-1042: glycolysis IV",
                                            "PWY-6609: adenine and adenosine salvage III",
                                            "PWY-7221: guanosine ribonucleotides de novo biosynthesis"),
                                out_Abundance = c("1963761.2729906905",
                                                  "3026993.4709948450",
                                                  "2697.9004105228",
                                                  "2360.2965535212",
                                                  "2328.2392201336",
                                                  "2292.9280094236"))
colnames(pathcoverage_list)[1] <- "#Pathway"

# write to inst/extdata/sample_out_pathabundance_unstratified.tsv
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_out_pathabundance_unstratified.tsv")
write.table(pathcoverage_list, extdata_path, append = TRUE, sep = "\t",
            col.names = TRUE, row.names = FALSE, quote = FALSE)
R.utils::gzip(extdata_path, overwrite = TRUE)

## sample_experiment
fpath <- file.path(system.file("extdata",
                               package = "parkinsonsMetagenomicData"),
                   "sample_metaphlan_bugs_list.tsv.gz")
parsed_bugs_list <- parse_metaphlan_list(sample_id = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
                                         file_path = fpath,
                                         data_type = "relative_abundance")

# write to inst/extdata/sample_experiment.Rds
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_experiment.Rds")
saveRDS(parsed_bugs_list, extdata_path)

## sample_experiment_list
cache_table <- cacheMetagenomicData(uuids = c("0001a4de-a907-46bd-8523-d7bfc1cdb544",
                                                          "004c5d07-ec87-40fe-9a72-6b23d6ec584e"),
                                                data_type = "relative_abundance")

se_list <- vector("list", nrow(cache_table))
for (i in seq_len(nrow(cache_table))) {
    se_list[[i]] <- parse_metaphlan_list(cache_table$uuid[i],
                                         cache_table$cache_path[i],
                                         cache_table$data_type[i])
}
names(se_list) <- paste(cache_table$uuid, cache_table$data_type, sep = "_")

# write to inst/extdata/sample_experiment_list.Rds
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_experiment_list.Rds")
saveRDS(se_list, extdata_path)

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
                          "sample_out_kneaddata_log.txt")
writeLines(lines_to_parse, extdata_path)

## sample_parquet
p_table <- data.frame(number = c(1, 2, 3, 4),
                      letter = c("A", "B", "C", "D"))

# write to inst/extdata/sample_table.parquet
extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "sample_table.parquet")
arrow::write_parquet(p_table, extdata_path)
