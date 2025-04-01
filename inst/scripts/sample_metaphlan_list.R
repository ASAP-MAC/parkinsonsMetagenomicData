## Construction of sample_metaphlan_bugs_list.tsv.gz for parse_metaphlan_list example

# sample_metaphlan_bugs_list
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
