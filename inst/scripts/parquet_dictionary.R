## Construction of parquet_dictionary.csv

vc_dict <- data.frame(general_data_type = "viral_clusters",
                      col_name = c("M-Group/Cluster", "genomeName", "len",
                                 "breadth_of_coverage",
                                 "depth_of_coverage_mean",
                                 "depth_of_coverage_median",
                                 "M-Group-Type [k|u]",
                                 "First Genome in Cluster", "Other Genomes",
                                 "filename"),
                      col_class = NA,
                      description = NA,
                      se_role = c("rdata", "rname", "rdata", "assay", "assay",
                                  "assay", "rdata", "rdata", "rdata", "cname"),
                      position = 1:10)

ra_dict <- data.frame(general_data_type = "relative_abundance",
                      col_name = c("clade_name", "NCBI_tax_id",
                                   "relative_abundance", "additional_species",
                                   "filename"),
                      col_class = NA,
                      description = NA,
                      se_role = c("rname", "rdata", "assay", "rdata", "cname"),
                      position = 1:5)

ma_dict <- data.frame(general_data_type = "marker_abundance",
                      col_name = c("marker", "abundance", "filename"),
                      col_class = NA,
                      description = NA,
                      se_role = c("rname", "assay", "cname"),
                      position = 1:3)

mp_dict <- data.frame(general_data_type = "marker_presence",
                      col_name = c("marker", "presence", "filename"),
                      col_class = NA,
                      description = NA,
                      se_role = c("rname", "assay", "cname"),
                      position = 1:3)

genefamilies_dict <- data.frame(general_data_type = "genefamilies",
                                col_name = c("Gene Family",
                                              "out_Abundance-RPKs", "filename"),
                                col_class = NA,
                                description = NA,
                                se_role = c("rname", "assay", "cname"),
                                position = 1:3)

pathabundance_dict <- data.frame(general_data_type = "pathabundance",
                                 col_name = c("Pathway", "out_Abundance",
                                               "filename"),
                                 col_class = NA,
                                 description = NA,
                                 se_role = c("rname", "assay", "cname"),
                                 position = 1:3)

pathcoverage_dict <- data.frame(general_data_type = "pathcoverage",
                                col_name= c("Pathway", "out_Coverage",
                                              "filename"),
                                col_class = NA,
                                description = NA,
                                se_role = c("rname", "assay", "cname"),
                                position = 1:3)

combined_dict <- rbind(vc_dict, ra_dict, ma_dict, mp_dict, genefamilies_dict,
                       pathabundance_dict, pathcoverage_dict)

extdata_path <- file.path(system.file("extdata",
                                      package = "parkinsonsMetagenomicData"),
                          "parquet_dictionary.csv")
write.table(combined_dict, extdata_path, sep = ",", col.names = TRUE,
            row.names = FALSE)
