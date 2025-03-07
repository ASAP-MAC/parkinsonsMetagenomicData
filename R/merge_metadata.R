library(purrr)
library(dplyr)

# file structure
curation_dir <- "/home/kaelyn/Desktop/Work/ASAP_MAC/parkinsonsManualCuration"
curated_dir <- file.path(curation_dir, "curated_metadata")
original_dir <- file.path(curation_dir, "original_metadata")
map_dir <- "/home/kaelyn/Desktop/Work/ASAP_MAC/parkinsonsMetagenomicData/shotgun_samples/sample_uuid_maps"
accession_dir <- "/home/kaelyn/Desktop/Work/ASAP_MAC/pipeline/data/ParkinsonAccessions/ParkinsonAccessions/"

# curated metadata
temp <- list.files(path = curated_dir, pattern="^[^W]*\\.csv$")
curated_meta <- lapply(file.path(curated_dir, temp), read.csv)
names(curated_meta) <- c("bedarf", "boktor1", "boktor2", "jo", "lee", "mao", "nishiwaki")
#all_curated <- bind_rows(curated_meta)

# UUID maps
temp <- list.files(path = map_dir, pattern="^[^WZ]*\\.tsv$")
uuid_maps <- lapply(file.path(map_dir, temp), read.delim)
uuid_maps <- lapply(uuid_maps, function(x) x %>% rename(BioSample = sample_id))
names(uuid_maps) <- c("bedarf", "boktor1", "boktor2", "jo", "lee", "mao", "nishiwaki")
#all_uuids <- bind_rows(uuid_maps)

# accession lists
temp <- list.files(path = accession_dir, pattern="^[^WZ]*\\.csv$")
accession_lists <- lapply(file.path(accession_dir, temp), read.csv)
accession_lists <- lapply(accession_lists, function(x) x %>%
                              mutate(across(everything(), as.character)))
names(accession_lists) <- c("bedarf", "boktor1", "boktor2", "jo", "lee", "mao", "nishiwaki")
#all_accessions <- bind_rows(accession_lists)

# combine curated with original metadata
all_metas <- list("bedarf" = NULL,
                  "boktor" = NULL,
                  "jo" = NULL,
                  "lee" = NULL,
                  "mao" = NULL,
                  "nishiwaki" = NULL)

final_metas <- list("bedarf" = NULL,
                    "boktor" = NULL,
                    "jo" = NULL,
                    "lee" = NULL,
                    "mao" = NULL,
                    "nishiwaki" = NULL)

curated_cols <- c("curation_id",
                  "study_name",
                  "sample_id",
                  "subject_id",
                  "target_condition",
                  "target_condition_ontology_term_id",
                  "control",
                  "control_ontology_term_id", 
                  "age", 
                  "age_group", 
                  "age_group_ontology_term_id", 
                  "age_unit", 
                  "age_unit_ontology_term_id", 
                  "sex", 
                  "sex_ontology_term_id", 
                  "disease", 
                  "disease_ontology_term_id", 
                  "curator")

# bedarf
bedarf <- read.csv(file.path(original_dir, "BedarfJR_2017_metadata_newgrammar.tsv"),
                   sep = "\t") %>% rename_with( ~ paste0("uncurated_", .x))
all_metas$bedarf <- curated_meta$bedarf %>%
    left_join(bedarf,
              by = join_by(sample_id == uncurated_sample_id),
              suffix = c("", ""),
              keep = TRUE)
final_metas$bedarf <- uuid_maps$bedarf %>%
    left_join(all_metas$bedarf,
              by = join_by(NCBI_accession == uncurated_ncbi_accession),
              suffix = c("", ""),
              keep = TRUE)

# boktor
boktor <- read_xlsx(file.path(original_dir, "mds29300-sup-0017-tables10.xlsx"),
                    sheet = "metadata") %>%
    mutate(across(everything(), as.character)) %>%
    rename_with( ~ paste0("uncurated_", .x))
boktor_curated <- bind_rows(curated_meta$boktor1, curated_meta$boktor2)
boktor_accessions_left <- bind_rows(accession_lists$boktor1,
                                    accession_lists$boktor2) %>%
    filter(!host_subject_id %in% boktor$uncurated_host_subject_id) %>%
    rename_with( ~ paste0("uncurated_", .x))
boktor_uncurated <- bind_rows(boktor, boktor_accessions_left)
all_metas$boktor <- boktor_curated %>%
    left_join(boktor_uncurated,
              by = join_by(subject_id == uncurated_host_subject_id),
              suffix = c("", ""),
              keep = TRUE)
boktor_accessions <- bind_rows(accession_lists$boktor1 %>%
                                   select(BioSample, host_subject_id),
                               accession_lists$boktor2 %>%
                                   select(BioSample, host_subject_id))
all_metas$boktor <- boktor_accessions %>%
    right_join(all_metas$boktor,
               by = join_by(host_subject_id == uncurated_host_subject_id),
               keep = TRUE) %>%
    select(-host_subject_id)
boktor_uuids <- bind_rows(uuid_maps$boktor1, uuid_maps$boktor2)
final_metas$boktor <- boktor_uuids %>%
    left_join(all_metas$boktor,
              by = join_by(BioSample),
              suffix = c("", ""),
              keep = TRUE)

# jo
jo <- read.csv(file.path(original_dir, "JoS_2022.csv")) %>%
    rename_with( ~ paste0("uncurated_", .x))
all_metas$jo <- curated_meta$jo %>%
    left_join(jo,
              by = join_by(sample_id == uncurated_Sample.Name),
              suffix = c("", ""),
              keep = TRUE)
all_metas$jo <- accession_lists$jo %>%
    select(BioSample, Sample.Name) %>%
    right_join(all_metas$jo,
               by = join_by(Sample.Name == sample_id),
               keep = TRUE) %>%
    select(-Sample.Name)
final_metas$jo <- uuid_maps$jo %>%
    left_join(all_metas$jo,
              by = join_by(BioSample),
              suffix = c("", ""),
              keep = TRUE)

# lee
lee <- read.csv(file.path(original_dir, "LeeEJ_2024.csv")) %>%
    rename_with( ~ paste0("uncurated_", .x))
all_metas$lee <- curated_meta$lee %>%
    left_join(lee,
              by = join_by(sample_id == uncurated_Sample.Name),
              suffix = c("", ""),
              keep = TRUE)
all_metas$lee <- accession_lists$lee %>%
    select(BioSample, Sample.Name) %>%
    right_join(all_metas$lee,
               by = join_by(Sample.Name == sample_id),
               keep = TRUE) %>%
    select(-Sample.Name)
final_metas$lee <- uuid_maps$lee %>%
    left_join(all_metas$lee,
              by = join_by(BioSample),
              suffix = c("", ""),
              keep = TRUE)

# mao
mao <- read.csv(file.path(original_dir, "MaoL_2021_metadata.tsv"),
                sep = "\t") %>% rename_with( ~ paste0("uncurated_", .x))
all_metas$mao <- curated_meta$mao %>%
    left_join(mao,
              by = join_by(sample_id == uncurated_sample_id),
              suffix = c("", ""),
              keep = TRUE)
final_metas$mao <- uuid_maps$mao %>%
    left_join(all_metas$mao,
              by = join_by(NCBI_accession == uncurated_ncbi_accession),
              suffix = c("", ""),
              keep = TRUE)

# nishiwaki
nishiwaki <- read_xlsx(file.path(original_dir, "NishiwakiH_2024_rawMetadata.xlsx"),
                       sheet = "Sheet2")[,1:43] %>%
    rename_with( ~ paste0("uncurated_", .x))
all_metas$nishiwaki <- curated_meta$nishiwaki %>%
    left_join(nishiwaki,
              by = join_by(sample_id == uncurated_...1),
              suffix = c("", ""),
              keep = TRUE)
all_metas$nishiwaki <- accession_lists$nishiwaki %>%
    select(BioSample, Sample_name) %>%
    right_join(all_metas$nishiwaki,
               by = join_by(Sample_name == sample_id),
               keep = TRUE) %>%
    select(-Sample_name)
final_metas$nishiwaki <- uuid_maps$nishiwaki %>%
    left_join(all_metas$nishiwaki,
              by = join_by(BioSample),
              suffix = c("", ""),
              keep = TRUE)

final_metas <- lapply(final_metas, function(x) x %>%
                          mutate(across(-any_of(curated_cols), as.character)))
merged_metadata <- bind_rows(final_metas)
write.csv(merged_metadata, file = "/home/kaelyn/Desktop/Work/ASAP_MAC/parkinsonsMetagenomicData/shotgun_samples/merged_metadata.csv", row.names = FALSE)
