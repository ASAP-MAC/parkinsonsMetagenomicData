#### Functions to parse different pMD output files

#' @title Parse basic MetaPhlAn output for a single sample as a
#' SummarizedExperiment object
#' @description 'parse_metaphlan_list' reads a file obtained from running
#' MetaPhlAn for microbial profiling (with or without unclassified fraction
#' estimation) or viral sequence cluster analysis. This file is parsed into a
#' SummarizedExperiment object.
#' @param sample_id String: A sample identifier
#' @param file_path String: Path to a locally stored MetaPhlAn output file in
#' TSV format
#' @param data_type String: The type of MetaPhlAn output file to be parsed,
#' either 'relative_abundance' or 'viral_clusters'
#' @return A SummarizedExperiment object with process metadata, row data, column
#' names, and relevant assays.
#' @details This function does not integrate sample metadata as column data. The
#' provided sample_id is used as the column name for assays within the
#' SummarizedExperiment object and is intended to be used for integration of
#' sample metadata.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fpath <- file.path(system.file("extdata",
#'                                 package = "parkinsonsMetagenomicData"),
#'                     "sample_metaphlan_bugs_list.tsv.gz")
#'  parse_metaphlan_list(sample_id = "004c5d07-ec87-40fe-9a72-6b23d6ec584e",
#'                       file_path = fpath,
#'                       data_type = "relative_abundance")
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[S4Vectors]{DataFrame-class}}, \code{\link[S4Vectors]{S4VectorsOverview}}
#'  \code{\link[SummarizedExperiment]{SummarizedExperiment-class}}, \code{\link[SummarizedExperiment]{SummarizedExperiment}}
#' @rdname parse_metaphlan_list
#' @export
#' @importFrom readr read_tsv
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment SummarizedExperiment
parse_metaphlan_list <- function(sample_id, file_path, data_type) {
    ## Confirm data_type input is valid
    confirm_data_type(data_type, "subdir", "metaphlan_lists")

    ## Slight differences in output file format
    if (data_type == "relative_abundance") {
        ## Convert commented header lines to metadata
        meta <- readLines(file_path, n = 3)
        meta <- gsub("#| reads processed", "", meta)
        meta_list <- as.list(meta)
        names(meta_list) <- c("metaphlan_database",
                              "command",
                              "reads_processed")

        ## Read remainder of output file
        load_file <- readr::read_tsv(file_path, skip = 5,
                                     col_names = c("clade_name", "ncbi_tax_id",
                                                                        "relative_abundance", "additional_species"),
                                     col_types = cols("factor", "factor", "double", "factor"))

        ## Standardize "additional_species" ordering
        species_delim <- ","
        load_file$additional_species <- lapply(load_file$additional_species,
                                        function(x) {
                                            if (!is.na(x)) {
                                                str_split(x, pattern = species_delim) |>
                                                    unlist() |>
                                                    sort() |>
                                                    paste(collapse = species_delim)
                                            } else { x }
                                            }) |>
                                        unlist()

        ## Separate out row data
        rdata_cols <- c("ncbi_tax_id", "additional_species")
        rdata <- S4Vectors::DataFrame(load_file[,rdata_cols])
        rownames(rdata) <- load_file$clade_name

        ## Set sample ID as column name
        cdata <- S4Vectors::DataFrame(matrix(nrow = 1, ncol = 0))
        rownames(cdata) <- sample_id

        ## Set relative abundance as assay
        relabundance <- as.matrix(load_file$relative_abundance)
        alist <- list(relabundance)
        names(alist) <- "relative_abundance"

    } else if (data_type == "viral_clusters") {
        ## Convert commented header lines to metadata
        meta <- readLines(file_path, n = 2)
        meta <- gsub("#", "", meta)
        meta_list <- as.list(meta)
        names(meta_list) <- c("metaphlan_database",
                              "command")

        ## Read remainder of output file
        load_file <- readr::read_tsv(file_path, skip = 3)
        colnames(load_file) <- c("m_group_cluster", "genome_name", "length",
                                 "breadth_of_coverage",
                                 "depth_of_coverage_mean",
                                 "depth_of_coverage_median", "m_group_type_k_u",
                                 "first_genome_in_cluster", "other_genomes")

        ## Separate out row data
        rdata_cols <- c("m_group_cluster", "length", "m_group_type_k_u",
                        "first_genome_in_cluster", "other_genomes")
        rdata <- S4Vectors::DataFrame(load_file[,rdata_cols])
        rownames(rdata) <- load_file$genome_name

        ## Set sample ID as column name
        cdata <- S4Vectors::DataFrame(matrix(nrow = 1, ncol = 0))
        rownames(cdata) <- sample_id

        ## Set breadth and depth of coverage measurements as separate assays
        breadth <- as.matrix(load_file$breadth_of_coverage)
        depth_mean <- as.matrix(load_file$depth_of_coverage_mean)
        depth_median <- as.matrix(load_file$depth_of_coverage_median)
        alist <- list(breadth,
                      depth_mean,
                      depth_median)
        names(alist) <- c("viral_breadth_of_coverage",
                          "viral_depth_of_coverage_mean",
                          "viral_depth_of_coverage_median")
    } else {
        ## Notify if output file is not able to be parsed by this function
        stop(paste0("data_type '", data_type, "' is not 'relative_abundance' or 'viral_clusters'. Please enter one of these values or use a different parsing function."))
    }

    ## Combine process metadata, row data, sample ID, and assays into
    ## SummarizedExperiment object
    ex <- SummarizedExperiment::SummarizedExperiment(assays = alist,
                                                     rowData = rdata,
                                                     colData = cdata,
                                                     metadata = meta_list)

    return(ex)
}

#' @title Parse HUMAnN output for a single sample as a SummarizedExperiment
#' object
#' @description 'parse_humann' reads a file obtained from running HUMAnN on a
#' single sample. This file is parsed into a SummarizedExperiment object.
#' @param sample_id String: A sample identifier
#' @param file_path String: Path to a locally stored HUMAnN output file in
#' gzipped TSV format
#' @param data_type String: The type of HUMAnN output file to be parsed, as
#' found in output_file_types("tool", "humann")
#' @return A SummarizedExperiment object with process metadata, row names,
#' column names, and relevant assays.
#' @details This function does not integrate sample metadata as column data. The
#' provided sample_id is used as the column name for assays within the
#' SummarizedExperiment object and is intended to be used for integration of
#' sample metadata.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[S4Vectors]{DataFrame-class}}
#'  \code{\link[SummarizedExperiment]{SummarizedExperiment-class}}, \code{\link[SummarizedExperiment]{SummarizedExperiment}}
#' @rdname parse_humann
#' @export
#' @importFrom readr read_tsv
#' @importFrom S4Vectors make_zero_col_DFrame
#' @importFrom SummarizedExperiment SummarizedExperiment
parse_humann <- function(sample_id, file_path, data_type) {
    ## Confirm data_type input is valid
    confirm_data_type(data_type, "tool", "humann")

    ## Load in file
    load_file <- readr::read_tsv(file_path)

    ## Parse header row and data_type as assay name
    header_prefix <- colnames(load_file) |>
        paste(collapse = "_") |>
        tolower() |>
        gsub(pattern = "#\\s|out_|s$", replacement = "") |>
        gsub(pattern = "\\s|-", replacement = "_")

    type_suffix <- gsub(data_type,
                        pattern = "^[^_]+(?=_|$)",
                        replacement = "",
                        perl = TRUE)

    aname <- paste0(header_prefix, type_suffix)

    ## Set sample ID as column name
    cdata <- S4Vectors::make_zero_col_DFrame(1)
    rownames(cdata) <- sample_id

    ## Set rownames
    rdata <- S4Vectors::make_zero_col_DFrame(nrow(load_file))
    rownames(rdata) <- as.vector(unlist(load_file[,1]))

    ## Create assay
    thisassay <- as.matrix(load_file[,2])
    colnames(thisassay) <- NULL
    alist <- list(thisassay)
    names(alist) <- aname

    ## Combine sample ID, assays, and row names into SummarizedExperiment object
    ex <- SummarizedExperiment::SummarizedExperiment(assays = alist,
                                                     rowData = rdata,
                                                     colData = cdata)


    return(ex)
}

#' @title Parse FastQC data file for a single sample as a named vector
#' @description 'parse_fastqc_stats' reads a file obtained from running FastQC
#' on a FASTQ file. This file is parsed into a named vector.
#' @param file_path String: Path to a locally stored MetaPhlAn output file in
#' TXT format
#' @return A named vector containing a number of sample-level statistics.
#' @details 'parse_fastqc_stats' does not parse every single item of information
#' within the fastqc_data.txt file, but gathers only the 'Basic Statistics'
#' module.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fpath <- file.path(system.file("extdata",
#'                                 package = "parkinsonsMetagenomicData"),
#'                     "sample_fastqc_data.txt")
#'  parse_fastqc_stats(fpath)
#'  }
#' }
#' @rdname parse_fastqc_stats
#' @export
parse_fastqc_stats <- function(file_path) {
    ## Read in file
    file_lines <- readLines(file_path)

    ## Extract statistics section
    s <- grep("Basic Statistics", file_lines)+2
    e <- grep("END_MODULE", file_lines)[1]-1

    stats_mod <- file_lines[s:e] |>
        strsplit("\t")

    ## Format as named vector
    final_stats <- sapply(stats_mod, "[[", 2)
    names(final_stats) <- sapply(stats_mod, "[[", 1)

    return(final_stats)
}

#' @title Parse KneadData log file for a single sample as a named vector
#' @description 'parse_kneaddata_stats' reads a file obtained from running
#' KneadData on a FASTQ file. This file is parsed into a named vector.
#' @param file_path String: Path to a locally stored KneadData log file stored
#' in TXT format
#' @return A named vector containing a number of read counts at different stages
#' @details 'parse_kneaddata_stats' does not parse every single item of
#' information within the out_kneaddata.log file, but gathers only the lines
#' with read counts.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fpath <- file.path(system.file("extdata",
#'                                 package = "parkinsonsMetagenomicData"),
#'                     "sample_out_kneaddata.log")
#'  parse_kneaddata_stats(fpath)
#'  }
#' }
#' @rdname parse_kneaddata_stats
#' @export
parse_kneaddata_stats <- function(file_path) {
    ## Read in file
    file_lines <- readLines(file_path)

    ## Extract stats
    # Input, surviving, dropped reads
    reads <- file_lines[grep("Input Reads", file_lines)]

    in_reads <- unlist(regmatches(reads, regexec("Input Reads: \\d*", reads)))
    surv_reads <- unlist(regmatches(reads, regexec("Surviving: \\d*", reads)))
    drop_reads <- unlist(regmatches(reads, regexec("Dropped: \\d*", reads)))

    # Total contaminate sequences in file
    total_contaminate <- file_lines[grep("Total contaminate sequences in file", file_lines)]

    parsed_contaminate <- c()
    for (line in total_contaminate) {
        full <- unlist(regmatches(line, regexec("Total contaminate sequences in file .*$", line)))
        db <- unlist(regmatches(line, regexec("([^\\/]+).(?=\\s\\))", line, perl = TRUE)))[1]
        repdb <- gsub("(?<=\\()[^)]*(?=\\))", db, full, perl = TRUE)
        #repdb <- gsub("out_kneaddata_|_contam.fastq", "", repdb)
        parsed_contaminate <- c(parsed_contaminate, repdb)
    }

    # Total reads after removing those found in reference database
    reads_after_ref <- file_lines[grep("Total reads after removing those found in reference database", file_lines)]

    parsed_ref <- c()
    for (line in reads_after_ref) {
        full <- unlist(regmatches(line, regexec("Total reads after removing those found in reference database .*$", line)))
        db <- unlist(regmatches(line, regexec("([^\\/]+).(?=\\s\\))", line, perl = TRUE)))[1]
        repdb <- gsub("(?<=\\()[^)]*(?=\\))", db, full, perl = TRUE)
        #repdb <- gsub("out_kneaddata_|_contam.fastq", "", repdb)
        parsed_ref <- c(parsed_ref, repdb)
    }

    # Total reads after merging results from multiple databases
    reads_after_merge <- file_lines[grep("Total reads after merging results from multiple databases", file_lines)]

    merge_info <- unlist(regmatches(reads_after_merge,
                                    regexec("Total reads after merging results from multiple databases .*$",
                                            reads_after_merge)))
    final_reads_after_merge <- gsub("\\([^)]*\\)", "", merge_info)

    ## Combine and format info as named vector
    kneaddata_stats <- c(in_reads, surv_reads, drop_reads, parsed_contaminate,
                         parsed_ref, final_reads_after_merge) |>
        strsplit(":")

    clean_stats <- lapply(kneaddata_stats, trimws)

    final_stats <- sapply(clean_stats, "[[", 2)
    names(final_stats) <- sapply(clean_stats, "[[", 1)

    return(final_stats)
}
