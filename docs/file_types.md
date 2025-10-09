Biobakery File Types
====================

This is a brief overview of each type of file that `parkinsonsMetagenomicData`
is intended to work with. A summary table is available with `biobakery_files()`.
This guide examines the structure and contents of each file type, and provide
common use cases for each one.

## MetaPhlAn

### Relative Abundance

This is the primary output for taxonomic profiling, showing the relative
abundance of each microbial taxon (from kingdom to species and strain level).
The base MetaPhlAn call to obtain this file is:

```bash
$ metaphlan metagenome.fastq --input_type fastq -o profiled_metagenome.txt
```

Here are 15 rows of this file for one sample. It contains a header with
information on the database, exact `metaphlan` command, and number of reads
processed as well as the standard MetaPhlAn header row and column names.

```text
#mpa_vJun23_CHOCOPhlAnSGB_202403
#/usr/local/bin/metaphlan --input_type bowtie2out --index latest --bowtie2db metaphlan --nproc 1 --unclassified_estimation -o metaphlan_unknown_list.tsv /dev/fd/63
#35820609 reads processed
#SampleID	Metaphlan_Analysis
#clade_name	NCBI_tax_id	relative_abundance	additional_species
UNCLASSIFIED	-1	14.32695	
k__Bacteria	2	85.33295166560956	
k__Archaea	2157	0.34009629850313816	
k__Bacteria|p__Firmicutes	2|1239	55.72767897077801	
k__Bacteria|p__Bacteroidota	2|976	27.98104021500391	
k__Bacteria|p__Proteobacteria	2|1224	1.2720648488663489	
k__Archaea|p__Euryarchaeota	2157|28890	0.34009629850313816	
k__Bacteria|p__Actinobacteria	2|201174	0.24810057959927395	
k__Bacteria|p__Bacteria_unclassified	2|	0.0951142178497579	
k__Bacteria|p__Bacteroidota|c__Bacteroidia|o__Bacteroidales|f__Bacteroidaceae|g__Bacteroides|s__Bacteroides_stercoris|t__SGB1830	2|976|200643|171549|815|816|46506|	0.19589142416994365	k__Bacteria|p__Bacteroidota|c__Bacteroidia|o__Bacteroidales|f__Bacteroidaceae|g__Bacteroides|s__Bacteroides_sp_NSJ_48
```

The columns contain the following information:

* clade_name: The taxonomic lineage of the detected microbial clade
* NCBI_tax_id: The NCBI Taxonomy identifier for the clade in clade_name
* relative_abundance: The proportion of the total microbial community
represented by the clade
* additional_species: Any other species represented by the same set of detected
markers

### Viral Clustering

This is the output file when performing Viral Sequence Clusters Analysis,
showing the coverage of viral genomes. The base MetaPhlAn call to obtain this
file is:

```bash
$ metaphlan metagenome.fastq --input_type fastq \
        --profile_vsc --vsc_out profiled_viruses.txt \
        -o profiled_metagenome.txt
```

```text
#mpa_vJun23_CHOCOPhlAnSGB_202403
#/usr/local/bin/metaphlan --input_type fastq --index latest --bowtie2db metaphlan --samout sam.bz2 --bowtie2out bowtie2.out --nproc 16 --profile_vsc --vsc_breadth 0.75 --vsc_out metaphlan_viruses_list.tsv -o metaphlan_bugs_list.tsv out.fastq
#SampleID	Metaphlan_Analysis
M-Group/Cluster	genomeName	len	breadth_of_coverage	depth_of_coverage_mean	depth_of_coverage_median	M-Group-Type [k|u]	First Genome in Cluster	Other Genomes
M697	VDB|0010-00F3-0-0001|M697-c3861-c6-c0	10022	1.0	2279.0755338255835	2374.0	uVSG	-	-
M441	VDB|0048-0014-0-0001|M441-c2353-c0-c2	8442	1.0	2474.0095948827293	1267.0	uVSG	-	-
M647	VDB|002C-0003-0-001D|M647-c2667-c1-c0	2631	1.0	117.01786393006462	111.0	uVSG	-	-
M282	VDB|001D-0115-0-0004|M282-c1770-c3-c58	1986	1.0	532.7703927492447	17.0	uVSG	-	-
M696	VDB|0047-0002-0-0004|M696-c2774-c1-c10	2219	0.9990986931050022	21.07577807848444	18.0	uVSG	-	-
M808	VDB|0010-0092-0-0000|M808-c3098-c0-c1	18422	0.9988057757029638	56.696902173913045	57.0	uVSG	-	-
M599	VDB|0003-0023-0-0000|M599-c2620-c0-c3	3025	0.9986776859504132	93.88282025819265	98.0	uVSG	-	-
M1075	VDB|0022-0016-0-0003|M1075-c3777-c1-c0	1606	0.9950186799501868	68.63329161451814	65.0	uVSG	-	-
M412	VDB|004F-0194-0-0003|M412-c2480-c2-c9	4124	0.9919980601357905	110.66071865069665	19.0	uVSG	-	-
M10	VDB|0010-004B-0-0002|M10-c1012-c0-c11	17451	0.9919202337974902	21.53056036972848	21.0	uVSG	-	-
M311	VDB|0026-0044-0-0000|M311-c1716-c0-c21	2349	0.9838229033631333	15.050194720900043	12.0	kVSG	NC_029014_Parabacteroides_phage_YZ2015b	NC_0290141_Parabacteroides_phage_YZ2015b
```

## HUMAnN


