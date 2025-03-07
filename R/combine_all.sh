#!/bin/bash
# combine.sh

scratch="/shares/CIBIO-Storage/CM/scratch/users/kaelyn.long/retrieve/"
bugsdir="bugs/"
virusdir="viruses/"
unknowndir="unknown/"

cd ${scratch}

gunzip ${bugsdir}*.gz
gunzip ${virusdir}*.gz
gunzip ${unknowndir}*.gz

module load singularity-3.4.0
singularity exec ../cmd_nf/singularity/krlong68-curatedmetagenomics-samtools-1.21.sif merge_metaphlan_tables.py bugs/* > merged_bugs.tsv
#singularity exec ../cmd_nf/singularity/krlong68-curatedmetagenomics-samtools-1.21.sif merge_metaphlan_tables.py viruses/* > merged_viruses.tsv
singularity exec ../cmd_nf/singularity/krlong68-curatedmetagenomics-samtools-1.21.sif merge_metaphlan_tables.py unknown/* > merged_unknown.tsv