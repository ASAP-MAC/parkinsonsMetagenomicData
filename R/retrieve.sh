#!/bin/bash
# retrieve.sh

source /shares/CIBIO-Storage/CM/scratch/tools/20231211_2023.09_anaconda3/.conda
conda activate metagenomicsMAC

input_file="$1"
input_list=$(cut -f1 $input_file | tail -n +2)

scratch="/shares/CIBIO-Storage/CM/scratch/users/kaelyn.long/retrieve/"
bugsdir="bugs/"
virusdir="viruses/"
unknowndir="unknown/"

cd ${scratch}

mkdir -p ${bugsdir}
mkdir -p ${virusdir}
mkdir -p ${unknowndir}

for id in $input_list;
do
    gcloud storage cp gs://metagenomics-mac/results/cMDv4/${id}/metaphlan_lists/metaphlan_bugs_list.tsv.gz ${bugsdir}${id}.tsv.gz
    gcloud storage cp gs://metagenomics-mac/results/cMDv4/${id}/metaphlan_lists/metaphlan_viruses_list.tsv.gz ${virusdir}${id}.tsv.gz
    gcloud storage cp gs://metagenomics-mac/results/cMDv4/${id}/metaphlan_lists/metaphlan_unknown_list.tsv.gz ${unknowndir}${id}.tsv.gz
done