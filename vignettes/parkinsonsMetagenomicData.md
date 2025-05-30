---
title: "parkinsonsMetagenomicData"
output: 
  rmarkdown::html_vignette:
      keep_md: TRUE
vignette: >
  %\VignetteIndexEntry{parkinsonsMetagenomicData}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: inline
---





## Sample Metadata

Metadata for all samples present within `parkinsonsMetagenomicData` is available
through the included `data.frame` `sampleMetadata`. Both curated and uncurated
features are included in this `data.frame`, with uncurated features being
prefixed by "uncurated_". Curated features include the following at this time:

 * curation_id
 * study_name
 * sample_id
 * subject_id
 * target_condition
 * target_condition_ontology_term_id
 * control
 * control_ontology_term_id
 * age
 * age_group
 * age_group_ontology_term_id
 * age_unit
 * age_unit_ontology_term_id
 * sex
 * sex_ontology_term_id
 * disease
 * disease_ontology_term_id
 * curator
 * BioProject
 * BioSample
 * NCBI_accession
 * uuid


``` r
selected_samples <- sampleMetadata |>
    filter(study_name == "ZhangM_2023") |>
    select(where(~ !any(is.na(.x))))
```


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-485f0437e81bcb092fe9" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-485f0437e81bcb092fe9">{"x":{"filter":"none","vertical":false,"extensions":["Responsive"],"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"],["ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023","ZhangM_2023"],["PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875"],["SAMN35813273","SAMN35813274","SAMN35813275","SAMN35813276","SAMN35813277","SAMN35813278","SAMN35813279","SAMN35813280","SAMN35813281","SAMN35813282","SAMN35813283","SAMN35813284","SAMN35813285","SAMN35813286","SAMN35813287","SAMN35813288","SAMN35813289","SAMN35813290","SAMN35813291","SAMN35813292","SAMN35813293","SAMN35813294","SAMN35813295","SAMN35813296"],["SRR25109817","SRR25109816","SRR25109805","SRR25109800","SRR25109799","SRR25109798","SRR25109797","SRR25109796","SRR25109795","SRR25109794","SRR25109815","SRR25109814","SRR25109813","SRR25109812","SRR25109811","SRR25109810","SRR25109809","SRR25109808","SRR25109807","SRR25109806","SRR25109804","SRR25109803","SRR25109802","SRR25109801"],["0807eb2a-a15e-4647-8e19-2600d8fda378","e0fbb54f-0249-4917-a4d7-bd68acb89c62","25172837-2849-4db3-be91-d54d6a815d00","39ddb5e7-97f6-4d3c-812b-9653b03f99b3","7b152a7d-e244-4e2b-b924-7195c7ecfb10","dd30f93b-7999-47a4-93fb-21971b899939","1406666f-04a8-43c9-983b-4ed62fd6da4a","fe3de3ca-3a14-4bd8-ae1c-0dad69edc9cd","8707e374-5ddb-4220-8cbf-364b8b0e7be1","22848a9c-66a6-4993-9058-cb6464edb42f","08e2b754-78e2-4cb4-8ff2-95fd7b0ff44a","9baef0b2-93d2-4a40-8082-d357c7f8156a","09a9303d-d87d-4556-9672-04cbbcaf3d37","ac9f3532-90d8-412c-9c80-491037f0bcc2","eda61949-02dc-40ae-8dbe-bea2add85a52","1f007260-be6c-4a21-800a-ad9c36129a0d","e47a59bb-443a-405f-9c5d-02659d80e9e5","b3eaf3ab-43ef-4830-ab6d-12bafed3c61e","28f7352f-fe23-4003-93e1-41f4fedc6232","b07e2362-5851-4181-ba9a-15d9109ee4dd","677be4e3-722b-4e43-bd5a-36d8fbed6f86","0c817272-f873-475f-a401-dfe46a679a9f","7a3945d9-21bb-434a-9a4e-bfcdeb6194de","56aa2ad5-007d-407c-a644-48aac1e9a8f0"],["ZhangM_2023:F_PD1","ZhangM_2023:F_PD2","ZhangM_2023:F_PD3","ZhangM_2023:F_PD5","ZhangM_2023:F_PD7","ZhangM_2023:F_PD16","ZhangM_2023:F_PD19","ZhangM_2023:F_PD24","ZhangM_2023:F_PD27","ZhangM_2023:F_PD28","ZhangM_2023:F_PD29","ZhangM_2023:M_PD4","ZhangM_2023:M_PD6","ZhangM_2023:M_PD9","ZhangM_2023:M_PD10","ZhangM_2023:M_PD11","ZhangM_2023:M_PD13","ZhangM_2023:M_PD17","ZhangM_2023:M_PD18","ZhangM_2023:M_PD20","ZhangM_2023:M_PD21","ZhangM_2023:M_PD30","ZhangM_2023:M_PD31","ZhangM_2023:M_PD32"],["F_PD1","F_PD2","F_PD3","F_PD5","F_PD7","F_PD16","F_PD19","F_PD24","F_PD27","F_PD28","F_PD29","M_PD4","M_PD6","M_PD9","M_PD10","M_PD11","M_PD13","M_PD17","M_PD18","M_PD20","M_PD21","M_PD30","M_PD31","M_PD32"],["F_PD1","F_PD2","F_PD3","F_PD5","F_PD7","F_PD16","F_PD19","F_PD24","F_PD27","F_PD28","F_PD29","M_PD4","M_PD6","M_PD9","M_PD10","M_PD11","M_PD13","M_PD17","M_PD18","M_PD20","M_PD21","M_PD30","M_PD31","M_PD32"],["Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease","Parkinson Disease"],["NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845"],["Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case","Case"],["NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152","NCIT:C49152"],["Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease","Parkinson disease"],["NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845","NCIT:C26845"],["Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long","Kaelyn Long"],["SRR25109817","SRR25109816","SRR25109805","SRR25109800","SRR25109799","SRR25109798","SRR25109797","SRR25109796","SRR25109795","SRR25109794","SRR25109815","SRR25109814","SRR25109813","SRR25109812","SRR25109811","SRR25109810","SRR25109809","SRR25109808","SRR25109807","SRR25109806","SRR25109804","SRR25109803","SRR25109802","SRR25109801"],["WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS","WGS"],["281","274","282","277","280","287","291","289","290","292","291","280","280","290","282","286","286","290","289","290","290","293","292","291"],["5126812288","4637651878","4260703402","5073414290","4328969224","7837329037","5290309611","7625510674","8090442929","5777147435","5661210166","4332986236","5371153114","5654310289","4128355169","7327386738","7324599259","7868775948","7628779061","8040835028","5194000455","5795748371","5862491101","5693155117"],["PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875","PRJNA985875"],["SAMN35813273","SAMN35813274","SAMN35813275","SAMN35813276","SAMN35813277","SAMN35813278","SAMN35813279","SAMN35813280","SAMN35813281","SAMN35813282","SAMN35813283","SAMN35813284","SAMN35813285","SAMN35813286","SAMN35813287","SAMN35813288","SAMN35813289","SAMN35813290","SAMN35813291","SAMN35813292","SAMN35813293","SAMN35813294","SAMN35813295","SAMN35813296"],["1709080646","1540916207","1430374561","1688915874","1446219669","2588558077","1801807568","2520270145","2703434143","1922688037","1877184419","1448195135","1798553487","1915323502","1378503453","2419592743","2407455235","2593857558","2548175809","2672160392","1762093191","1933525398","1935544819","1895741776"],["THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY","THE AFFILIATED HUAIAN NO.1 PEOPLE HOSPITAL OF NANJING MEDICAL UNIVERSITY"],["public","public","public","public","public","public","public","public","public","public","public","public","public","public","public","public","public","public","public","public","public","public","public","public"],["fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra","fastq,run.zq,sra"],["gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3","gs,ncbi,s3"],["gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1","gs.us-east1,ncbi.public,s3.us-east-1"],["SRX20862822","SRX20862823","SRX20862834","SRX20862839","SRX20862840","SRX20862841","SRX20862842","SRX20862843","SRX20862844","SRX20862845","SRX20862824","SRX20862825","SRX20862826","SRX20862827","SRX20862828","SRX20862829","SRX20862830","SRX20862831","SRX20862832","SRX20862833","SRX20862835","SRX20862836","SRX20862837","SRX20862838"],["China","China","China","China","China","China","China","China","China","China","China","China","China","China","China","China","China","China","China","China","China","China","China","China"],["Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia"],["China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou","China:HangZhou"],["Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000","Illumina NovaSeq 6000"],["F_PD1","F_PD2","F_PD3","F_PD5","F_PD7","F_PD16","F_PD19","F_PD24","F_PD27","F_PD28","F_PD29","M_PD4","M_PD6","M_PD9","M_PD10","M_PD11","M_PD13","M_PD17","M_PD18","M_PD20","M_PD21","M_PD30","M_PD31","M_PD32"],["SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE","SINGLE"],["RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM","RANDOM"],["METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC","METAGENOMIC"],["human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome","human metagenome"],["ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA","ILLUMINA"],["2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z","2023-07-02T00:00:00Z"],["2023-07-02T04:11:00Z","2023-07-02T04:11:00Z","2023-07-02T04:11:00Z","2023-07-02T04:10:00Z","2023-07-02T04:13:00Z","2023-07-02T04:18:00Z","2023-07-02T04:10:00Z","2023-07-02T04:12:00Z","2023-07-02T04:14:00Z","2023-07-02T04:13:00Z","2023-07-02T04:12:00Z","2023-07-02T04:11:00Z","2023-07-02T04:11:00Z","2023-07-02T04:14:00Z","2023-07-02T04:10:00Z","2023-07-02T04:12:00Z","2023-07-02T04:14:00Z","2023-07-02T04:18:00Z","2023-07-02T04:20:00Z","2023-07-02T04:16:00Z","2023-07-02T04:10:00Z","2023-07-02T04:11:00Z","2023-07-02T04:12:00Z","2023-07-02T04:12:00Z"],["1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1"],["F_PD1","F_PD2","F_PD3","F_PD5","F_PD7","F_PD16","F_PD19","F_PD24","F_PD27","F_PD28","F_PD29","M_PD4","M_PD6","M_PD9","M_PD10","M_PD11","M_PD13","M_PD17","M_PD18","M_PD20","M_PD21","M_PD30","M_PD31","M_PD32"],["SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959","SRP446959"],["2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12","2022-12"],["Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental","Metagenome or environmental"],["Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens","Homo sapiens"],["30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E","30 N 120 E"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>study_name<\/th>\n      <th>BioProject<\/th>\n      <th>BioSample<\/th>\n      <th>NCBI_accession<\/th>\n      <th>uuid<\/th>\n      <th>curation_id<\/th>\n      <th>sample_id<\/th>\n      <th>subject_id<\/th>\n      <th>target_condition<\/th>\n      <th>target_condition_ontology_term_id<\/th>\n      <th>control<\/th>\n      <th>control_ontology_term_id<\/th>\n      <th>disease<\/th>\n      <th>disease_ontology_term_id<\/th>\n      <th>curator<\/th>\n      <th>uncurated_Run<\/th>\n      <th>uncurated_Assay.Type<\/th>\n      <th>uncurated_AvgSpotLen<\/th>\n      <th>uncurated_Bases<\/th>\n      <th>uncurated_BioProject<\/th>\n      <th>uncurated_BioSample<\/th>\n      <th>uncurated_Bytes<\/th>\n      <th>uncurated_Center.Name<\/th>\n      <th>uncurated_Consent<\/th>\n      <th>uncurated_DATASTORE.filetype<\/th>\n      <th>uncurated_DATASTORE.provider<\/th>\n      <th>uncurated_DATASTORE.region<\/th>\n      <th>uncurated_Experiment<\/th>\n      <th>uncurated_geo_loc_name_country<\/th>\n      <th>uncurated_geo_loc_name_country_continent<\/th>\n      <th>uncurated_geo_loc_name<\/th>\n      <th>uncurated_Instrument<\/th>\n      <th>uncurated_Library.Name<\/th>\n      <th>uncurated_LibraryLayout<\/th>\n      <th>uncurated_LibrarySelection<\/th>\n      <th>uncurated_LibrarySource<\/th>\n      <th>uncurated_Organism<\/th>\n      <th>uncurated_Platform<\/th>\n      <th>uncurated_ReleaseDate<\/th>\n      <th>uncurated_create_date<\/th>\n      <th>uncurated_version<\/th>\n      <th>uncurated_Sample.Name<\/th>\n      <th>uncurated_SRA.Study<\/th>\n      <th>uncurated_Collection_Date<\/th>\n      <th>uncurated_BioSampleModel<\/th>\n      <th>uncurated_HOST<\/th>\n      <th>uncurated_lat_lon<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"study_name","targets":1},{"name":"BioProject","targets":2},{"name":"BioSample","targets":3},{"name":"NCBI_accession","targets":4},{"name":"uuid","targets":5},{"name":"curation_id","targets":6},{"name":"sample_id","targets":7},{"name":"subject_id","targets":8},{"name":"target_condition","targets":9},{"name":"target_condition_ontology_term_id","targets":10},{"name":"control","targets":11},{"name":"control_ontology_term_id","targets":12},{"name":"disease","targets":13},{"name":"disease_ontology_term_id","targets":14},{"name":"curator","targets":15},{"name":"uncurated_Run","targets":16},{"name":"uncurated_Assay.Type","targets":17},{"name":"uncurated_AvgSpotLen","targets":18},{"name":"uncurated_Bases","targets":19},{"name":"uncurated_BioProject","targets":20},{"name":"uncurated_BioSample","targets":21},{"name":"uncurated_Bytes","targets":22},{"name":"uncurated_Center.Name","targets":23},{"name":"uncurated_Consent","targets":24},{"name":"uncurated_DATASTORE.filetype","targets":25},{"name":"uncurated_DATASTORE.provider","targets":26},{"name":"uncurated_DATASTORE.region","targets":27},{"name":"uncurated_Experiment","targets":28},{"name":"uncurated_geo_loc_name_country","targets":29},{"name":"uncurated_geo_loc_name_country_continent","targets":30},{"name":"uncurated_geo_loc_name","targets":31},{"name":"uncurated_Instrument","targets":32},{"name":"uncurated_Library.Name","targets":33},{"name":"uncurated_LibraryLayout","targets":34},{"name":"uncurated_LibrarySelection","targets":35},{"name":"uncurated_LibrarySource","targets":36},{"name":"uncurated_Organism","targets":37},{"name":"uncurated_Platform","targets":38},{"name":"uncurated_ReleaseDate","targets":39},{"name":"uncurated_create_date","targets":40},{"name":"uncurated_version","targets":41},{"name":"uncurated_Sample.Name","targets":42},{"name":"uncurated_SRA.Study","targets":43},{"name":"uncurated_Collection_Date","targets":44},{"name":"uncurated_BioSampleModel","targets":45},{"name":"uncurated_HOST","targets":46},{"name":"uncurated_lat_lon","targets":47}],"order":[],"autoWidth":false,"orderClasses":false,"responsive":true}},"evals":[],"jsHooks":[]}</script>
```


## Output files

The UUID(s) of any sample(s) of interest may be then used to access
output files. Available data types and information about their full file paths
can be found with `output_file_types()`


``` r
ftypes <- output_file_types()
```


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-395b1a3188d527638eb1" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-395b1a3188d527638eb1">{"x":{"filter":"none","vertical":false,"extensions":["Responsive"],"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27"],["viral_clusters","relative_abundance","marker_abundance","marker_presence","genefamilies","genefamilies_cpm","genefamilies_relab","genefamilies_stratified","genefamilies_unstratified","genefamilies_cpm_stratified","genefamilies_relab_stratified","genefamilies_cpm_unstratified","genefamilies_relab_unstratified","pathabundance","pathabundance_cpm","pathabundance_relab","pathabundance_stratified","pathabundance_unstratified","pathabundance_cpm_stratified","pathabundance_relab_stratified","pathabundance_cpm_unstratified","pathabundance_relab_unstratified","pathcoverage_unstratified","pathcoverage_stratified","pathcoverage","fastqc","kneaddata_log"],["metaphlan_viruses_list.tsv.gz","metaphlan_unknown_list.tsv.gz","marker_abundance.tsv.gz","marker_presence.tsv.gz","out_genefamilies.tsv.gz","out_genefamilies_cpm.tsv.gz","out_genefamilies_relab.tsv.gz","out_genefamilies_stratified.tsv.gz","out_genefamilies_unstratified.tsv.gz","out_genefamilies_cpm_stratified.tsv.gz","out_genefamilies_relab_stratified.tsv.gz","out_genefamilies_cpm_unstratified.tsv.gz","out_genefamilies_relab_unstratified.tsv.gz","out_pathabundance.tsv.gz","out_pathabundance_cpm.tsv.gz","out_pathabundance_relab.tsv.gz","out_pathabundance_stratified.tsv.gz","out_pathabundance_unstratified.tsv.gz","out_pathabundance_cpm_stratified.tsv.gz","out_pathabundance_relab_stratified.tsv.gz","out_pathabundance_cpm_unstratified.tsv.gz","out_pathabundance_relab_unstratified.tsv.gz","out_pathcoverage_unstratified.tsv.gz","out_pathcoverage_stratified.tsv.gz","out_pathcoverage.tsv.gz","fastqc_data.txt","out_kneaddata.log"],["metaphlan_lists/","metaphlan_lists/","metaphlan_markers/","metaphlan_markers/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","humann/","fasterq_dump/out_sample_fastqc/","kneaddata/kneaddata_output/"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>data_type<\/th>\n      <th>file_name<\/th>\n      <th>subdir<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"data_type","targets":1},{"name":"file_name","targets":2},{"name":"subdir","targets":3}],"order":[],"autoWidth":false,"orderClasses":false,"responsive":true}},"evals":[],"jsHooks":[]}</script>
```


At the moment, only the `metaphlan_lists` data types, `viral_clusters` and
`relative_abundance`, as well as the `humann` data types, have parsing functions
for automatically loading into SummarizedExperiment objects.

## Google Bucket Setup

Output files are contained in the private Google Cloud Bucket
`gs://metagenomics-mac` at the moment, the user must authenticate with a service
account keyfile in the following way:


``` r
googleCloudStorageR::gcs_auth("full/path/to/keyfile.json")
```

To obtain this keyfile, the owner of a service account affiliated with the
Google Cloud Project containing `gs://metagenomics-mac` must create it
according to the process detailed in the Google Cloud Guide
"[Create and delete service account keys](https://cloud.google.com/iam/docs/keys-create-delete)".

Additionally, the default bucket for `GoogleCloudStorageR` must be set to the
Google Bucket name, in this case `metagenomics-mac`. This is done automatically
upon loading the package, but if an error occurs can be manually done through
the following:


``` r
googleCloudStorageR::gcs_global_bucket("metagenomics-mac")
```

## Data Retrieval

All objects stored in `gs://metagenomics-mac` can then be viewed with the
following. This operation can take several minutes due to the number of objects
stored in `gs://metagenomics-mac`.


``` r
googleCloudStorageR::gcs_list_objects()
```

Alternatively, `listMetagenomicData` will provide a table of objects that are compatible
with the `cacheMetagenomicData` streamlined downloading and caching function.


``` r
file_tbl <- listMetagenomicData()
```


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-5b8d99b3c700c0e0fd0c" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-5b8d99b3c700c0e0fd0c">{"x":{"filter":"none","vertical":false,"extensions":["Responsive"],"data":[["1","2","3","4","5","6","7","8","9","10"],["709819be-d776-4750-80cb-c6b5373771c8","da3bd88f-c992-43cf-9cec-be30c5d0133d","ab409b4c-d010-429a-9bd4-c32f1d7505bd","eee80cf1-041f-4459-b5d4-42bb908b8749","367613f5-f3a3-4448-99da-4d204b93b422","d68b9685-0edb-4d59-86b6-fbabef9472a5","a6815adb-f91f-463c-a447-06257cbec849","3f19dd23-0fd7-4dbd-a97c-b65227fda53c","7d31dfde-d639-479a-b70e-3975485b2e5e","830b5be1-7458-45ad-8b19-2fe5ed9e6cf4"],["marker_abundance","pathcoverage_stratified","pathabundance_stratified","genefamilies_relab_stratified","marker_abundance","pathabundance_relab_unstratified","pathabundance_cpm_unstratified","pathabundance_cpm_unstratified","genefamilies","pathabundance_cpm_unstratified"],["results/cMDv4/709819be-d776-4750-80cb-c6b5373771c8/metaphlan_markers/marker_abundance.tsv.gz","results/cMDv4/da3bd88f-c992-43cf-9cec-be30c5d0133d/humann/out_pathcoverage_stratified.tsv.gz","results/cMDv4/ab409b4c-d010-429a-9bd4-c32f1d7505bd/humann/out_pathabundance_stratified.tsv.gz","results/cMDv4/eee80cf1-041f-4459-b5d4-42bb908b8749/humann/out_genefamilies_relab_stratified.tsv.gz","results/cMDv4/367613f5-f3a3-4448-99da-4d204b93b422/metaphlan_markers/marker_abundance.tsv.gz","results/cMDv4/d68b9685-0edb-4d59-86b6-fbabef9472a5/humann/out_pathabundance_relab_unstratified.tsv.gz","results/cMDv4/a6815adb-f91f-463c-a447-06257cbec849/humann/out_pathabundance_cpm_unstratified.tsv.gz","results/cMDv4/3f19dd23-0fd7-4dbd-a97c-b65227fda53c/humann/out_pathabundance_cpm_unstratified.tsv.gz","results/cMDv4/7d31dfde-d639-479a-b70e-3975485b2e5e/humann/out_genefamilies.tsv.gz","results/cMDv4/830b5be1-7458-45ad-8b19-2fe5ed9e6cf4/humann/out_pathabundance_cpm_unstratified.tsv.gz"],["450.4 Kb","31.2 Kb","12 Kb","2.4 Mb","148.7 Kb","5.5 Kb","4.5 Kb","7.9 Kb","5 Mb","5 Kb"],["2025-05-03T07:19:27Z","2025-05-06T00:37:08Z","2025-05-27T14:49:37Z","2025-05-29T07:31:26Z","2025-05-06T00:18:35Z","2025-05-06T00:36:44Z","2025-05-06T00:31:06Z","2025-05-06T00:19:26Z","2025-05-06T00:26:27Z","2025-05-26T22:51:38Z"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>UUID<\/th>\n      <th>data_type<\/th>\n      <th>gcb_object<\/th>\n      <th>size<\/th>\n      <th>updated<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"UUID","targets":1},{"name":"data_type","targets":2},{"name":"gcb_object","targets":3},{"name":"size","targets":4},{"name":"updated","targets":5}],"order":[],"autoWidth":false,"orderClasses":false,"responsive":true}},"evals":[],"jsHooks":[]}</script>
```


To access output files, use `cacheMetagenomicData`. This function takes UUID and
data type arguments, downloads the corresponding output files, and
stores them in a local cache. If the same files are requested again through this
function, they will not be re-downloaded unless explicitly specified, in order
to reduce excessive downloads. `cacheMetagenomicData` returns a tibble of cached
file paths and cache IDs for each requested file.


``` r
cache_tbl <- cacheMetagenomicData(uuids = selected_samples$uuid,
                                  data_type = "relative_abundance")
#| Resource with rname = 'results/cMDv4/0807eb2a-a15e-4647-8e19-2600d8fda378/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/e0fbb54f-0249-4917-a4d7-bd68acb89c62/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/25172837-2849-4db3-be91-d54d6a815d00/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/39ddb5e7-97f6-4d3c-812b-9653b03f99b3/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/7b152a7d-e244-4e2b-b924-7195c7ecfb10/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/dd30f93b-7999-47a4-93fb-21971b899939/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/1406666f-04a8-43c9-983b-4ed62fd6da4a/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/fe3de3ca-3a14-4bd8-ae1c-0dad69edc9cd/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/8707e374-5ddb-4220-8cbf-364b8b0e7be1/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/22848a9c-66a6-4993-9058-cb6464edb42f/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/08e2b754-78e2-4cb4-8ff2-95fd7b0ff44a/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/9baef0b2-93d2-4a40-8082-d357c7f8156a/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/09a9303d-d87d-4556-9672-04cbbcaf3d37/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/ac9f3532-90d8-412c-9c80-491037f0bcc2/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/eda61949-02dc-40ae-8dbe-bea2add85a52/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/1f007260-be6c-4a21-800a-ad9c36129a0d/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/e47a59bb-443a-405f-9c5d-02659d80e9e5/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/b3eaf3ab-43ef-4830-ab6d-12bafed3c61e/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/28f7352f-fe23-4003-93e1-41f4fedc6232/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/b07e2362-5851-4181-ba9a-15d9109ee4dd/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/677be4e3-722b-4e43-bd5a-36d8fbed6f86/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/0c817272-f873-475f-a401-dfe46a679a9f/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/7a3945d9-21bb-434a-9a4e-bfcdeb6194de/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
#| Resource with rname = 'results/cMDv4/56aa2ad5-007d-407c-a644-48aac1e9a8f0/metaphlan_lists/metaphlan_unknown_list.tsv.gz' found in cache, proceeding with most recent version.
```


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-817a65e017d834e96688" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-817a65e017d834e96688">{"x":{"filter":"none","vertical":false,"extensions":["Responsive"],"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"],["0807eb2a-a15e-4647-8e19-2600d8fda378","e0fbb54f-0249-4917-a4d7-bd68acb89c62","25172837-2849-4db3-be91-d54d6a815d00","39ddb5e7-97f6-4d3c-812b-9653b03f99b3","7b152a7d-e244-4e2b-b924-7195c7ecfb10","dd30f93b-7999-47a4-93fb-21971b899939","1406666f-04a8-43c9-983b-4ed62fd6da4a","fe3de3ca-3a14-4bd8-ae1c-0dad69edc9cd","8707e374-5ddb-4220-8cbf-364b8b0e7be1","22848a9c-66a6-4993-9058-cb6464edb42f","08e2b754-78e2-4cb4-8ff2-95fd7b0ff44a","9baef0b2-93d2-4a40-8082-d357c7f8156a","09a9303d-d87d-4556-9672-04cbbcaf3d37","ac9f3532-90d8-412c-9c80-491037f0bcc2","eda61949-02dc-40ae-8dbe-bea2add85a52","1f007260-be6c-4a21-800a-ad9c36129a0d","e47a59bb-443a-405f-9c5d-02659d80e9e5","b3eaf3ab-43ef-4830-ab6d-12bafed3c61e","28f7352f-fe23-4003-93e1-41f4fedc6232","b07e2362-5851-4181-ba9a-15d9109ee4dd","677be4e3-722b-4e43-bd5a-36d8fbed6f86","0c817272-f873-475f-a401-dfe46a679a9f","7a3945d9-21bb-434a-9a4e-bfcdeb6194de","56aa2ad5-007d-407c-a644-48aac1e9a8f0"],["relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance","relative_abundance"],["results/cMDv4/0807eb2a-a15e-4647-8e19-2600d8fda378/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/e0fbb54f-0249-4917-a4d7-bd68acb89c62/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/25172837-2849-4db3-be91-d54d6a815d00/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/39ddb5e7-97f6-4d3c-812b-9653b03f99b3/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/7b152a7d-e244-4e2b-b924-7195c7ecfb10/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/dd30f93b-7999-47a4-93fb-21971b899939/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/1406666f-04a8-43c9-983b-4ed62fd6da4a/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/fe3de3ca-3a14-4bd8-ae1c-0dad69edc9cd/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/8707e374-5ddb-4220-8cbf-364b8b0e7be1/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/22848a9c-66a6-4993-9058-cb6464edb42f/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/08e2b754-78e2-4cb4-8ff2-95fd7b0ff44a/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/9baef0b2-93d2-4a40-8082-d357c7f8156a/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/09a9303d-d87d-4556-9672-04cbbcaf3d37/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/ac9f3532-90d8-412c-9c80-491037f0bcc2/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/eda61949-02dc-40ae-8dbe-bea2add85a52/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/1f007260-be6c-4a21-800a-ad9c36129a0d/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/e47a59bb-443a-405f-9c5d-02659d80e9e5/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/b3eaf3ab-43ef-4830-ab6d-12bafed3c61e/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/28f7352f-fe23-4003-93e1-41f4fedc6232/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/b07e2362-5851-4181-ba9a-15d9109ee4dd/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/677be4e3-722b-4e43-bd5a-36d8fbed6f86/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/0c817272-f873-475f-a401-dfe46a679a9f/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/7a3945d9-21bb-434a-9a4e-bfcdeb6194de/metaphlan_lists/metaphlan_unknown_list.tsv.gz","results/cMDv4/56aa2ad5-007d-407c-a644-48aac1e9a8f0/metaphlan_lists/metaphlan_unknown_list.tsv.gz"],["BFC6786","BFC6787","BFC6788","BFC6789","BFC6790","BFC6791","BFC6792","BFC6793","BFC6794","BFC6795","BFC6796","BFC6797","BFC6798","BFC6799","BFC6800","BFC6801","BFC6802","BFC6803","BFC6804","BFC6805","BFC6806","BFC6807","BFC6808","BFC6809"],["/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed766d3d8b.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed316de998.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed2f25ff85.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32edb2aefc9.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed750e9261.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed758c30c8.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed40ac0d62.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed6a5fa104.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32edc5bde52.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed68392cbc.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed4a29651f.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed123174ed.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed2be3f7a0.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed62c58fc1.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed78c3747a.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed277d2f3b.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed5e5156eb.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed42514a5e.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed48310172.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed5f1f853b.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32edded4142.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed168269a.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed5feec164.tsv.gz","/home/kaelyn/.cache/R/parkinsonsMetagenomicData/a32ed1b6bca0f.tsv.gz"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>UUID<\/th>\n      <th>data_type<\/th>\n      <th>gcb_object<\/th>\n      <th>cache_id<\/th>\n      <th>cache_path<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"UUID","targets":1},{"name":"data_type","targets":2},{"name":"gcb_object","targets":3},{"name":"cache_id","targets":4},{"name":"cache_path","targets":5}],"order":[],"autoWidth":false,"orderClasses":false,"responsive":true}},"evals":[],"jsHooks":[]}</script>
```


## Data Handling

### Automatic Experiment Setup (MetaPhlAn and HUMAnN output files only)

The above table can then be supplied to `loadMetagenomicData` and the cached
files will be parsed into a single SummarizedExperiment object with sample
metadata. At this point, only the `metaphlan_lists` data types,
`viral_clusters` and `relative_abundance`, as well as all `humann` data_types,
are compatible with this function.


``` r
merged_experiment <- loadMetagenomicData(cache_tbl)
```


```
#| class: SummarizedExperiment 
#| dim: 2915 24 
#| metadata(0):
#| assays(1): relative_abundance
#| rownames(2915): UNCLASSIFIED k__Bacteria ...
#|   k__Bacteria|p__Firmicutes|c__CFGB3009|o__OFGB3009|f__FGB3009|g__GGB31234|s__GGB31234_SGB14869|t__SGB14869
#|   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Eubacteriales_unclassified|g__Eubacteriales_unclassified|s__Clostridiales_bacterium_CHKCI006|t__SGB7261
#| rowData names(2): ncbi_tax_id additional_species
#| colnames(24): 0807eb2a-a15e-4647-8e19-2600d8fda378
#|   e0fbb54f-0249-4917-a4d7-bd68acb89c62 ...
#|   7a3945d9-21bb-434a-9a4e-bfcdeb6194de
#|   56aa2ad5-007d-407c-a644-48aac1e9a8f0
#| colData names(521): study_name BioProject ... date_of_birth cage
```

### Stepwise Experiment Setup

Alternatively, we can parse the files, add metadata, and merge the
SummarizedExperiment objects in separate steps.

`parse_metaphlan_list` completes the first step for files with 'data_type' equal
to "relative_abundance" or "viral_clusters". `parse_humann` is also available
for HUMAnN output files.


``` r
parsed_rel_ab_list <- vector("list", nrow(cache_tbl))
names(parsed_rel_ab_list) <- cache_tbl$UUID

for (i in 1:nrow(cache_tbl)) {
    parsed_rel_ab_list[[i]] <- parse_metaphlan_list(sample_id = cache_tbl$UUID[i],
                                                file_path = cache_tbl$cache_path[i],
                                                data_type = cache_tbl$data_type[i])
    
}
```

Once the files have been loaded as SummarizedExperiment objects, matching assays
from multiple samples can be merged together with `mergeExperiments`.


``` r
merged_rel_abs <- mergeExperiments(parsed_rel_ab_list)
```


```
#| class: SummarizedExperiment 
#| dim: 2915 24 
#| metadata(0):
#| assays(1): relative_abundance
#| rownames(2915): UNCLASSIFIED k__Bacteria ...
#|   k__Bacteria|p__Firmicutes|c__CFGB3009|o__OFGB3009|f__FGB3009|g__GGB31234|s__GGB31234_SGB14869|t__SGB14869
#|   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Eubacteriales_unclassified|g__Eubacteriales_unclassified|s__Clostridiales_bacterium_CHKCI006|t__SGB7261
#| rowData names(2): ncbi_tax_id additional_species
#| colnames(24): 0807eb2a-a15e-4647-8e19-2600d8fda378
#|   e0fbb54f-0249-4917-a4d7-bd68acb89c62 ...
#|   7a3945d9-21bb-434a-9a4e-bfcdeb6194de
#|   56aa2ad5-007d-407c-a644-48aac1e9a8f0
#| colData names(0):
```

The corresponding metadata from sampleMetadata is then added as colData with
the function `add_metadata`.


``` r
rel_abs_with_metadata <- add_metadata(sample_ids = colnames(merged_rel_abs),
                                      id_col = "uuid",
                                      experiment = merged_rel_abs)
```


```
#| class: SummarizedExperiment 
#| dim: 2915 24 
#| metadata(0):
#| assays(1): relative_abundance
#| rownames(2915): UNCLASSIFIED k__Bacteria ...
#|   k__Bacteria|p__Firmicutes|c__CFGB3009|o__OFGB3009|f__FGB3009|g__GGB31234|s__GGB31234_SGB14869|t__SGB14869
#|   k__Bacteria|p__Firmicutes|c__Clostridia|o__Eubacteriales|f__Eubacteriales_unclassified|g__Eubacteriales_unclassified|s__Clostridiales_bacterium_CHKCI006|t__SGB7261
#| rowData names(2): ncbi_tax_id additional_species
#| colnames(24): 0807eb2a-a15e-4647-8e19-2600d8fda378
#|   e0fbb54f-0249-4917-a4d7-bd68acb89c62 ...
#|   7a3945d9-21bb-434a-9a4e-bfcdeb6194de
#|   56aa2ad5-007d-407c-a644-48aac1e9a8f0
#| colData names(521): study_name BioProject ... date_of_birth cage
```
