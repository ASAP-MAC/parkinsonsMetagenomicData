# https://huggingface.co/datasets/waldronlab/metagenomics_mac

library(DBI)
library(duckdb)
library(duckplyr)

con <-
    dbConnect(
        duckdb(
            dbdir = "curatedMetagenomicData.duckdb"
        )
    )

dbExecute(con, "INSTALL httpfs")

dbExecute(con, "CREATE VIEW train AS (SELECT * FROM read_parquet('hf://datasets/waldronlab/metagenomics_mac@~parquet/default/train/*.parquet'));")
dbExecute(con, "CREATE VIEW pc_un AS (SELECT * FROM read_parquet('hf://datasets/waldronlab/metagenomics_mac/pathcoverage_unstratified.parquet'));")

dbListTables(con)

tbl(con, "train") |>
    colnames()

tbl(con, "train") |>
    head(10L)

tbl(con, "train") |>
    head(10L) |>
    collect()

tbl(con, "train") |>
    filter(`# Gene Family` == "UNMAPPED") |>
    collect()

tbl(con, "pc_un") |>
    colnames()

tbl(con, "pc_un") |>
    filter(`# Pathway` == "PWY-5686: UMP biosynthesis I") |>
    head(10L) |>
    collect()
