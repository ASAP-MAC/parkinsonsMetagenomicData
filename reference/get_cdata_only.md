# Return unique colData columns for a data type

'get_cdata_only' takes a data type and vector of UUIDs, filters the
relevant parquet file available in a provided database connection, and
returns a single row for each uuid containing only the data marked as
'cdata' in the 'se_role' column of 'parquet_colinfo()'.

## Usage

``` r
get_cdata_only(con, data_type, uuids)
```

## Arguments

- con:

  DuckDB connection object of class 'duckdb_connection'

- data_type:

  Single string: value found in the data_type' column of
  output_file_types() and also as part of the name of a view found in
  DBI::dbListTables(con), indicating which views to consider when
  collecting data.

- uuids:

  Character vector: UUIDs to return information for.

## Value

A data frame with a 'uuid' column as well as all columns marked as
'cdata' in the 'se_role' column of 'parquet_colinfo()'.

## See also

[`select`](https://dplyr.tidyverse.org/reference/select.html),
[`filter`](https://dplyr.tidyverse.org/reference/filter.html),
[`distinct`](https://dplyr.tidyverse.org/reference/distinct.html),
[`compute`](https://dplyr.tidyverse.org/reference/compute.html)
[`sym`](https://rlang.r-lib.org/reference/sym.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 con <- accessParquetData(data_types = "relative_abundance")
 uuids <- c("c3eb1e35-9a43-413d-8078-6a0a7ac064ba",
            "a82385f0-d1be-4d79-854c-a7fbfe4473e1",
            "a1444b37-d568-4575-a5c4-14c5eb2a5b89",
            "2a497dd7-b974-4f04-9e1f-16430c678f06",
            "496a2d0c-75ae-430f-b969-b15dedc16b3c",
            "4a786fd8-782f-4d43-937a-36b98e9c0ab6",
            "c789b8bc-ebe6-4b85-83e1-5cf1bbfa6111",
            "d311d028-a54b-4557-970c-eb5b77ec0050",
            "373a5ac4-161a-46c6-b7d8-4f28b854b386",
            "5a93179f-7ca7-41d8-96d7-dbed215894aa",
            "7a2e961b-2f13-4760-9392-c896c54e7ec3",
            "c6ecc460-33db-4032-9092-9148a134f5dc",
            "e4a83901-e130-4b64-9e7a-fea55bc5f3f2",
            "6a034c9f-f7c9-4ead-812b-123ee99b1e0b",
            "ee26b6f0-89fd-45d0-8af9-bc1d9647a700")
 get_cdata_only(con, data_type = "relative_abundance", uuids)
 }
} # }
```
