# 0001. Adopt CMD v4 schema and shared infrastructure

- **Status:** Accepted
- **Date:** 2026-06-17
- **Deciders:** Levi Waldron, Sean Davis, Sehyun Oh, cMD/pMD Team

## Context

`parkinsonsMetagenomicData` (pMD) and `curatedMetagenomicData` (cMD) have diverged but share significant code and pipeline overlap. Both use the same upstream Nextflow pipeline. However, pMD has its own custom data extraction logic (`parquet_generation`) and a distinct metadata schema.

cMD v4.0.0 introduced a significant cleanup and improvement over older metadata schemas, featuring rigorous ontology integration (EFO, NCIT, UBERON) via OLS and explicit unit specifications. Meanwhile, approximately 70% of the core software logic (DuckDB connections, data assembly) is duplicated between pMD and cMD.

## Decision

We will adopt the cMD v4.0.0 schema structure as the foundation for pMD to gain its ontology-backed validation. This establishes a unified schema structure across the ecosystem.

We will also extract the shared backend data access logic into a separate package (`curatedCore`) that pMD will depend on, effectively replacing pMD's internal DuckDB and dataset assembly logic with the shared infrastructure.

Finally, we will archive the custom `parquet_generation` prototype for pMD and migrate pMD's configurations to `curatedMetagenomicDataETL` to centralize all data transformation logic.

## Alternatives considered

- **Option A** — Keep pMD as a completely independent codebase. We rejected this because it duplicates maintenance effort for the DuckDB integration and TreeSummarizedExperiment assembly logic.
- **Option B** — Build pMD as an extension inside cMD. We rejected this because pMD contains Parkinson's-specific uncurated metadata and documentation that would clutter the general-purpose cMD package.

## Consequences

- **Easier:** Future improvements to the DuckDB data access layer or the ETL pipeline will automatically benefit pMD.
- **Easier:** pMD metadata will be standardized, enabling straightforward cross-study analyses with cMD datasets using controlled vocabularies.
- **Cost:** Requires a one-time intensive refactoring of the pMD data access layer to wire it up to `curatedCore`.
- **Cost:** Requires migrating pMD's data transformation logic to `curatedMetagenomicDataETL` and deprecating `parquet_generation`.

## References

- [curatedMetagenomicData.wiki/UNIFICATION_DECISIONS.md](https://github.com/waldronlab/curatedMetagenomicData/wiki/UNIFICATION_DECISIONS)
- [curatedMetagenomicData.wiki/UNIFICATION_PLAN.md](https://github.com/waldronlab/curatedMetagenomicData/wiki/UNIFICATION_PLAN)
