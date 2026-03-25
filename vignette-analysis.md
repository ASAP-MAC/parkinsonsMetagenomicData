# Vignette Analysis: Clarity, Cohesiveness, and Completeness

## Executive Summary

After enhancing `first-15-minutes.Rmd` with complete data discovery workflows, the other three vignettes now have consistency gaps and missing discovery patterns. The main issues are:

1. **Discovery workflow inconsistency**: Other vignettes lack the data type and reference table discovery sections now present in first-15-minutes
2. **Limited example diversity**: Only taxonomic examples shown; no examples of stratified/unstratified pathways, gene families, or viral clusters. 
3. **Scope issues**: "Common Issues" is too narrow; "Full Workflow" doesn't show anything more "full" than the updated first-15-minutes
4. **UI inconsistency**: Inconsistent use of DT datatables across vignettes

---

## Full Workflow (`full-workflow.Rmd`)

### Implementation Status: ✅ COMPLETE

**Changes Made**:
1. Added "Exploring Data Types" subsection with searchable DT datatable
2. Added "Additional Data Type Examples" section with 3 new examples:
   - Pathway Abundance (stratified vs unstratified)
   - Viral Clusters
   - Gene Family Abundance
3. Added comparison table summarizing all 4 data types
4. Enhanced search guidance with specific keywords
5. **Replaced pseudocode with working local files example** using inst/extdata files:
   - Uses actual parquet files included in package (pathcoverage_unstratified)
   - Demonstrates system.file() pattern for locating package data
   - Shows complete executable workflow (not eval=interactive())
   - Users can actually run this code
6. **Removed developer-focused "Parquet Creation" section** (moved to README.md)
7. **Enhanced parquet_colinfo() explanation** with new "Understanding Reference Table Structure" subsection

**Result**: Vignette now demonstrates diverse data types and clearly differentiates from first-15-minutes by showing:
- More data types (4 vs 2)
- Deeper explanations (stratified/unstratified, viral genomes, gene families)
- Performance considerations for each type
- More comprehensive reference table usage
- All documentation gaps addressed

### Current Structure
1. Parquet File Overview and Setup
   - Hugging Face background
   - Parquet creation (developer-focused)
   - DuckDB in R
2. Standard Workflow
   - File Selection
   - Sample Table
   - Feature Table
   - returnSamples()

### Strengths
- ✅ Clear explanation of returnSamples() arguments
- ✅ Shows both remote and local file usage
- ✅ Demonstrates dry_run functionality
- ✅ Uses DT datatables consistently
- ✅ Good background information

### Issues

#### 1. Missing Discovery Workflow (Critical) - ✅ FIXED
**Problem**: Doesn't show how to discover available data types before choosing one.

**Solution Implemented**: Added "Exploring Data Types" subsection after line 127 that:
- Shows `get_hf_parquet_urls()` call with data saved to `available_data`
- Displays data types in searchable DT datatable with `data_type`, `tool`, `description`, and `units_normalization` columns
- Includes helpful search tips (search "pathway", "stratified", "viral", "genefamilies")
- Provides better context for choosing data types

**Impact**: Users now have a clear discovery workflow consistent with first-15-minutes.

#### 2. Vignette Identity Crisis - ✅ PARTIALLY FIXED
**Problem**: The vignette guide table says this is for "full control over sample, feature, and query setup" but the updated first-15-minutes now shows similar functionality.

**Solution Implemented**: Added "Additional Data Type Examples" section with:
- **Example 2: Pathway Abundance** (Stratified vs Unstratified)
  - Shows both unstratified and stratified pathway retrieval
  - Explains the difference between aggregate and species-level data
  - Demonstrates filtering for specific pathways
- **Example 3: Viral Clusters**
  - Shows genome_name_ref reference usage
  - Retrieves viral cluster data from MetaPhlAn
- **Example 4: Gene Family Abundance**
  - Shows gene_family_ref reference usage
  - Demonstrates HUMAnN gene family data retrieval
  - Includes performance considerations for large queries
- Comparison table summarizing all data types

**What makes this "full"**: Now clearly emphasizes:
- ✅ Multiple data types in one vignette (4 examples vs 2 in first-15-minutes)
- ✅ More detailed explanations of each data type
- ✅ Stratified vs unstratified concept explained
- ✅ Performance considerations for different data types
- ✅ Comparison across data types
- Still shows: local file usage, dry_run functionality, detailed argument explanations

**Remaining differentiation from first-15-minutes**:
- More comprehensive coverage (4 data types vs 2)
- Deeper explanation of concepts (stratified/unstratified, viral, gene families)
- Performance guidance for each data type
- More detailed background information about tools and infrastructure

#### 3. Developer Content in User Vignette - ✅ FIXED
**Problem**: The "Parquet Creation" section (lines 36-45) discussed:
- Google Bucket credentials
- parquet_generation repo
- How parquet files are created

**Why it was problematic**: This is package developer documentation, not end-user workflow documentation. Users don't need to know how to create parquet files.

**Solution Implemented**: Section removed entirely (moved to README.md by user).

#### 4. Only Taxonomic Example - ✅ FIXED
**Problem**: Shows only Faecalibacterium genus example. No pathway or functional data.

**Solution Implemented**: Added "Additional Data Type Examples" section with three new complete examples:
1. **Pathway Abundance**: Butyrate biosynthesis pathways (unstratified and stratified)
2. **Viral Clusters**: Viral genome detection from MetaPhlAn
3. **Gene Family Abundance**: UniRef gene family quantification from HUMAnN

Each example includes:
- Reference table discovery (get_ref_info(), load_ref())
- Feature filtering specific to that data type
- Complete returnSamples() workflow
- Data type-specific considerations

**Impact**: Users now see diverse examples covering MetaPhlAn taxonomic + viral data and HUMAnN functional data (pathways + gene families).

#### 5. Incomplete parquet_colinfo() Demonstration - ✅ FIXED
**Problem**: Showed `parquet_colinfo("relative_abundance")` to see columns, but didn't explain:
- Why this matters
- How it relates to reference files
- What to do with this information

**Solution Implemented**: Added "Understanding Reference Table Structure" subsection that explains:
- How parquet_colinfo() helps with filtering features
- How it helps understand data structure
- How it helps verify correct reference table selection
- Made the output more actionable for users

### Recommendations for Full Workflow

**Priority 1 (Required for consistency):**
1. Add data type discovery section with `get_hf_parquet_urls()` and DT table
2. Add reference table discovery with `get_ref_info()` and DT table
3. Add a functional pathway example (in addition to taxonomic)

**Priority 2 (Improve vignette identity):**
1. Remove or minimize "Parquet Creation" section
2. Better articulate what makes this "full" vs first-15-minutes:
   - Show retrieving MULTIPLE data types
   - Show combining data from different sources
   - Show more advanced filtering patterns
3. Add section on query optimization strategies

**Priority 3 (Polish):**
1. Add cross-references to other vignettes at decision points
2. Add troubleshooting callouts referencing common-issues vignette

---

## Piecewise Workflow (`piecewise-workflow.Rmd`)

### Current Structure
1. Introduction (when to use this)
2. File Selection
3. accessParquetData()
4. Selecting Samples
5. Selecting Features (as named lists)
6. loadParquetData()
7. Optional View Customization

### Strengths
- ✅ Clear explanation of when to use this approach
- ✅ Good demonstration of accessParquetData() + loadParquetData() pattern
- ✅ Excellent custom view section with dplyr manipulation
- ✅ Shows the lower-level API clearly

### Issues

#### 1. No Discovery Workflow (Critical)
**Problem**: Says "We can use the same resources of `get_repo_info()` and `get_hf_parquet_urls()`" but doesn't actually SHOW calling them.

**Current**: References these functions but expects users to know from other vignettes.

**Should have**: Actual calls with output/tables showing what's available.

#### 2. Inconsistent Output Display
**Problem**: Some code chunks use `echo=FALSE` to show output, others use explicit print.

**Lines 86-88**: Uses `echo=FALSE` with just `sample_table`
**Lines 109-111**: Uses `echo=FALSE` with just `feature_table`

**Issue**: No DT datatables like other vignettes, just plain tibble output.

**Recommendation**: Use DT datatables for consistency with other vignettes.

#### 3. Only Taxonomic Example
**Problem**: Same Faecalibacterium example used everywhere.

**Impact**: Users working with pathways, gene families, etc. won't see relevant examples.

**Recommendation**: Add a second example with functional data to show the pattern applies broadly.

#### 4. Weak Motivation Section
**Problem**: Lines 25-28 say "when you need finer control" but don't clearly articulate WHAT control you get.

**Should explain:**
- When would you need accessParquetData vs returnSamples?
- What can you do with the connection object that you can't do with returnSamples?
- Performance implications
- Memory management considerations

#### 5. Custom View Section Underutilized
**Problem**: The custom view section (lines 159-197) is the MOST VALUABLE part showing true "piecewise" power, but it's buried at the end and given minimal explanation.

**Should expand:**
- Multiple custom view examples
- Joining views
- Aggregations before loading
- Complex dplyr chains
- When this is better than returnSamples filtering

### Recommendations for Piecewise Workflow

**Priority 1 (Required for consistency):**
1. Add actual get_hf_parquet_urls() call with DT table
2. Add actual get_ref_info() call with DT table
3. Use DT datatables for sample_table and feature_table display

**Priority 2 (Strengthen vignette identity):**
1. Expand introduction: clearly articulate when/why to use this approach
2. Move custom view section earlier and expand it significantly
3. Add multiple custom view examples showing power of the approach
4. Show performance comparison: when piecewise is faster/better

**Priority 3 (Add examples):**
1. Add functional pathway example
2. Show cross-data-type query (accessing multiple data types in one connection)
3. Show memory-efficient patterns for large queries

---

## Common Issues (`common-issues.Rmd`)

### Current Structure
1. Large File Considerations
   - Rate limits (HTTP 429)
   - Result size
   - Local file strategy
2. Example large queries with genefamilies_stratified

### Strengths
- ✅ Addresses a real pain point (rate limits with large files)
- ✅ Good practical examples with genefamilies_stratified
- ✅ Clear explanation of sorted vs non-sorted column filtering
- ✅ Shows strategic filtering approach

### Issues

#### 1. Title vs Content Mismatch (Critical)
**Problem**: Title "Common Issues" suggests broad troubleshooting coverage.

**Current content**: Only covers large file queries and rate limits.

**Missing common issues:**
- Connection errors and troubleshooting
- Memory exhausted errors
- Slow query performance (not due to rate limits)
- Empty results troubleshooting
- Data type mismatch errors
- Invalid filter errors
- include_empty_samples behavior confusion

#### 2. No Discovery Workflow
**Problem**: Even for troubleshooting, users need to understand what they're working with.

**Should have:**
- How to check if a query will be large before running
- How to estimate result size
- How to use dry_run to preview queries
- How to use parquet_colinfo() to understand data structure

#### 3. Narrow Scope
**Problem**: Only 132 lines total. The most important troubleshooting vignette is the shortest.

**Should expand to cover:**

**Connection Issues:**
- "Error: Failed to connect to repo"
- Network timeouts
- Authentication issues (if applicable)
- How to test connection

**Query Issues:**
- "0 rows returned" troubleshooting
- Unexpected NA values
- Feature names not found
- UUID not found

**Performance Issues:**
- Query hanging (not just rate limits)
- Memory issues
- When to download locally vs query remotely
- Optimal query patterns

**Debugging:**
- Using dry_run effectively
- Reading SQL output
- Understanding DuckDB error messages
- Using parquet_colinfo() for debugging

#### 4. No DT Datatables
**Problem**: Could show example outputs in searchable tables for consistency.

#### 5. No Cross-References
**Problem**: Doesn't link to relevant sections in other vignettes.

**Should have:**
- "For basic queries, see First 15 Minutes"
- "For custom filtering, see Piecewise Workflow"
- "For reference table issues, see Full Workflow"

### Recommendations for Common Issues

**Priority 1 (Expand scope):**
1. Add connection troubleshooting section
2. Add query debugging section
3. Add memory/performance section
4. Add "empty results" troubleshooting
5. Add error message interpretation guide

**Priority 2 (Improve structure):**
1. Reorganize into clear sections:
   - Connection Issues
   - Query Debugging
   - Performance and Rate Limits
   - Data Type and Feature Issues
   - Memory Management
2. Add TOC links or clear headings for quick navigation

**Priority 3 (Add tools):**
1. Show how to use dry_run for debugging
2. Show how to estimate query size
3. Add decision tree: "When should I download locally?"
4. Add cross-references to other vignettes

**Alternative**: Consider renaming to "Troubleshooting and Large Files" or split into two vignettes:
- "Troubleshooting Guide"
- "Working with Large Data Types"

---

## Cross-Vignette Consistency Issues

### 1. Discovery Pattern Inconsistency
**Problem**: first-15-minutes now has complete discovery workflow, others don't.

**Pattern that should be consistent:**
```r
# Discover data types
available_data <- get_hf_parquet_urls()
# Show in DT datatable with filters

# Discover reference tables
ref_info <- get_ref_info()
# Show in DT datatable with filters
```

**Status:**
- ✅ first-15-minutes: Has both with DT tables
- ❌ full-workflow: Shows get_hf_parquet_urls but no DT for discovery; shows get_ref_info with DT but buries it
- ❌ piecewise-workflow: References but doesn't show
- ❌ common-issues: Doesn't show

### 2. Example Data Type Inconsistency
**Problem**: All vignettes use the SAME example (Faecalibacterium + relative_abundance).

**Impact**:
- Monotonous
- Users working with pathways/gene families don't see examples
- Doesn't demonstrate package breadth

**Recommendation**:
- first-15-minutes: Keep both (taxonomic + pathway) ✅ Already done
- full-workflow: Add pathway example
- piecewise-workflow: Use pathway or gene family example instead
- common-issues: Keep gene families (appropriate for large file demo) ✅ Already good

### 3. DT Datatable Inconsistency
**Problem**: Inconsistent use of interactive tables.

**Status:**
- ✅ first-15-minutes: Uses DT consistently
- ✅ full-workflow: Uses DT consistently
- ❌ piecewise-workflow: No DT, uses plain tibble output
- ⚠️ common-issues: Doesn't show table outputs (but less relevant here)

### 4. Vignette Progression Unclear
**Problem**: Vignette guide table in first-15-minutes says:

| Vignette | Audience | Use when |
|---|---|---|
| First 15 Minutes | New users | You want your first successful data retrieval quickly |
| Full Workflow | Intermediate/advanced | You want full control over sample, feature, and query setup |

**But**: first-15-minutes NOW shows:
- Complete sample control (filtering sampleMetadata)
- Complete feature control (filtering reference tables)
- Complete query setup (returnSamples with all args)
- PLUS discovery that full-workflow lacks

**What's "full" about Full Workflow?** Needs clarity.

**Recommendation**:
- Full Workflow should show: multiple data types, cross-data-type analysis, advanced filtering, query optimization
- OR: Rename/restructure to emphasize different aspects

### 5. Missing Use Case Guidance
**Problem**: Users might not know which vignette to use.

**Should add** (in each vignette or in a shared intro):
- "Use first-15-minutes when: [specific scenarios]"
- "Use full-workflow when: [specific scenarios]"
- "Use piecewise-workflow when: [specific scenarios]"
- "Use common-issues when: [specific scenarios]"

---

## Priority Recommendations

### High Priority (Required for Consistency)

1. **Add discovery sections to all vignettes**
   - get_hf_parquet_urls() with DT table
   - get_ref_info() with DT table
   - Make this pattern consistent

2. **Add functional data examples**
   - full-workflow: Add pathway example
   - piecewise-workflow: Add pathway or gene family example

3. **Expand common-issues vignette**
   - Add connection troubleshooting
   - Add query debugging section
   - Add empty results section

### Medium Priority (Improve Quality)

4. **Strengthen vignette identities**
   - full-workflow: Show what makes it "full"
   - piecewise-workflow: Better explain when/why to use
   - common-issues: Expand scope or retitle

5. **Add DT datatables to piecewise-workflow**
   - Consistent with other vignettes
   - Better user experience

6. **Remove/minimize developer content**
   - full-workflow: Parquet Creation section

### Low Priority (Polish)

7. **Add cross-references**
   - Link between vignettes at decision points
   - "See X vignette for more on Y"

8. **Add use case guidance**
   - Clear decision trees for which vignette to use
   - Move beyond time estimates to scenario-based

9. **Standardize code chunk patterns**
   - Consistent eval=interactive() usage
   - Consistent echo=FALSE for display-only chunks

---

## Specific Code Changes Needed

### Full Workflow

**Add after line 128 (after parquet URLs table):**
```r
## Discovering Available Data Types

Before selecting files, explore what data types are available:

```{r explore_data_types}
available_data <- get_hf_parquet_urls()
```

```{r show_data_types, echo=FALSE}
available_data |>
    select(data_type, tool, description, units_normalization) |>
    distinct() |>
    datatable(
        options = list(pageLength = 10),
        filter = 'top',
        caption = "Available data types"
    )
```

Use the search boxes to filter by keywords like "pathway", "abundance", or specific tools.
```

**Add after Feature Table section (after line 208):**
```r
## Example 2: Pathway Abundance Data

Here's a second example using functional pathway data instead of taxonomic:

[Add pathway example similar to first-15-minutes]
```

### Piecewise Workflow

**Add after line 42 (after file selection intro):**
```r
## Discovering Available Data

First, see what's available:

```{r show_available_data}
get_hf_parquet_urls() |>
    select(data_type, tool, description) |>
    distinct() |>
    datatable(filter = 'top')
```

```{r show_ref_tables}
get_ref_info() |>
    datatable(filter = 'top')
```
```

**Replace plain output at lines 86-88 and 109-111 with DT datatables**

### Common Issues

**Expand to include new sections:**
- Connection Troubleshooting (new)
- Query Debugging (new)
- Empty Results (new)
- Large File Considerations (existing, keep)

---

## Conclusion

The vignettes are generally well-written but need consistency updates following the improvements to first-15-minutes. The main gaps are:

1. **Discovery workflows**: Must be added to all vignettes
2. **Example diversity**: Need functional data examples throughout
3. **Common Issues scope**: Too narrow, needs expansion
4. **Vignette identity**: "Full Workflow" needs clearer differentiation

These changes will create a cohesive vignette suite that progressively guides users from basic to advanced usage.
