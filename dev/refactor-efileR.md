# `efileR` / `fiscal` refactor specification

Status: Phase 1 behavioral and package-boundary specification.

## Current implementation status

The initial extraction is now operational in the nested `efileR` package.
Implemented components include:

- Native-schema geocoded BMF retrieval and merging.
- Concordance-driven, form-aware data normalization.
- Filing deduplication.
- Panel classification, filtering, missing-year imputation, and smoothing.
- Arbitrary table-name resolution and built-in aliases.
- Retained, temporary, and virtual acquisition modes.
- Memory CSV and optional DuckDB read backends.
- Explicit-key merge planning, cardinality checks, collision handling, and
  table/join manifests.
- Single- and multi-year panel assembly.

`fiscal` now imports `efileR` and delegates its BMF, retrieval, panel-building,
deduplication, classification, filtering, imputation, and smoothing entry
points. Statistical normalization and accounting metrics remain in `fiscal`.

## 1. Dependency direction

The packages must form a one-way dependency:

```text
efileR  <-  fiscal
```

`efileR` must not import or suggest `fiscal`. It owns acquisition and general
efile/panel preparation. `fiscal` imports `efileR` and owns nonprofit financial
interpretation, accounting metrics, and fiscal-health scores.

## 2. Function ownership and migration

### Move to `efileR`

| Current export | Proposed `efileR` API | Migration treatment |
|---|---|---|
| `retrieve_efile_data()` | `efile_retrieve()` | Keep a deprecated wrapper in `fiscal` |
| `efile_tables()` | `efile_tables()` | Keep a deprecated wrapper in `fiscal` |
| `get_panel()` | `efile_panel()` | Keep a deprecated wrapper in `fiscal` |
| `merge_bmf()` | `efile_merge_bmf()` | Keep a deprecated wrapper in `fiscal` |
| `inspect_duplicates()` | `efile_inspect_duplicates()` | Keep a deprecated wrapper |
| `deduplicate()` | `efile_deduplicate()` | Keep a deprecated wrapper |
| `panel_composition()` | `efile_panel_classify()` | Keep a deprecated wrapper |
| `panel_summary()` | `efile_panel_summary()` | Keep a deprecated wrapper |
| `panel_filter_types()` | `efile_panel_filter()` | Keep a deprecated wrapper |
| `panel_impute()` | `efile_panel_impute()` | Keep a deprecated wrapper |
| `panel_smooth()` | `efile_panel_smooth()` | Keep a deprecated wrapper |
| NTEE/BMF helpers | Same names initially | Move because acquisition uses them |
| Schema-driven data normalization | `efile_normalize_data()` | Interpret source encodings and form/table scope |

The first `efileR` release should retain familiar argument names where they are
not ambiguous. Renaming should occur through wrappers rather than forcing a
simultaneous package move and API rewrite.

### Remain in `fiscal`

- All individual `get_*_ratio()` functions.
- `compute_all()` and `compute_all_panel()`.
- Distributional normalization and winsorization: `normalize_x()`,
  `find_best_normalization()`, `apply_normalization()`,
  `apply_transformations()`, and their helpers. These produce the fiscal
  `_w`, `_z`, and `_p` metric variants.
- Fiscal metric metadata, weights, and scoring.

### Split or reconsider

- `detect_ez_rows()`, `impute_zero()`, and `sanitize_financials()` currently
  mix efile data normalization with fiscal calculation preparation. Extract
  schema-driven encoding interpretation and form-scope rules to `efileR`;
  retain only metric-specific validation or derived-variable handling in
  `fiscal`.
- Financial field sets (`get_pc_fields()`, `get_pz_fields()`) currently serve
  both table semantics and accounting calculations. Source/form availability
  belongs in the `efileR` concordance; metric input registries remain in
  `fiscal`.
- `get_idvars()` currently combines filing identifiers with fiscal output
  selection. `efileR` should expose schema keys; `fiscal` should maintain its
  own list of columns retained with metric output.

## 3. Proposed public `efileR` API

### Acquisition primitives

```r
efile_tables(cardinality = "all", source = efile_source_nccs())

efile_download(
  years,
  tables,
  path = "efdata",
  retain = TRUE,
  overwrite = FALSE,
  ...
)

efile_retrieve(
  years,
  tables,
  backend = c("memory", "duckdb"),
  cache = c("retain", "temporary", "none"),
  path = "efdata",
  filters = NULL,
  columns = NULL,
  join_1xm = FALSE,
  keys = NULL,
  collect = TRUE,
  ...
)

efile_panel(...)
```

`efile_download()` only acquires files. `efile_retrieve()` reads and merges
tables. `efile_panel()` is the multi-year convenience orchestration layer.

### Panel operations

```r
efile_deduplicate(data, schema = efile_schema_irs990(), ...)
efile_panel_classify(data, id, year, ...)
efile_panel_filter(data, classification, keep, ...)
efile_panel_impute(data, id, year, variables, ...)
efile_panel_smooth(data, id, year, variables, ...)
efile_normalize_data(data, concordance, form, audit = TRUE, ...)
```

Generic functions must accept explicit `id`, `year`, and field mappings.
IRS-friendly defaults should be supplied through `efile_schema_irs990()`.

## 3.1 Data normalization and operation order

In the public API, **data normalization** means interpreting source encodings,
including blank cells, checkbox values, and form scope. **Imputation** is
reserved for inserting and filling missing panel years. Data normalization is
also distinct from the **statistical normalization** of derived financial
metrics performed by `fiscal`.

A blank cell is not assigned one universal meaning. Concordance metadata should
classify each field/form combination as one of at least:

- `implicit_zero`: a blank financial field means zero when the field is in
  scope for that return type.
- `implicit_false`: a blank checkbox means no/false when in scope.
- `unknown`: a blank means the value was not reported or cannot be inferred.
- `not_applicable`: the field is inapplicable for this filing.
- `out_of_scope`: the field does not exist on the filed form or schedule.
- `literal_missing`: preserve the source blank without normalization.

Rules are evaluated by table, field, form/return type, year/version, and when
needed schedule presence. For example, a blank full-990 financial field may be
recoded to zero for a 990 filing but must remain out-of-scope—not zero—for a
990EZ filing that does not contain that field.

The processing order is:

1. Parse raw source values and preserve raw missingness.
2. Identify filing form and applicable table/field scope.
3. Apply concordance-driven data normalization (zero/false only in scope).
4. Deduplicate filings and construct the panel.
5. Classify panel coverage and gaps.
6. Apply explicitly requested longitudinal imputation.
7. Smooth panel values where requested.
8. Pass prepared financial fields to `fiscal` for ratios, winsorization,
   statistical normalization, and scoring.

Data normalization should optionally add or return an audit record containing
field, rule, form, affected row count, and reason. Raw blanks, structural
out-of-scope values, and panel-imputed values must remain distinguishable.

## 4. Source, table, and schema metadata

Hard-coded table names should be represented by a source catalog with at least:

| Field | Meaning |
|---|---|
| `table` | Canonical table name |
| `alias` | Optional short name such as `P00` |
| `form` | Form or schedule family |
| `cardinality` | `1:1`, `1:M`, or `supplemental` |
| `keys` | Ordered join-key candidates |
| `row_keys` | Keys that identify a repeated row in 1:M tables |
| `year_min`, `year_max` | Known availability range when available |
| `url_template` | Resource location template |

Table inputs must support:

1. Known aliases.
2. Known canonical catalog names.
3. Literal canonical names not yet in the catalog, with a warning.
4. Complete user-provided URLs, with explicit keys/cardinality when merging.

Unknown tables may be downloaded without metadata. They must not be merged
implicitly when a safe key cannot be determined.

The default filing-level key is a catalog/schema property, not every shared
column. Before joining, the engine must report duplicate key counts and reject
an unexpected many-to-many join unless the user explicitly permits it.

## 5. Backend contract

Both backends consume the same resolved request and merge plan.

### Memory backend

- Download or reuse local CSV files.
- Read with `data.table::fread()`.
- Apply projection and filtering as early as practical.
- Merge with explicit keys and validate cardinality.

### DuckDB backend

- Query remote CSVs or retained local CSVs with DuckDB.
- Push EIN, year, and column filters into the scan.
- Use the same explicit keys and cardinality checks as the memory backend.
- Do not return a lazy object tied to a temporary, closed connection.
- With `collect = FALSE`, return a durable DuckDB file/table or another object
  that owns and documents its connection lifecycle.
- Treat `httpfs` setup failure as a structured backend error.

For identical inputs, ordering aside, collected memory and DuckDB results must
have equivalent rows, columns, and missing values.

## 6. Result and logging contract

Retrieval should return an `efile_result` object with stable components rather
than storing all operational information only as data-frame attributes:

```r
list(
  data = data.frame_or_lazy_reference,
  manifest = table_year_manifest,
  joins = join_manifest,
  run = run_metadata,
  log_file = path
)
```

Convenience methods should include `collect()`, `as.data.frame()`, `print()`,
and `efile_manifest()`.

The table-year manifest should contain:

- Requested and resolved table and year.
- URL and local path.
- Availability and final status.
- Cache action: downloaded, reused, temporary, or removed.
- Attempt count, elapsed time, and file bytes when known.
- Source rows/columns and selected rows/columns.
- Duplicate key count.
- Warning/error class and message.

The join manifest should contain:

- Left and right resources.
- Keys and expected/observed cardinality.
- Rows before and after the join.
- Unmatched left/right key counts.
- Non-key name collisions and their resolution.

The run record should contain package version, source configuration, backend,
arguments, timestamps, final dimensions, requested/retrieved years, and paths.
Console output and the human-readable receipt must be rendered from these same
records so they cannot disagree.

## 7. Panel classification contract

The implemented classifier is already more expressive than its older public
documentation. It records two independent dimensions:

- Boundary coverage (`panel_type`): `full`, `entry`, `exit`, or `interior`.
- Spell coverage (`panel_spell_balance`): `contiguous` or `fragmented`.

It also records the first/last observed year, number of observed waves, number
of internal gaps, and maximum gap size. `efileR` should retain this
two-dimensional model. All public functions use these labels directly; no
parallel compatibility vocabulary is maintained.

Phase 1 identified three inconsistencies to resolve before extraction:

1. `panel_filter_types()` previously expected an older one-dimensional `group`
   column. It has been updated to filter `panel_type` and
   `panel_spell_balance` independently.
2. `panel_composition()` refers to an undefined `return_classification`
   argument when `append_classification = FALSE`.
3. Some documentation still describes the older one-dimensional taxonomy.

These should be corrected in `fiscal` and covered by tests before moving the
implementation.

## 8. `fiscal` scoring API

```r
fiscal_metrics()  # metric registry: field, category, direction, coverage rules
fiscal_weights()  # construct/validate a named weighting specification
fiscal_score()    # apply standardized metrics and weights
```

Scores must retain their metric registry, weight specification, normalization
method, missing-data rule, and minimum coverage threshold as metadata or a
companion audit table.

## 9. Compatibility policy

1. Extract implementations without changing numerical behavior.
2. Add `fiscal` wrappers that call `efileR` and use lifecycle deprecation
   messages.
3. Maintain wrappers for at least one minor release.
4. Document changed defaults separately from package relocation.
5. Release `efileR` before releasing the dependent `fiscal` version.

## 10. Verification gates

### Before extraction

- Current unit tests pass.
- Local contract tests cover table cardinality and panel taxonomy.
- Retrieval fixtures cover missing tables, retries, schema drift, duplicate
  filings, and 1:M data without accessing the network.

### Before enabling DuckDB

- Memory and DuckDB fixture results are equivalent.
- Filter pushdown produces the same subset as post-read filtering.
- Connections and temporary files are cleaned up on success and error.
- SQL identifiers, strings, paths, and EIN values are safely quoted.

### Before removing implementations from `fiscal`

- Compatibility wrapper tests run against the installed `efileR` package.
- Reverse dependency examples and vignettes use the new API.
- `R CMD check` passes independently for both packages and jointly in a clean
  library.

## 11. Implementation sequence

1. Add contract fixtures/tests to `fiscal` (Phase 1).
2. Create the sibling `efileR` package skeleton and result/schema classes.
3. Extract data normalization, panel classification, filtering, deduplication,
   and panel imputation.
4. Refactor current CSV acquisition into the common request/manifest engine.
5. Add DuckDB as an optional backend.
6. Replace moved `fiscal` implementations with compatibility wrappers.
7. Add the metric registry, weighting specification, and score calculation.
8. Publish migration documentation and release in dependency order.
