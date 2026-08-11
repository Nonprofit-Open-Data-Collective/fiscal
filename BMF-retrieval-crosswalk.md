# BMF retrieval and field crosswalk draft

Source CSV:
`https://nccsdata.s3.us-east-1.amazonaws.com/geocoding/bmf-master/merged/bmf_master_geocoded.csv`

Data dictionary: `unified_bmf_geocoded_data_dictionary.csv` (106 fields).

## Recommended retrieval steps

1. Resolve the BMF source as either the default URL or a user-supplied URL/local
   file. Store the default URL once as a package constant.
2. Download the CSV into the shared BMF cache when retention is enabled. Record
   URL, local path, bytes, timestamp, attempts, and cache status in the run
   manifest.
3. Read and validate the header before loading the full data. Require `EIN2` and
   report absent requested fields explicitly rather than silently dropping
   them with `intersect()`.
4. Select the merge key plus requested native fields as early as the backend
   permits. For a panel merge, optionally filter the BMF scan to the panel's
   distinct `EIN2` values.
5. Treat all EIN representations as character. Validate `EIN2` against the
   expected `EIN-XX-XXXXXXX` form; do not reconstruct it from lossy `ein_raw`.
6. Check uniqueness of `EIN2`. The merged master is designed to contain a
   surviving record, but duplicate handling should remain defensive:
   - Prefer the greatest `bmf_vintage_ym`.
   - Then prefer `bmf_source == "current"` over `"legacy"` when tied.
   - Use deterministic source-row order as the final tie-break.
   Ruling date is an organizational attribute and must not be used as a record
   recency key.
7. Retain native snake_case names throughout. Do not create legacy aliases.
8. Left-join to efile data on `EIN2`, validate the join as many filings to one
   BMF record, and preserve input row order.
9. Log source rows, distinct EINs, duplicate EINs, requested/available/missing
   fields, panel EINs, matched/unmatched EINs, rows before/after the join, and
   final dimensions.

## Current-to-new field crosswalk

| Current package field | New BMF field | Status | Notes |
|---|---|---|---|
| `EIN2` | `EIN2` | Exact | Stable merge key; already uppercase and coercion-safe. |
| `NTEE_NCCS` | `ntee_code_clean` | Direct replacement | New field is already standardized and includes explicit `UNDEFINED`/`INVALID` encodings. |
| `NTEEV2` | `nteev2` | Direct replacement | New BMF already supplies the full NTEEv2 value. Do not recompute unless validation is desired. |
| `NTMAJ12` | `nteev2_subsector` | Likely direct replacement | Confirm category equivalence against `get_industry()` fixtures before declaring exact compatibility. The dictionary describes this as the NTEEv2 subsector code. |
| `NTEE_ORG_TYPE` | `nteev2_org_type` | Direct replacement | New BMF supplies the organization-type component. |
| `CENSUS_CBSA_FIPS` | — | No replacement | No CBSA/metro FIPS field appears in the new dictionary. |
| `CENSUS_CBSA_NAME` | `geo_metro_area` | Approximate | Geocoder metropolitan-area name, not necessarily the former Census CBSA definition. Rename rather than presenting it as exact CBSA compatibility. |
| `CENSUS_BLOCK_FIPS` | — | No replacement | No Census block identifier appears in the new dictionary. |
| `CENSUS_URBAN_AREA` | — | No replacement | No Census urban-area field appears in the new dictionary. |
| `CENSUS_STATE_ABBR` | `geo_state_abbr` | Direct geocoded analogue | This is based on the matched geocoded location. `org_addr_state` is the normalized source-address alternative. |
| `CENSUS_COUNTY_NAME` | `geo_county` | Direct geocoded analogue | County name from the geocoder; no county FIPS field is supplied. |
| `BMF_SUBSECTION_CODE` | `subsection_code` | Direct replacement | Native field also has related classification and definition fields. |
| `BMF_FOUNDATION_CODE` | `foundation_code` | Direct replacement | `foundation_code_definition` is also available and useful. |
| `ORG_RULING_YEAR` | derive from `ruling_date` | Derived | Use the four-digit year only when `ruling_date_is_missing == FALSE`; the new source uses `1900-01-01` as a sentinel for missing/invalid dates. |
| `F990_TOTAL_REVENUE_RECENT` | `revenue_amount` | Direct replacement | Most recent BMF revenue amount. |
| `F990_TOTAL_INCOME_RECENT` | `income_amount` | Direct replacement | Most recent BMF income amount. |
| `F990_TOTAL_ASSETS_RECENT` | `asset_amount` | Direct replacement | Most recent BMF asset amount. |
| `F990_TOTAL_EXPENSES_RECENT` | — | No replacement | The new dictionary has no expense-amount field. Keep absent, obtain from efile data, or explicitly return `NA`; do not substitute income or revenue. |

## Proposed native default selection

```r
.BMF_VARS <- c(
  "EIN2",
  "org_name_display",
  "ntee_code_clean",
  "ntee_code_major_group",
  "nteev2",
  "nteev2_subsector",
  "nteev2_org_type",
  "subsection_code",
  "foundation_code",
  "foundation_code_definition",
  "ruling_date",
  "ruling_date_is_missing",
  "filing_requirement_code",
  "filing_requirement_code_definition",
  "asset_amount",
  "income_amount",
  "revenue_amount",
  "org_addr_state",
  "org_addr_zip5",
  "geo_is_geocoded",
  "geo_lat",
  "geo_lon",
  "geo_state_abbr",
  "geo_county",
  "geo_metro_area",
  "geo_score",
  "geo_status",
  "bmf_source",
  "bmf_vintage_ym"
)
```

This default intentionally includes provenance and geocoding-quality fields.
Without `geo_status`, `geo_score`, and `geo_is_geocoded`, users cannot assess
whether the appended geography is reliable.

The derived native field `ruling_year` is generated with sentinel-aware date
logic. Fields without replacements are omitted from defaults. If explicitly
requested, they produce one consolidated warning and appear in the manifest's
`missing_requested_fields`; they are not silently omitted or populated from
approximate fields.

## Changes needed in the current implementation

- Replace all three old Unified BMF URL defaults.
- Consolidate `.attach_bmf()`, `.attach_bmf_filtered()`, and `merge_bmf()` so
  they use one field-selection and preparation implementation.
- Remove assumptions about uppercase `EIN`, `ORG_RULING_DATE`, `NTEE_NCCS`, and
  `NTEE_IRS` source columns.
- Stop ordering records by ruling date.
- Stop recomputing NTEEv2 fields by default; use the supplied normalized fields.
- Replace silent `intersect()` selection with requested/available/missing field
  reporting.
- Preserve native fields throughout and do not add legacy aliases.
- Add fixture tests for duplicate EIN resolution, missing requested fields,
  ruling-date sentinel handling, geocoding fields, and many-to-one join
  validation.
