# List known efile table names

The Form 990 / 990EZ tables of panel990's catalog
([`panel990::table_catalog()`](https://nonprofit-open-data-collective.github.io/panel990/reference/table_catalog.html)),
which follows the concordance990 tables of the current efile release.
Cardinality comes from the T-number: T00 is one-to-one, T01-T98
one-to-many, and T99 supplemental text.

## Usage

``` r
efile_tables(cardinality = "all")
```

## Arguments

- cardinality:

  One of `"all"`, `"1x1"`, `"1xm"`, or `"supplemental"`.

## Value

Character vector of known canonical table names.
