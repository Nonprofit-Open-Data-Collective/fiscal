#' List known efile table names
#'
#' The Form 990 / 990EZ tables of panel990's catalog
#' ([panel990::table_catalog()]), which follows the concordance990 tables of
#' the current efile release. Cardinality comes from the T-number: T00 is
#' one-to-one, T01-T98 one-to-many, and T99 supplemental text.
#'
#' @param cardinality One of `"all"`, `"1x1"`, `"1xm"`, or
#'   `"supplemental"`.
#' @return Character vector of known canonical table names.
#' @export
efile_tables <- function(cardinality = "all") {
  if (!is.character(cardinality) || length(cardinality) != 1L ||
      !cardinality %in% c("all", "1x1", "1xm", "supplemental"))
    stop("cardinality must be one of: 'all', '1x1', '1xm', 'supplemental'")
  panel990::table_catalog(cardinality, form = "990")$table
}
