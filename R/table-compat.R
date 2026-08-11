#' List known efile table names
#'
#' @param cardinality One of `"all"`, `"1x1"`, `"1xm"`, or
#'   `"supplemental"`.
#' @return Character vector of known canonical table names.
#' @export
efile_tables <- function(cardinality = "all") {
  tnums <- vapply(.VALID_TABLES, .table_tnum, integer(1L))
  switch(cardinality,
    all = .VALID_TABLES,
    `1x1` = .VALID_TABLES[tnums == 0L],
    `1xm` = .VALID_TABLES[tnums >= 1L & tnums <= 98L],
    supplemental = .VALID_TABLES[tnums == 99L],
    stop("cardinality must be one of: 'all', '1x1', '1xm', 'supplemental'")
  )
}
