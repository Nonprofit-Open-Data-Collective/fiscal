# ---- Fiscal wrappers around panel990 BMF functionality --------------------

.prepare_bmf <- function(
    bmf,
    bmf_vars = .BMF_VARS,
    strict = FALSE,
    verbose = TRUE
) {
  panel990::bmf_prepare(
    bmf, vars = bmf_vars, strict = strict, verbose = verbose
  )
}

.join_bmf <- function(
    df,
    bmf,
    bmf_vars = .BMF_VARS,
    strict = FALSE,
    verbose = TRUE
) {
  panel990::bmf_merge(
    data = as.data.frame(df), source = as.data.frame(bmf), vars = bmf_vars,
    strict = strict, verbose = verbose
  )
}

.attach_bmf <- function(
    df,
    bmf_url,
    verbose = TRUE,
    by_df = "EIN2",
    by_bmf = "EIN2",
    deduplicate_bmf = TRUE,
    normalize_ntee = TRUE,
    bmf_vars = .BMF_VARS
) {
  if (!identical(by_df, "EIN2") || !identical(by_bmf, "EIN2"))
    stop("The current BMF master joins on `EIN2`.")
  panel990::bmf_merge(
    data = as.data.frame(df), source = bmf_url, vars = bmf_vars,
    verbose = verbose
  )
}

.attach_bmf_filtered <- function(df, bmf_raw, verbose = TRUE) {
  panel990::bmf_merge(
    data = as.data.frame(df), source = as.data.frame(bmf_raw),
    vars = .BMF_VARS, verbose = verbose
  )
}
