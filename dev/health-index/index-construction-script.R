library(dplyr)
library(psych)
library(purrr)
library(tibble)

compute_variable_diagnostics <- function(data,
                                         corr_cutoff = 0.90,
                                         kmo_low = 0.50,
                                         communality_low = 0.20,
                                         mean_abs_cor_low = 0.10,
                                         smc_high = 0.90,
                                         vif_high = 10,
                                         near_zero_sd = 1e-6,
                                         nfactors_comm = NULL,
                                         rotate = "oblimin",
                                         fm = "minres") {
  
  x <- data |> dplyr::select(where(is.numeric)) |> as.data.frame()
  
  if (ncol(x) < 2) stop("Need at least two numeric variables.")
  
  sds <- sapply(x, sd, na.rm = TRUE)
  zero_var <- names(sds)[is.na(sds) | sds <= near_zero_sd]
  
  x_use <- x[, sds > near_zero_sd & !is.na(sds), drop = FALSE]
  cor_mat <- cor(x_use, use = "pairwise.complete.obs")
  
  if (is.null(nfactors_comm)) {
    nfactors_comm <- max(1, min(3, floor(ncol(x_use) / 3)))
  }
  
  kmo_obj <- psych::KMO(cor_mat)
  msa_i <- kmo_obj$MSAi
  smc <- psych::smc(cor_mat)
  vif <- 1 / (1 - smc)
  mean_abs_cor <- apply(abs(cor_mat), 1, mean, na.rm = TRUE)
  max_abs_cor <- apply(abs(cor_mat), 1, function(v) max(v[v < 0.999999], na.rm = TRUE))
  det_full <- det(cor_mat)
  
  delta_det <- sapply(seq_len(ncol(cor_mat)), function(i) {
    det(cor_mat[-i, -i, drop = FALSE]) - det_full
  })
  names(delta_det) <- colnames(cor_mat)
  
  delta_log_det <- sapply(seq_len(ncol(cor_mat)), function(i) {
    d_sub <- det(cor_mat[-i, -i, drop = FALSE])
    log(abs(d_sub) + 1e-12) - log(abs(det_full) + 1e-12)
  })
  names(delta_log_det) <- colnames(cor_mat)
  
  inv_cor <- tryCatch(solve(cor_mat), error = function(e) NULL)
  anti_image_diag <- if (!is.null(inv_cor)) 1 / diag(inv_cor) else rep(NA_real_, ncol(cor_mat))
  names(anti_image_diag) <- colnames(cor_mat)
  
  fa_try <- tryCatch(
    psych::fa(x_use, nfactors = nfactors_comm, fm = fm, rotate = rotate),
    error = function(e) NULL,
    warning = function(w) invokeRestart("muffleWarning")
  )
  
  communality <- if (!is.null(fa_try)) fa_try$communality else rep(NA_real_, ncol(x_use))
  names(communality) <- colnames(x_use)
  
  cor_long <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE) |>
    dplyr::rename(var1 = Var1, var2 = Var2, r = Freq) |>
    dplyr::filter(var1 != var2) |>
    dplyr::mutate(abs_r = abs(r))
  
  redundancy_pairs <- cor_long |>
    dplyr::filter(abs_r >= corr_cutoff) |>
    dplyr::rowwise() |>
    dplyr::mutate(pair_id = paste(sort(c(var1, var2)), collapse = "~~")) |>
    dplyr::ungroup() |>
    dplyr::distinct(pair_id, .keep_all = TRUE)
  
  redundant_with <- lapply(colnames(cor_mat), function(v) {
    redundancy_pairs |>
      dplyr::filter(var1 == v | var2 == v) |>
      dplyr::mutate(other = ifelse(var1 == v, var2, var1)) |>
      dplyr::pull(other)
  })
  names(redundant_with) <- colnames(cor_mat)
  
  n_redundant_links <- sapply(redundant_with, length)
  
  diagnostics <- tibble::tibble(
    variable = colnames(cor_mat),
    sd = sds[colnames(cor_mat)],
    kmo_msa = as.numeric(msa_i[colnames(cor_mat)]),
    communality = as.numeric(communality[colnames(cor_mat)]),
    smc = as.numeric(smc[colnames(cor_mat)]),
    vif = as.numeric(vif[colnames(cor_mat)]),
    mean_abs_cor = as.numeric(mean_abs_cor[colnames(cor_mat)]),
    max_abs_cor = as.numeric(max_abs_cor[colnames(cor_mat)]),
    anti_image_diag = as.numeric(anti_image_diag[colnames(cor_mat)]),
    delta_det = as.numeric(delta_det[colnames(cor_mat)]),
    delta_log_det = as.numeric(delta_log_det[colnames(cor_mat)]),
    n_redundant_links = as.integer(n_redundant_links[colnames(cor_mat)]),
    redundant_with = vapply(redundant_with[colnames(cor_mat)], function(z) paste(z, collapse = "; "), character(1))
  ) |>
    dplyr::mutate(
      flag_zero_variance = sd <= near_zero_sd,
      flag_low_kmo = kmo_msa < kmo_low,
      flag_low_communality = !is.na(communality) & communality < communality_low,
      flag_high_redundancy = max_abs_cor >= corr_cutoff,
      flag_weak_correlation = mean_abs_cor < mean_abs_cor_low,
      flag_high_smc = smc >= smc_high,
      flag_high_vif = vif >= vif_high,
      prelim_class = dplyr::case_when(
        flag_zero_variance ~ "noise",
        flag_high_redundancy ~ "redundant_candidate",
        flag_low_kmo & flag_low_communality & flag_weak_correlation ~ "noise",
        flag_low_kmo & flag_low_communality ~ "standalone_or_noise",
        flag_low_communality ~ "standalone_candidate",
        TRUE ~ "retain"
      )
    )
  
  if (length(zero_var) > 0) {
    zero_tbl <- tibble::tibble(
      variable = zero_var,
      sd = sds[zero_var],
      kmo_msa = NA_real_,
      communality = NA_real_,
      smc = NA_real_,
      vif = NA_real_,
      mean_abs_cor = NA_real_,
      max_abs_cor = NA_real_,
      anti_image_diag = NA_real_,
      delta_det = NA_real_,
      delta_log_det = NA_real_,
      n_redundant_links = NA_integer_,
      redundant_with = NA_character_,
      flag_zero_variance = TRUE,
      flag_low_kmo = NA,
      flag_low_communality = NA,
      flag_high_redundancy = NA,
      flag_weak_correlation = NA,
      flag_high_smc = NA,
      flag_high_vif = NA,
      prelim_class = "noise"
    )
    diagnostics <- dplyr::bind_rows(diagnostics, zero_tbl)
  }
  
  attr(diagnostics, "cor_mat") <- cor_mat
  attr(diagnostics, "redundancy_pairs") <- redundancy_pairs
  attr(diagnostics, "kmo_object") <- kmo_obj
  attr(diagnostics, "fa_screen") <- fa_try
  
  diagnostics
}

triage_variables <- function(diagnostics,
                             keep_from_redundant = c("higher_kmo", "higher_mean_abs_cor", "first")) {
  
  keep_from_redundant <- match.arg(keep_from_redundant)
  d <- diagnostics |> dplyr::arrange(variable)
  
  choose_keeper <- function(sub) {
    if (nrow(sub) == 1) return(sub$variable)
    
    if (keep_from_redundant == "higher_kmo") {
      sub |>
        dplyr::arrange(dplyr::desc(kmo_msa), dplyr::desc(mean_abs_cor), variable) |>
        dplyr::slice(1) |>
        dplyr::pull(variable)
    } else if (keep_from_redundant == "higher_mean_abs_cor") {
      sub |>
        dplyr::arrange(dplyr::desc(mean_abs_cor), dplyr::desc(kmo_msa), variable) |>
        dplyr::slice(1) |>
        dplyr::pull(variable)
    } else {
      sort(sub$variable)[1]
    }
  }
  
  adjacency <- lapply(d$variable, function(v) {
    txt <- d$redundant_with[d$variable == v]
    if (length(txt) == 0 || is.na(txt) || txt == "") character(0) else strsplit(txt, ";\\s*")[[1]]
  })
  names(adjacency) <- d$variable
  
  visited <- setNames(rep(FALSE, length(adjacency)), names(adjacency))
  redundancy_groups <- list()
  
  for (v in names(adjacency)) {
    if (visited[[v]] || length(adjacency[[v]]) == 0) next
    
    queue <- c(v)
    comp <- character(0)
    
    while (length(queue) > 0) {
      cur <- queue[1]
      queue <- queue[-1]
      if (visited[[cur]]) next
      
      visited[[cur]] <- TRUE
      comp <- unique(c(comp, cur))
      nxt <- adjacency[[cur]]
      queue <- unique(c(queue, nxt[!visited[nxt]]))
    }
    
    redundancy_groups[[length(redundancy_groups) + 1]] <- sort(comp)
  }
  
  redundancy_tbl <- purrr::map_dfr(redundancy_groups, function(g) {
    sub <- d |> dplyr::filter(variable %in% g)
    keeper <- choose_keeper(sub)
    
    tibble::tibble(
      group = paste(g, collapse = " | "),
      kept = keeper,
      dropped = paste(setdiff(g, keeper), collapse = "; ")
    )
  })
  
  dropped_redundant <- if (nrow(redundancy_tbl) > 0) {
    unique(unlist(strsplit(redundancy_tbl$dropped[redundancy_tbl$dropped != ""], ";\\s*")))
  } else {
    character(0)
  }
  
  build_reasons <- function(row) {
    out <- character(0)
    
    if (isTRUE(row$flag_zero_variance)) {
      out <- c(out, sprintf("Near-zero variance (sd = %.4f).", row$sd))
    }
    if (isTRUE(row$flag_high_redundancy)) {
      out <- c(out, sprintf("Highly redundant with %s (max |r| = %.3f).", row$redundant_with, row$max_abs_cor))
    }
    if (isTRUE(row$flag_low_kmo)) {
      out <- c(out, sprintf("Low KMO / MSA (%.3f), suggesting weak shared variance with the rest of the system.", row$kmo_msa))
    }
    if (isTRUE(row$flag_low_communality)) {
      out <- c(out, sprintf("Low communality (%.3f), suggesting poor fit in a common-factor model.", row$communality))
    }
    if (isTRUE(row$flag_weak_correlation)) {
      out <- c(out, sprintf("Weak overall correlation structure (mean absolute correlation = %.3f).", row$mean_abs_cor))
    }
    if (isTRUE(row$flag_high_smc)) {
      out <- c(out, sprintf("Highly predictable from the remaining variables (SMC = %.3f).", row$smc))
    }
    if (isTRUE(row$flag_high_vif)) {
      out <- c(out, sprintf("High multicollinearity (VIF = %.2f).", row$vif))
    }
    
    out
  }
  
  d$reasons <- purrr::pmap(d, function(...) build_reasons(as.list(list(...))))
  
  keep_vars <- d |>
    dplyr::filter(!variable %in% dropped_redundant) |>
    dplyr::filter(!(flag_zero_variance %in% TRUE)) |>
    dplyr::filter(!(flag_low_kmo %in% TRUE & flag_low_communality %in% TRUE & flag_weak_correlation %in% TRUE)) |>
    dplyr::pull(variable)
  
  low_comm_standalone <- d |>
    dplyr::filter(!variable %in% dropped_redundant) |>
    dplyr::filter(flag_low_communality %in% TRUE) |>
    dplyr::filter(!(flag_low_kmo %in% TRUE & flag_weak_correlation %in% TRUE)) |>
    dplyr::select(variable, kmo_msa, communality, mean_abs_cor, reasons)
  
  suspected_noise <- d |>
    dplyr::filter(
      flag_zero_variance %in% TRUE |
        (flag_low_kmo %in% TRUE & flag_low_communality %in% TRUE & flag_weak_correlation %in% TRUE)
    ) |>
    dplyr::select(variable, sd, kmo_msa, communality, mean_abs_cor, reasons)
  
  dropped_report <- d |>
    dplyr::filter(variable %in% c(dropped_redundant, suspected_noise$variable, low_comm_standalone$variable)) |>
    dplyr::mutate(reason_bullets = purrr::map_chr(reasons, function(x) paste0("- ", x, collapse = "\n"))) |>
    dplyr::select(variable, reason_bullets)
  
  list(
    keep_for_subsequent_steps = keep_vars,
    highly_redundant_groups = redundancy_tbl,
    low_communality_but_possible_standalone = low_comm_standalone,
    suspected_noise = suspected_noise,
    dropped_report = dropped_report,
    full_diagnostics = d
  )
}