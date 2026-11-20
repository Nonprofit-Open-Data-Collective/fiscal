# Decompose an observed correlation matrix of shares into
#   structural (closure-induced) correlation + remaining (real) association.
#
# Simulation: six expense categories with three planted real relationships among
# the dollar amounts (Admin-Occupancy, Program-Fundraising, Travel-Other).
# Amounts are closed to shares of total expenses, as a ratio-based index would.
#
# Structural component = expected share correlations under a permutation null:
# each category's amounts are shuffled independently (keeping every marginal
# distribution but breaking all real association), closed to shares, and the
# correlation matrix is averaged over B permutations.
# Remainder = observed - structural.
#
# Output: dev/health-index/closure-decomposition.png

library(ggplot2)
library(dplyr)
library(cowplot)

set.seed(1897)

n <- 1000
B <- 500
parts <- c("Program", "Admin", "Fundraising", "Occupancy", "Travel", "Other")
meanlog <- c(13.0, 11.5, 10.5, 11.0, 9.5, 10.0)
sdlog   <- c(0.45, 0.60, 0.90, 0.70, 1.00, 0.85)
D <- length(parts)

R <- diag(D); dimnames(R) <- list(parts, parts)
plant <- function(a, b, r) { R[a, b] <<- r; R[b, a] <<- r }
plant("Admin", "Occupancy", 0.6)
plant("Program", "Fundraising", 0.4)
plant("Travel", "Other", 0.5)

L <- MASS::mvrnorm(n, meanlog, diag(sdlog) %*% R %*% diag(sdlog))
amounts <- exp(L); colnames(amounts) <- parts
shares  <- amounts / rowSums(amounts)

observed <- cor(shares)
structural <- Reduce(`+`, lapply(seq_len(B), function(b) {
  p <- apply(amounts, 2, sample)
  cor(p / rowSums(p))
})) / B
remainder <- observed - structural
truth <- cor(amounts)

ut <- upper.tri(observed)
r_rem <- cor(remainder[ut], truth[ut]);  r_obs <- cor(observed[ut], truth[ut])
mae_rem <- mean(abs(remainder[ut] - truth[ut])); mae_obs <- mean(abs(observed[ut] - truth[ut]))

col_low  <- "#2a78d6"
col_high <- "#b8541a"
ink      <- "#1f1f1e"
muted    <- "#7a7a75"

heat <- function(m, title, subtitle) {
  df <- as.data.frame(as.table(m)) |>
    mutate(Var1 = factor(Var1, levels = parts), Var2 = factor(Var2, levels = rev(parts)),
           diag = Var1 == Var2,
           lab = ifelse(diag, "", sprintf("%.2f", Freq)),
           Freq = ifelse(diag, NA, Freq),
           planted = !diag & R[cbind(as.character(Var1), as.character(Var2))] != 0)
  ggplot(df, aes(Var1, Var2, fill = Freq)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_tile(data = filter(df, planted), fill = NA, colour = ink, linewidth = 0.7,
              width = 0.92, height = 0.92) +
    geom_text(aes(label = lab), size = 3, colour = ink) +
    scale_fill_gradient2(low = col_low, mid = "white", high = col_high,
                         limits = c(-1, 1), na.value = "grey92") +
    coord_fixed() +
    labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(), legend.position = "none",
          plot.title = element_text(face = "bold", size = 11, colour = ink),
          plot.subtitle = element_text(size = 8.5, colour = muted),
          axis.text.x = element_text(angle = 40, hjust = 1))
}

op <- function(sym) ggdraw() + draw_label(sym, size = 26, colour = muted, fontface = "bold")

panels <- plot_grid(
  heat(observed,   "Observed", "Correlations among expense shares"),
  op("="),
  heat(structural, "Structural (closure artifact)", "Expected under independent amounts"),
  op("+"),
  heat(remainder,  "Remainder (real association)", "Observed minus structural"),
  nrow = 1, rel_widths = c(1, 0.12, 1, 0.12, 1), align = "h", axis = "tb")

title <- ggdraw() +
  draw_label("Separating closure artifacts from real association in share correlations",
             fontface = "bold", size = 13, x = 0.01, hjust = 0, colour = ink)
subtitle <- ggdraw() +
  draw_label(paste0(
    "Six expense categories, n = ", n, ", with three planted relationships among dollar amounts (outlined cells: Admin-Occupancy, ",
    "Program-Fundraising, Travel-Other).\n",
    "Structural component = mean share correlation over ", B, " permutations that shuffle each category independently, ",
    "keeping its distribution but breaking real links.\n",
    sprintf("Remainder vs. true amount correlations: r = %.2f, mean abs. error = %.2f (observed shares: r = %.2f, error = %.2f). ",
            r_rem, mae_rem, r_obs, mae_obs),
    "Small leftovers (about -0.15) remain\n",
    "where real correlations shift the totals, so the artifact and the real structure are not perfectly additive."),
    size = 9, x = 0.01, hjust = 0, colour = muted, lineheight = 1.1)

fig <- plot_grid(title, subtitle, panels, ncol = 1, rel_heights = c(0.07, 0.2, 1))

out <- file.path("dev", "health-index", "closure-decomposition.png")
ggsave(out, fig, width = 13, height = 5.6, dpi = 200, bg = "white", device = ragg::agg_png)
message("Wrote ", out)
