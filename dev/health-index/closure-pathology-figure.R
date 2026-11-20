# Closure pathology: how turning independent amounts into shares creates
# correlation structure, and with it a spurious bipolar first factor.
#
# Null simulation: D independent lognormal expense categories, so the true
# correlation among them is zero. The same data are then analyzed four ways:
#   1. log amounts (no closure)             -> the truth: no structure
#   2. shares of total expenses (closed)    -> negative correlations, bipolar PC1
#   3. CLR of shares (the "standard fix")   -> still biased; CLR is singular too
#   4. ILR pivot balances of shares         -> full rank and much weaker bias, but
#                                              not zero: ILR coordinates are only
#                                              uncorrelated under this null when
#                                              every part has equal log-variance
#
# Output: dev/health-index/closure-pathology.png

library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

set.seed(1897)  # Pearson's spurious-correlation paper

n <- 1000
parts <- c("Program", "Admin", "Fundraising", "Occupancy", "Travel", "Other")
meanlog <- c(13.0, 11.5, 10.5, 11.0, 9.5, 10.0)  # program spending dominates
sdlog   <- c(0.45, 0.60, 0.90, 0.70, 1.00, 0.85)
D <- length(parts)

amounts <- sapply(seq_len(D), function(j) rlnorm(n, meanlog[j], sdlog[j]))
colnames(amounts) <- parts

log_amt <- log(amounts)
shares  <- amounts / rowSums(amounts)
clr     <- log(shares) - rowMeans(log(shares))

# ILR pivot coordinates: balance i contrasts part i with the geometric mean of
# the parts after it, scaled so the basis is orthonormal.
pivot_ilr <- function(x) {
  L <- log(x); k <- ncol(L)
  sapply(seq_len(k - 1), function(i)
    sqrt((k - i) / (k - i + 1)) * (L[, i] - rowMeans(L[, (i + 1):k, drop = FALSE])))
}
ilr <- pivot_ilr(shares)
colnames(ilr) <- c("Program | rest", "Admin | rest", "Fundraising | rest",
                   "Occupancy | rest", "Travel | Other")

versions <- list(
  "Log amounts\n(no closure)"          = log_amt,
  "Shares of total\n(closed)"          = shares,
  "CLR of shares\n(standard fix)"      = clr,
  "ILR balances\n(pivot coordinates)"  = ilr
)

# Parallel analysis: 95th percentile eigenvalues from independent normal data,
# computed for each number of variables in use (6 parts, 5 ILR balances)
pa95_for <- function(k) {
  pa <- replicate(200, eigen(cor(matrix(rnorm(n * k), n, k)), only.values = TRUE)$values)
  apply(pa, 1, quantile, 0.95)
}
pa95 <- lapply(c(D - 1, D), pa95_for)
names(pa95) <- c(D - 1, D)

col_low  <- "#2a78d6"
col_high <- "#b8541a"
ink      <- "#1f1f1e"
muted    <- "#7a7a75"

theme_fig <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = "bold", colour = ink),
        axis.title = element_text(colour = muted, size = 9),
        legend.position = "none")

heat_plot <- function(x, title) {
  v <- colnames(x)
  r <- cor(x)
  df <- as.data.frame(as.table(r)) |>
    mutate(Var1 = factor(Var1, levels = v), Var2 = factor(Var2, levels = rev(v)),
           lab = ifelse(Var1 == Var2, "", sprintf("%.2f", Freq)),
           Freq = ifelse(Var1 == Var2, NA, Freq))
  ggplot(df, aes(Var1, Var2, fill = Freq)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = lab), size = 2.6, colour = ink) +
    scale_fill_gradient2(low = col_low, mid = "white", high = col_high,
                         limits = c(-1, 1), na.value = "grey92") +
    coord_fixed() + labs(x = NULL, y = NULL, title = title) + theme_fig +
    theme(axis.text.x = element_text(angle = 40, hjust = 1), panel.grid = element_blank())
}

scree_plot <- function(x, title) {
  k <- ncol(x)
  ev <- eigen(cor(x), only.values = TRUE)$values
  rnd <- pa95[[as.character(k)]]
  df <- tibble(k = seq_len(k), observed = pmax(ev, 0), random = rnd)
  ggplot(df, aes(k)) +
    geom_line(aes(y = random), colour = muted, linetype = "dashed") +
    geom_line(aes(y = observed), colour = ink) +
    geom_point(aes(y = observed, fill = observed > random), shape = 21, size = 2.6, colour = ink) +
    scale_fill_manual(values = c(`TRUE` = col_high, `FALSE` = "white")) +
    scale_x_continuous(breaks = seq_len(D), limits = c(1, D)) +
    coord_cartesian(ylim = c(0, max(2.6, max(ev) * 1.05))) +
    labs(x = "Component", y = "Eigenvalue", title = title) + theme_fig
}

load_plot <- function(x, title) {
  v <- colnames(x)
  e <- eigen(cor(x))
  l <- e$vectors[, 1] * sqrt(e$values[1])
  if (sum(l) < 0) l <- -l  # orient so the majority loads positive
  sig <- e$values[1] > pa95[[as.character(ncol(x))]][1]
  df <- tibble(part = factor(v, levels = rev(v)), l = l,
               grp = if (sig) ifelse(l > 0, "pos", "neg") else "noise")
  if (!sig) title <- paste0(title, " (noise: not retained)")
  ggplot(df, aes(l, part, fill = grp)) +
    geom_col(width = 0.65) +
    geom_vline(xintercept = 0, colour = muted, linewidth = 0.3) +
    scale_fill_manual(values = c(pos = col_high, neg = col_low, noise = "grey80")) +
    scale_x_continuous(limits = c(-1, 1)) +
    labs(x = "Loading on first component", y = NULL, title = title) + theme_fig
}

rows <- lapply(names(versions), function(nm) {
  x <- versions[[nm]]
  row_lab <- ggdraw() + draw_label(nm, fontface = "bold", size = 10, colour = ink, angle = 90)
  plot_grid(row_lab,
            heat_plot(x, "Correlations"),
            scree_plot(x, "Scree vs. parallel analysis"),
            load_plot(x, "First component"),
            nrow = 1, rel_widths = c(0.12, 1.15, 1, 1), align = "h", axis = "tb")
})

title <- ggdraw() +
  draw_label("Closure manufactures structure from independent parts",
             fontface = "bold", size = 13, x = 0.01, hjust = 0, colour = ink)
subtitle <- ggdraw() +
  draw_label(paste0(
    "Six expense categories simulated as independent lognormals (true correlations = 0, n = ", n,
    "). Filled points exceed the 95th percentile of eigenvalues from random data.\n",
    "Closing to shares forces negative correlations and a bipolar first component (the dominant category vs. the rest), and the zero eigenvalue lifts\n",
    "the others over the random-data line, so parallel analysis retains spurious components. CLR swaps in a new artifact: the most volatile category vs. the rest.\n",
    "ILR balances are full rank and far less biased, but a spurious first component (and a borderline second) survives: balances are uncorrelated only if every\n",
    "category has equal log-variance. No transform of the shares alone can recover independence of the amounts."),
    size = 9, x = 0.01, hjust = 0, colour = muted, lineheight = 1.1)

fig <- plot_grid(title, subtitle, plotlist = rows, ncol = 1,
                 rel_heights = c(0.07, 0.27, 1, 1, 1, 1))

out <- file.path("dev", "health-index", "closure-pathology.png")
ggsave(out, fig, width = 11, height = 14.4, dpi = 200, bg = "white", device = ragg::agg_png)
message("Wrote ", out)
