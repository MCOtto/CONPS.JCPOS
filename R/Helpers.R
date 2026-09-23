# Shared helpers for Germination.R and Survival.R.

# 90% interval on the probability scale from a fitted mean and standard error.
# A beta distribution with that mean and sd is matched and its 5th, 50th, and
# 95th percentiles are returned (IPMbook::qbeta2). If the sd is too large for a
# beta with that mean, qbeta2 warns and the limits are NA.
beta_interval <- function(mean, se) {
  tibble::tibble(
    Mean = as.double(mean),
    SE = as.double(se),
    Median = IPMbook::qbeta2(0.50, mean = Mean, sd = SE),
    CI05 = IPMbook::qbeta2(0.05, mean = Mean, sd = SE),
    CI95 = IPMbook::qbeta2(0.95, mean = Mean, sd = SE)
  )
}

# Marginal rates: average the linear predictor (logit) over the rows of `grid`
# within each level of `by`, with a standard error from the full covariance
# matrix. Handles rank-deficient fits by dropping aliased (NA) coefficients.
marginal_rates <- function(fit, grid, by) {
  X <- model.matrix(delete.response(terms(fit)), data = grid,
                    xlev = fit$xlevels, contrasts.arg = fit$contrasts)
  b <- coef(fit)
  ok <- !is.na(b)
  X <- X[, ok, drop = FALSE]
  V <- vcov(fit)[ok, ok]
  grp <- grid[[by]]
  levs <- if (is.factor(grp)) levels(droplevels(grp)) else unique(grp)
  out <- lapply(levs, function(lv) {
    L <- colMeans(X[grp == lv, , drop = FALSE])
    lgt <- sum(L * b[ok])
    se <- sqrt(drop(t(L) %*% V %*% L))
    p <- plogis(lgt)
    dplyr::bind_cols(tibble::tibble(!!by := factor(lv, levels = levs), logit = lgt, logit.SE = se),
                     beta_interval(p, p * (1 - p) * se))
  })
  dplyr::bind_rows(out)
}

# Consistent colours (Okabe-Ito palette) and theme.
soil.colours <- c("Peat" = "#009E73", "peat" = "#009E73",
                  "Mineral/Compost" = "#D55E00", "MC" = "#D55E00",
                  "S3" = "#0072B2")
herb.colours <- c("Forb" = "#CC79A7", "Grass" = "#E69F00")
date.colours <- c("2025-07-10" = "#B39DDB", "2025-08-14" = "#7E57C2", "2025-09-11" = "#311B92")

theme_jcpos <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title.position = "plot",
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "right")
}

save_figure <- function(plot, filename, width = 7, height = 4.5) {
  ggplot2::ggsave(here::here("doc", "images", filename), plot,
                  width = width, height = height, dpi = 300, bg = "white")
}

write_table <- function(x, filename) {
  dir.create(here::here("doc", "tables"), showWarnings = FALSE)
  utils::write.csv(x, here::here("doc", "tables", filename), row.names = FALSE)
}
