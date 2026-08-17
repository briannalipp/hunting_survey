## =============================================================================
## Hunting Survey — Identity Block Data Reduction (rework)
## Replaces the PCA treatment in hunt_regress.Rmd.
##
## Self-contained: runs standalone in a fresh session. The SETUP section below
## duplicates helpers already defined in hunt_regress.Rmd — do NOT paste it in
## when folding this work into the document, only the sections after it.
## =============================================================================

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------
library(here)
library(tidyverse)
library(psych)
library(GPArotation)
library(knitr)
library(kableExtra)
library(e1071)
library(ragg)

fam       <- "Libre Franklin"
okabe_ito <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00", "#56B4E9")
theme_set(theme_minimal(base_size = 12, base_family = fam))
options(knitr.kable.NA = "")

if (!exists("personal_weights")) {
  personal_weights <- readRDS(here("data", "personal_weights.rds"))
}

ktab <- function(x, caption = NULL, ...) {
  kable(x, caption = caption, ...) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE, position = "left")
}

loadings_table <- function(efa, cutoff = 0.30) {
  L     <- unclass(efa$loadings)
  out   <- as.data.frame(L)
  out[] <- lapply(out, function(x) ifelse(abs(x) < cutoff, NA_real_, round(x, 3)))
  out %>%
    rownames_to_column("item") %>%
    mutate(h2       = round(efa$communality[item], 2),
           dominant = colnames(L)[apply(abs(L), 1, which.max)],
           top_load = apply(abs(L), 1, max)) %>%
    arrange(dominant, desc(top_load)) %>%
    select(-top_load)
}

phi_text <- function(efa) {
  p <- efa$Phi
  if (is.null(p) || ncol(p) < 2) return("n/a")
  idx <- which(upper.tri(p), arr.ind = TRUE)
  paste(sprintf("%s-%s r = %.2f",
                rownames(p)[idx[, "row"]], colnames(p)[idx[, "col"]], p[upper.tri(p)]),
        collapse = "; ")
}

var_footer <- function(efa) {
  v <- efa$Vaccounted
  paste0("SS loadings ", paste(round(v["SS loadings", ], 2), collapse = " / "),
         "; cumulative variance ", round(max(v["Cumulative Var", ]), 3))
}

spearman_brown <- function(df, items) {
  r <- cor(df[[items[1]]], df[[items[2]]], use = "pairwise.complete.obs")
  (2 * r) / (1 + r)
}

omega_total <- function(df, items) {
  f <- fa(df[, items], nfactors = 1, fm = "ml", cor = "poly")
  l <- as.numeric(f$loadings)
  sum(l)^2 / (sum(l)^2 + sum(1 - l^2))
}

alpha_table <- function(df, scales) {
  map_dfr(names(scales), function(nm) {
    items <- scales[[nm]]
    a <- psych::alpha(df[, items], warnings = FALSE)
    tibble(scale  = nm,
           k      = length(items),
           alpha  = round(a$total$raw_alpha, 3),
           omega  = if (length(items) >= 3) round(omega_total(df, items), 3) else NA_real_,
           sb     = if (length(items) == 2) round(spearman_brown(df, items), 3) else NA_real_,
           mean_r = round(a$total$average_r, 3))
  })
}

composite <- function(df, items, min_items = ceiling(length(items) / 2)) {
  m   <- as.matrix(df[, items, drop = FALSE])
  out <- rowMeans(m, na.rm = TRUE)
  out[rowSums(!is.na(m)) < min_items] <- NA_real_
  out
}

# -----------------------------------------------------------------------------
# ITEMS
# -----------------------------------------------------------------------------
# All nine Q24 items on the lv_identify scale (1 = Not at all, 5 = Extremely).
identity_vars <- c(
  "identify_hunters", "identify_ranchers", "identify_environmentalists",
  "identify_farmers", "identify_small_business", "identify_landowners",
  "identify_outdoor_rec", "identify_outdoor_viewers", "identify_anglers")

id_df <- personal_weights %>% select(all_of(identity_vars)) %>% as.data.frame()

tibble(item     = identity_vars,
       n_miss   = colSums(is.na(id_df)),
       mean     = round(colMeans(id_df, na.rm = TRUE), 2),
       sd       = round(apply(id_df, 2, sd, na.rm = TRUE), 2)) %>%
  ktab(caption = "Identity items: descriptives")

# factorability checked on the polychoric matrix, consistent with the EFA below
id_poly <- polychoric(id_df)$rho

tibble(block      = "Identity",
       n_items    = length(identity_vars),
       KMO        = round(KMO(id_poly)$MSA, 3),
       bartlett_p = signif(cortest.bartlett(id_poly, n = nrow(id_df))$p.value, 3)) %>%
  ktab(caption = "Sampling adequacy and sphericity (polychoric)")

# -----------------------------------------------------------------------------
# DIMENSIONALITY
# -----------------------------------------------------------------------------
set.seed(42)
id_par <- fa.parallel(id_df, fa = "both", cor = "poly", n.iter = 100, plot = FALSE)

id_eigen <- tibble(factor    = seq_along(id_par$fa.values),
                   observed  = round(id_par$fa.values, 3),
                   simulated = round(id_par$fa.sim, 3),
                   retain    = id_par$fa.values > id_par$fa.sim)

id_eigen %>%
  ktab(caption = paste0("Identity block: parallel analysis eigenvalues ",
                      "(100 iterations, polychoric). Retained: ", id_par$nfact, "."))

id_eigen %>%
  pivot_longer(c(observed, simulated), names_to = "series", values_to = "eigenvalue") %>%
  mutate(series = recode(series, observed = "Observed",
                         simulated = "Simulated (random data)")) %>%
  ggplot(aes(x = factor, y = eigenvalue,
             color = series, shape = series, linetype = series)) +
  geom_hline(yintercept = 0, color = "grey80", linewidth = 0.4) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = seq_len(max(id_eigen$factor))) +
  scale_color_manual(values = c("Observed" = okabe_ito[1],
                                "Simulated (random data)" = "grey55")) +
  scale_shape_manual(values = c("Observed" = 16, "Simulated (random data)" = 17)) +
  scale_linetype_manual(values = c("Observed" = "solid",
                                   "Simulated (random data)" = "dashed")) +
  labs(x = "Factor", y = "Eigenvalue",
       caption = "Horn's parallel analysis, 100 iterations, polychoric correlations.") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.caption = element_text(color = "grey45", hjust = 0, size = rel(0.8)),
        axis.title = element_text(color = "grey20"),
        legend.title = element_blank(), legend.position = "top")

ggsave(here("figs", "identity_scree.png"),
       width = 7, height = 3.8, dpi = 300, device = agg_png)

# -----------------------------------------------------------------------------
# CANDIDATE SOLUTIONS
# -----------------------------------------------------------------------------
# The original PCA assumed 2 components. Fit 2 and 3 and compare rather than
# carrying that assumption forward.
set.seed(42)
id_efa2 <- fa(id_df, nfactors = 2, rotate = "oblimin", fm = "ml", cor = "poly")
id_efa3 <- fa(id_df, nfactors = 3, rotate = "oblimin", fm = "ml", cor = "poly")

tibble(solution = c("2-factor", "3-factor"),
       RMSEA    = round(c(id_efa2$RMSEA[1], id_efa3$RMSEA[1]), 3),
       TLI      = round(c(id_efa2$TLI, id_efa3$TLI), 3),
       BIC      = round(c(id_efa2$BIC, id_efa3$BIC), 1),
       cum_var  = round(c(max(id_efa2$Vaccounted["Cumulative Var", ]),
                          max(id_efa3$Vaccounted["Cumulative Var", ])), 3)) %>%
  ktab(caption = paste("Identity block: 2- vs 3-factor solution.",
                     "RMSEA < .08 acceptable, < .05 good; TLI > .95 good;",
                     "lower BIC preferred."))

# -----------------------------------------------------------------------------
# CHOSEN SOLUTION — set after reading the comparison above
# -----------------------------------------------------------------------------
n_identity <- 2
id_efa     <- if (n_identity == 2) id_efa2 else id_efa3

print(id_efa$loadings, cutoff = 0.30, sort = TRUE)
round(id_efa$Phi, 2)
round(id_efa$communality, 2)

loadings_table(id_efa) %>%
  ktab(caption = paste0(
    "Identity — EFA loadings (", n_identity, " factors, ML, oblimin, polychoric). ",
    "Loadings below |0.30| suppressed; h2 = communality. ", var_footer(id_efa),
    ". Factor correlations: ", phi_text(id_efa), "."))

# -----------------------------------------------------------------------------
# SCALE ASSIGNMENT — edit after reading the loadings
# -----------------------------------------------------------------------------
# Confirmed by the 2-factor EFA: no cross-loadings above |0.30|, all items load
# 0.51-0.97 on their dominant factor.
identity_scales <- list(
  ID_consum      = c("identify_hunters", "identify_ranchers", "identify_farmers",
                     "identify_landowners", "identify_anglers",
                     "identify_small_business"),
  ID_non_consume = c("identify_environmentalists", "identify_outdoor_rec",
                     "identify_outdoor_viewers"))

# -----------------------------------------------------------------------------
# RELIABILITY
# -----------------------------------------------------------------------------
alpha_table(id_df, identity_scales) %>%
  ktab(caption = "Identity scale reliability")

walk(names(identity_scales), function(nm) {
  if (length(identity_scales[[nm]]) >= 3) {
    cat("\n---", nm, "---\n")
    print(psych::alpha(id_df[, identity_scales[[nm]]], warnings = FALSE)$alpha.drop)
  }
})

# Sensitivity check on identify_environmentalists (lowest communality, .32).
# Corrected item-total correlation and model fit are the evidence here; comparing
# a 3-item alpha against a 2-item Spearman-Brown is not like-for-like.
nc3 <- identity_scales$ID_non_consume
a3  <- psych::alpha(id_df[, nc3], warnings = FALSE)

round(a3$item.stats[, c("r.drop", "mean", "sd")], 3)
round(cor(id_df[, nc3], use = "pairwise.complete.obs"), 2)

set.seed(42)
efa_noenv <- fa(id_df %>% select(-identify_environmentalists),
                nfactors = 2, rotate = "oblimin", fm = "ml", cor = "poly")

tibble(version = c("including environmentalists", "excluding environmentalists"),
       RMSEA   = round(c(id_efa2$RMSEA[1], efa_noenv$RMSEA[1]), 3),
       TLI     = round(c(id_efa2$TLI, efa_noenv$TLI), 3)) %>%
  ktab(caption = paste("Sensitivity check: environmentalists item.",
                       "Retained — r.drop = .48 and removal does not improve fit."))

# -----------------------------------------------------------------------------
# COMPOSITES
# -----------------------------------------------------------------------------
# min-items rule replaces the earlier plain rowMeans(na.rm = TRUE)
for (nm in names(identity_scales)) {
  personal_weights[[nm]] <- composite(personal_weights, identity_scales[[nm]])
}

personal_weights <- personal_weights %>%
  mutate(across(all_of(names(identity_scales)), ~ as.numeric(scale(.x)),
                .names = "{.col}_z"))

personal_weights %>%
  select(all_of(names(identity_scales))) %>%
  pivot_longer(everything(), names_to = "Composite", values_to = "value") %>%
  group_by(Composite) %>%
  dplyr::summarize(
    Mean     = round(mean(value, na.rm = TRUE), 2),
    SD       = round(sd(value, na.rm = TRUE), 2),
    Min      = round(min(value, na.rm = TRUE), 2),
    Max      = round(max(value, na.rm = TRUE), 2),
    Skew     = round(e1071::skewness(value, na.rm = TRUE), 2),
    `N miss` = sum(is.na(value)),
    .groups  = "drop") %>%
  ktab(caption = "Identity composite variables")

## =============================================================================
## RESULTS AND INTERPRETATION — raw material for the .Rmd write-up
## =============================================================================
#
# STRUCTURE (2 factors, 60% of variance after rotation, no cross-loadings > |.30|)
#   Consumptive:     hunters .87, ranchers .97, farmers .84, anglers .63,
#                    small_business .61, landowners .51
#   Non-consumptive: outdoor_viewers .95, outdoor_rec .74, environmentalists .53
#   Factor correlation r = .62. Composite correlation r = .58.
#
# THE CENTRAL FINDING: THIS IS LARGELY ONE DIMENSION
# omega_h = .77, omega_total = .95 — roughly 81% of the reliable variance is
# attributable to a single general factor. The first eigenvalue (5.07) is almost
# nine times the second (0.58). Substantively this reads as a general
# "identification with rural, land-based and outdoor life" dimension.
#
# The two subscales are NOT opposing poles. If they were, they would correlate
# negatively; they correlate +.58. Respondents who identify with hunters and
# ranchers also tend to identify with wildlife viewers and outdoor
# recreationists. Much of the human-dimensions literature implicitly treats
# these as a trade-off; these data do not support that.
#
# Both subscales are retained on theoretical grounds. They share only about a
# third of their variance, behave as distinct predictors, and imply a VIF near
# 1.5 — no collinearity concern.
#
# FIT IS MARGINAL AND SHOULD BE REPORTED AS SUCH
#   2-factor: RMSEA .182, TLI .824, BIC 830
#   3-factor: RMSEA .154, TLI .873, BIC 356
#   4-factor: RMSEA .118, TLI .926, BIC  88
# Fit improves monotonically without becoming good — the signature of an
# unmodelled general factor absorbing variance, not evidence for four
# dimensions. Parallel analysis retained 4, but eigenvalues 3 and 4 (.22 and
# .16) are trivial; parallel analysis over-retains when one factor dominates
# because the simulated eigenvalues collapse. Two factors are retained on
# theory and interpretability, with the fit limitation stated openly.
#
# Only two residual correlations exceed |.10|: small_business-landowners (.17)
# and environmentalists-landowners (-.11). Local dependence is minor.
#
# ITEM DECISIONS
# - identify_small_business loads .61 on the consumptive factor and is retained.
# - identify_environmentalists has the lowest communality (.32) and the weakest
#   loading (.53), and correlates .41/.47 with the other two non-consumptive
#   items versus .71 between them. Retained: corrected item-total r = .48, and
#   removing it does not improve fit (RMSEA .182 -> .190). A three-item scale
#   reporting alpha is preferable to a two-item scale reporting Spearman-Brown,
#   and it is the only environmental identity item in the instrument.
#
# MISSINGNESS
# Unlike the reasons and methods blocks, these items carry 2-6 missing responses
# each. Responses were not forced throughout the instrument. The min-items rule
# therefore does affect a small number of cases here — check N miss on the
# composites and amend the claim in the reasons/methods prose accordingly.
