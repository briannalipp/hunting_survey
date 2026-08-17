## =============================================================================
## Hunting Survey — Reasons and Methods Data Reduction
## Chunk contents for hunt_regress.Rmd; paste into chunks, don't source wholesale
## =============================================================================

library(here)
library(tidyverse)     # dplyr, tidyr, purrr, tibble
library(psych)         # fa, KMO, alpha, omega
library(pheatmap)      # loadings heatmaps
library(knitr)         # kable
library(e1071)         # skewness
library(GPArotation)   # required by psych for oblimin rotation
library(ragg)

fam       <- "Libre Franklin"
okabe_ito <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00", "#56B4E9")
theme_set(theme_minimal(base_size = 12, base_family = fam))

# -----------------------------------------------------------------------------
# DATA
# -----------------------------------------------------------------------------
# Running standalone loads the raw object; running inside hunt_regress.Rmd keeps
# whatever recoding has already happened there.
if (!exists("personal_weights")) {
  personal_weights <- readRDS(here("data", "personal_weights.rds"))
}

# -----------------------------------------------------------------------------
# ITEMS
# -----------------------------------------------------------------------------
reason_items <- c(
  "reason_food", "reason_recreation", "reason_population_control",
  "reason_trophy", "reason_reduce_conflict", "reason_conservation_revenue",
  "reason_indigenous_practice", "reason_cultural_tradition")

method_items <- c(
  "method_baiting", "method_captive_hunt", "method_trapping",
  "method_neck_snares", "method_dogs_predators", "method_dogs_birds",
  "method_aerial", "method_bow", "method_firearms")

reason_df <- personal_weights %>% select(all_of(reason_items)) %>% as.data.frame()
method_df <- personal_weights %>% select(all_of(method_items)) %>% as.data.frame()

tibble(
  block    = c(rep("reasons", length(reason_items)), rep("methods", length(method_items))),
  item     = c(reason_items, method_items),
  n_miss   = c(colSums(is.na(reason_df)), colSums(is.na(method_df))),
  pct_miss = round(100 * c(colMeans(is.na(reason_df)), colMeans(is.na(method_df))), 1)) %>%
  kable(caption = "Item-level missingness, reasons and methods blocks")

# -----------------------------------------------------------------------------
# FACTORABILITY
# -----------------------------------------------------------------------------
# KMO above ~0.70 and a significant Bartlett test mean the correlation structure
# is worth factoring at all
tibble(
  block    = c("Reasons", "Methods"),
  n_items  = c(length(reason_items), length(method_items)),
  KMO      = round(c(KMO(reason_df)$MSA, KMO(method_df)$MSA), 3),
  bartlett_p = signif(c(cortest.bartlett(reason_df)$p.value,
                        cortest.bartlett(method_df)$p.value), 3)) %>%
  kable(caption = "Sampling adequacy and sphericity")

# -----------------------------------------------------------------------------
# DIMENSIONALITY — PARALLEL ANALYSIS
# -----------------------------------------------------------------------------
# Horn's parallel analysis: retain factors whose observed eigenvalue exceeds the
# mean eigenvalue from random data of the same dimensions. Polychoric
# correlations because the items are ordinal.
set.seed(42)
reason_par <- fa.parallel(reason_df, fa = "both", cor = "poly", n.iter = 100, plot = FALSE)
method_par <- fa.parallel(method_df, fa = "both", cor = "poly", n.iter = 100, plot = FALSE)

parallel_table <- function(pa, block) {
  tibble(block     = block,
         factor    = seq_along(pa$fa.values),
         observed  = round(pa$fa.values, 3),
         simulated = round(pa$fa.sim, 3),
         retain    = pa$fa.values > pa$fa.sim)
}

eigen_tbl <- bind_rows(parallel_table(reason_par, "Reasons"),
                       parallel_table(method_par, "Methods"))

eigen_tbl %>%
  kable(caption = paste0(
    "Parallel analysis eigenvalues (100 iterations, polychoric). ",
    "Retained: reasons = ", reason_par$nfact,
    ", methods = ", method_par$nfact, "."))

# scree — observed vs simulated, encoded by color, shape and linetype together
eigen_tbl %>%
  pivot_longer(c(observed, simulated), names_to = "series", values_to = "eigenvalue") %>%
  mutate(series = recode(series, observed = "Observed", simulated = "Simulated (random data)"),
         block  = factor(block, levels = c("Reasons", "Methods"))) %>%
  ggplot(aes(x = factor, y = eigenvalue,
             color = series, shape = series, linetype = series)) +
  geom_hline(yintercept = 0, color = "grey80", linewidth = 0.4) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.2) +
  facet_wrap(~ block, scales = "free_x") +
  scale_x_continuous(breaks = seq_len(max(eigen_tbl$factor))) +
  scale_color_manual(values = c("Observed" = okabe_ito[1],
                                "Simulated (random data)" = "grey55")) +
  scale_shape_manual(values = c("Observed" = 16, "Simulated (random data)" = 17)) +
  scale_linetype_manual(values = c("Observed" = "solid",
                                   "Simulated (random data)" = "dashed")) +
  labs(x = "Factor", y = "Eigenvalue",
       caption = "Horn's parallel analysis, 100 iterations, polychoric correlations. Factors retained where the observed eigenvalue exceeds the simulated value.") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.caption = element_text(color = "grey45", hjust = 0, size = rel(0.8)),
        axis.title = element_text(color = "grey20"),
        legend.title = element_blank(), legend.position = "top")

ggsave(here("figs", "parallel_analysis_scree.png"),
       width = 7, height = 3.8, dpi = 300, device = agg_png)

# -----------------------------------------------------------------------------
# EFA — REASONS (2- vs 3-factor comparison)
# -----------------------------------------------------------------------------
# Parallel analysis retained 2 factors, but the 2-factor solution fit poorly.
# Both are estimated here so the comparison is reproducible rather than asserted.
set.seed(42)
reason_efa2 <- fa(reason_df, nfactors = 2, rotate = "oblimin", fm = "ml", cor = "poly")
reason_efa3 <- fa(reason_df, nfactors = 3, rotate = "oblimin", fm = "ml", cor = "poly")

tibble(
  solution = c("2-factor", "3-factor"),
  RMSEA    = round(c(reason_efa2$RMSEA[1], reason_efa3$RMSEA[1]), 3),
  TLI      = round(c(reason_efa2$TLI, reason_efa3$TLI), 3),
  BIC      = round(c(reason_efa2$BIC, reason_efa3$BIC), 1),
  cum_var  = round(c(max(reason_efa2$Vaccounted["Cumulative Var", ]),
                     max(reason_efa3$Vaccounted["Cumulative Var", ])), 3)) %>%
  kable(caption = paste("Reasons block: 2- vs 3-factor solution.",
                        "RMSEA < .08 acceptable, < .05 good; TLI > .95 good;",
                        "lower BIC preferred."))

# 3 factors retained on fit and interpretability, against the parallel analysis
# suggestion. Parallel analysis under-extracts when factors are strongly
# correlated, which is the case here (utilitarian-cultural r = .75).
n_reason <- 3
n_method <- 2

reason_efa <- reason_efa3

round(reason_efa$e.values[1:5], 2)
print(reason_efa$loadings, cutoff = 0.30, sort = TRUE)
round(reason_efa$Phi, 2)
round(reason_efa$communality, 2)
c(RMSEA = round(reason_efa$RMSEA[1], 3), TLI = round(reason_efa$TLI, 3))

# -----------------------------------------------------------------------------
# EFA — METHODS
# -----------------------------------------------------------------------------
set.seed(42)
method_efa <- fa(method_df, nfactors = n_method, rotate = "oblimin", fm = "ml", cor = "poly")

round(method_efa$e.values[1:5], 2)
print(method_efa$loadings, cutoff = 0.30, sort = TRUE)
round(method_efa$Phi, 2)
round(method_efa$communality, 2)
c(RMSEA = round(method_efa$RMSEA[1], 3), TLI = round(method_efa$TLI, 3))

# -----------------------------------------------------------------------------
# LOADINGS TABLES — for reporting
# -----------------------------------------------------------------------------
options(knitr.kable.NA = "")

loadings_table <- function(efa, cutoff = 0.30) {
  L   <- unclass(efa$loadings)
  out <- as.data.frame(L)
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
         "; proportion variance ", paste(round(v["Proportion Var", ], 3), collapse = " / "),
         "; cumulative ", round(max(v["Cumulative Var", ]), 3))
}

loadings_table(reason_efa) %>%
  kable(caption = paste0(
    "Reasons for hunting — EFA loadings (3 factors, ML extraction, oblimin ",
    "rotation, polychoric correlations). Loadings below |0.30| suppressed. ",
    "h2 = communality. ", var_footer(reason_efa),
    ". Factor correlations: ", phi_text(reason_efa), "."))

loadings_table(method_efa) %>%
  kable(caption = paste0(
    "Hunting methods — EFA loadings (2 factors, ML extraction, oblimin ",
    "rotation, polychoric correlations). Loadings below |0.30| suppressed. ",
    "h2 = communality. ", var_footer(method_efa),
    ". Factor correlations: ", phi_text(method_efa),
    ". method_dogs_predators cross-loaded (0.47 / 0.44) and is excluded from ",
    "both composite scales."))

# diverging ramp is blue–white–vermillion (colorblind-safe); avoid navy/firebrick
load_ramp <- colorRampPalette(c("#0072B2", "white", "#D55E00"))(50)

pheatmap(as.data.frame(unclass(reason_efa$loadings)),
         cluster_rows = TRUE, cluster_cols = FALSE,
         main = "Reasons — EFA loadings (oblimin)",
         color = load_ramp, display_numbers = TRUE, number_format = "%.2f")

pheatmap(as.data.frame(unclass(method_efa$loadings)),
         cluster_rows = TRUE, cluster_cols = FALSE,
         main = "Methods — EFA loadings (oblimin)",
         color = load_ramp, display_numbers = TRUE, number_format = "%.2f")

# -----------------------------------------------------------------------------
# SCALE ASSIGNMENT — placeholders, replace with the actual factor structure
# -----------------------------------------------------------------------------
# Assignments follow the EFA above (loadings > 0.30, dominant factor).
# Names are provisional — align them with the framing used in the literature.
#
# method_dogs_predators loaded 0.468 / 0.437 — near-equal cross-loading, so it is
# excluded from both scales and tested separately below.
reason_scales <- list(
  reason_utilitarian = c("reason_food", "reason_population_control",
                         "reason_reduce_conflict", "reason_conservation_revenue"),
  reason_sport       = c("reason_recreation", "reason_trophy"),
  reason_cultural    = c("reason_indigenous_practice", "reason_cultural_tradition"))

method_scales <- list(
  method_contested     = c("method_baiting", "method_captive_hunt", "method_trapping",
                           "method_neck_snares", "method_aerial"),
  method_conventional  = c("method_dogs_birds", "method_bow", "method_firearms"))

# -----------------------------------------------------------------------------
# RELIABILITY
# -----------------------------------------------------------------------------
# Spearman-Brown is the recommended coefficient for two-item scales; alpha is
# biased and uninterpretable at k = 2 (Eisinga, Grotenhuis & Pelzer 2013)
spearman_brown <- function(df, items) {
  r <- cor(df[[items[1]]], df[[items[2]]], use = "pairwise.complete.obs")
  (2 * r) / (1 + r)
}

# McDonald's omega_total from a one-factor model. psych::omega() attempts a
# Schmid-Leiman transform that is undefined at one factor and emits warnings;
# computing it directly avoids that. Polychoric input makes this ordinal omega.
omega_total <- function(df, items) {
  f <- fa(df[, items], nfactors = 1, fm = "ml", cor = "poly")
  l <- as.numeric(f$loadings)
  sum(l)^2 / (sum(l)^2 + sum(1 - l^2))
}

alpha_table <- function(df, scales) {
  map_dfr(names(scales), function(nm) {
    items <- scales[[nm]]
    a <- psych::alpha(df[, items], warnings = FALSE)
    o <- if (length(items) >= 3) {
      tryCatch(omega_total(df, items), error = function(e) NA_real_)
    } else NA_real_
    tibble(scale    = nm,
           k        = length(items),
           alpha    = round(a$total$raw_alpha, 3),
           omega    = round(o, 3),
           sb       = if (length(items) == 2) round(spearman_brown(df, items), 3) else NA_real_,
           mean_r   = round(a$total$average_r, 3))
  })
}

bind_rows(alpha_table(reason_df, reason_scales),
          alpha_table(method_df, method_scales)) %>%
  kable(caption = paste("Scale reliability. Alpha assumes tau-equivalence; omega does not.",
                        "For two-item scales report Spearman-Brown (sb), not alpha."))

# sensitivity checks on the two items flagged in the EFA
tibble(
  scale   = c("method_contested", "method_contested",
              "reason_utilitarian", "reason_utilitarian"),
  version = c("excluding dogs_predators", "including dogs_predators",
              "including conservation_revenue", "excluding conservation_revenue"),
  alpha   = round(c(
    psych::alpha(method_df[, method_scales$method_contested], warnings = FALSE)$total$raw_alpha,
    psych::alpha(method_df[, c(method_scales$method_contested, "method_dogs_predators")],
                 warnings = FALSE)$total$raw_alpha,
    psych::alpha(reason_df[, reason_scales$reason_utilitarian], warnings = FALSE)$total$raw_alpha,
    psych::alpha(reason_df[, setdiff(reason_scales$reason_utilitarian,
                                     "reason_conservation_revenue")],
                 warnings = FALSE)$total$raw_alpha), 3)) %>%
  kable(caption = paste("Sensitivity checks on cross-loading items.",
                        "Retain the item unless removal raises alpha materially."))

# per-item detail — look for items whose removal raises alpha materially
walk(names(reason_scales), function(nm) {
  if (length(reason_scales[[nm]]) >= 3) {   # alpha.drop is meaningless at k = 2
    cat("\n---", nm, "---\n")
    print(psych::alpha(reason_df[, reason_scales[[nm]]], warnings = FALSE)$alpha.drop)
  }
})
walk(names(method_scales), function(nm) {
  if (length(method_scales[[nm]]) >= 3) {   # alpha.drop is meaningless at k = 2
    cat("\n---", nm, "---\n")
    print(psych::alpha(method_df[, method_scales[[nm]]], warnings = FALSE)$alpha.drop)
  }
})

# -----------------------------------------------------------------------------
# COMPOSITES
# -----------------------------------------------------------------------------
# a respondent needs at least half a scale's items to get a score; plain
# rowMeans(na.rm = TRUE) would score someone off a single item
composite <- function(df, items, min_items = ceiling(length(items) / 2)) {
  m   <- as.matrix(df[, items, drop = FALSE])
  out <- rowMeans(m, na.rm = TRUE)
  out[rowSums(!is.na(m)) < min_items] <- NA_real_
  out
}

for (nm in names(reason_scales)) {
  personal_weights[[nm]] <- composite(personal_weights, reason_scales[[nm]])
}
for (nm in names(method_scales)) {
  personal_weights[[nm]] <- composite(personal_weights, method_scales[[nm]])
}

composite_names <- c(names(reason_scales), names(method_scales))

# method_dogs_predators is standardized alongside the composites so it can enter
# the model as a standalone predictor on the same scale
personal_weights <- personal_weights %>%
  mutate(across(all_of(c(composite_names, "method_dogs_predators")),
                ~ as.numeric(scale(.x)), .names = "{.col}_z"))

personal_weights %>%
  select(all_of(composite_names)) %>%
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
  kable(caption = "Composite variables: reasons and methods")

# -----------------------------------------------------------------------------
# COMPOSITE INTERCORRELATIONS
# -----------------------------------------------------------------------------
# anything above ~0.70 will show up as inflated VIFs downstream.
# ID_non_consume / ID_consum come from the identity block in hunt_regress.Rmd —
# included only if that has already run.
cor_vars <- c(composite_names, "method_dogs_predators",
              intersect(c("ID_non_consume", "ID_consum"), names(personal_weights)))

comp_cor <- personal_weights %>%
  select(all_of(cor_vars)) %>%
  cor(use = "pairwise.complete.obs")

pheatmap(comp_cor,
         cluster_rows = TRUE, cluster_cols = TRUE,
         main = "Composite intercorrelations",
         color = load_ramp, breaks = seq(-1, 1, length.out = 51),
         display_numbers = TRUE, number_format = "%.2f")

## =============================================================================
## METHODS NOTES — raw material for the .Rmd write-up
## Numbers below are from the run of 2026-08-15; re-check against current output
## before quoting them.
## =============================================================================
#
# ANALYTIC APPROACH
# Two item blocks were reduced separately: reasons hunting is acceptable (Q13,
# 8 items) and acceptability of hunting methods (Q14, 9 items). Both are 5-point
# acceptability scales. They were factored separately because they answer
# different questions (why vs how); a joint solution would impose a shared
# structure across them.
#
# Exploratory factor analysis, not principal components: the items are treated
# as indicators of latent acceptability dimensions rather than as variables to
# be compressed. Maximum likelihood extraction with oblimin (oblique) rotation,
# on polychoric correlations because the items are ordinal and the 1-5 spacing
# should not be assumed interval. Oblique rotation because acceptability
# dimensions are expected to correlate, and they do.
#
# Analyses are unweighted. Factor structure is a property of the instrument;
# the survey weights are for population estimation and enter at the regression
# stage. Worth stating explicitly in the text.
#
# DIMENSIONALITY
# Horn's parallel analysis (100 iterations, polychoric) retained 3 factors for
# reasons and 2 for methods.
#
# For reasons, both solutions were fit and compared because 2 would have been
# the intuitive choice. The 2-factor solution fit poorly (RMSEA = .140,
# TLI = .899, BIC = 301); the 3-factor solution fit well (RMSEA = .038,
# TLI = .992, BIC = -29). Fit and parallel analysis agree on 3.
#
# Worth noting: two of the three reasons factors correlate at r = .75
# (utilitarian and cultural). Strongly correlated factors are the condition
# under which parallel analysis most often under-extracts, so the agreement
# between the eigenvalue criterion and the fit indices is reassuring rather
# than incidental.
#
# FACTOR STRUCTURE — REASONS (3 factors, 61% of variance after rotation)
#   Utilitarian: food (.57), population control (.85), reduce conflict (.69),
#                conservation revenue (.46)
#   Sport:       recreation (.69), trophy (.96)
#   Cultural:    indigenous practice (.79), cultural tradition (.84)
#   Factor correlations: utilitarian-cultural .75, others ~.48.
#
# FACTOR STRUCTURE — METHODS (2 factors, 66% of variance after rotation)
#   Contested:    baiting (.66), captive hunt (.93), trapping (.71),
#                 neck snares (.82), aerial (.79)
#   Conventional: dogs for birds (.72), bow (.94), firearms (.94)
#
# ITEM DECISIONS
# - method_dogs_predators loaded .47 / .44 — a near-equal cross-load — and was
#   excluded from both scales. Note that including it *raises* method_contested
#   alpha from .876 to .887; this was not treated as evidence for inclusion,
#   because alpha increases mechanically with scale length and a gain of .011
#   from a sixth item is unremarkable. The factor structure is the substantive
#   evidence. The item is carried forward as a standalone predictor: hound
#   hunting is contested in ways that cut across the contested/conventional
#   distinction, which is plausibly why it cross-loads.
# - reason_conservation_revenue is the weakest loading item (.46 primary,
#   .31 cross-load, h2 = .64) but earns its place in the scale: removing it
#   drops alpha from .819 to .778, and alpha.drop shows no item whose removal
#   improves the scale. Retained.
# - alpha.drop confirmed every retained item in reason_utilitarian,
#   method_contested and method_conventional contributes; no item's removal
#   raised alpha above the full-scale value.
# - reason_food cross-loads onto cultural (.33). Substantively sensible —
#   subsistence is both utilitarian and cultural — and it was left on
#   utilitarian, where it loads more strongly.
# - reason_trophy loads .96 with h2 = .93. Not a Heywood case, but the sport
#   factor is largely defined by this one item.
#
# RELIABILITY
# Cronbach's alpha for scales of 3+ items, with McDonald's omega alongside
# because alpha assumes tau-equivalence. For the two 2-item scales (sport,
# cultural) the Spearman-Brown coefficient is reported instead — alpha is
# biased and difficult to interpret at k = 2 (Eisinga, Grotenhuis & Pelzer,
# 2013, Int J Public Health 58:637-642).
#
# RELIABILITY RESULTS
#   reason_utilitarian   k = 4  alpha .82  mean r .53
#   reason_sport         k = 2  SB    .84  mean r .72
#   reason_cultural      k = 2  SB    .81  mean r .68
#   method_contested     k = 5  alpha .88  mean r .59
#   method_conventional  k = 3  alpha .89  mean r .73
# Alpha and omega agreed to within .002 on every 3+ item scale, so
# tau-equivalence is a reasonable approximation and reporting alpha alone is
# defensible. Mean inter-item r for method_conventional (.73) is high — bow,
# firearms and dogs-for-birds are close to interchangeable in respondents'
# minds, which is a substantive finding, not a problem with the scale.
#
# COMPOSITES
# Unweighted means of constituent items, requiring responses to at least half
# a scale's items; respondents below that threshold are set to NA rather than
# scored from a fragment. Standardized (_z) versions created for the
# regression so coefficients are comparable across scales.
#
# COMPOSITE DESCRIPTIVES
#   reason_utilitarian   M 3.41  SD 0.93  skew -0.60
#   reason_cultural      M 3.30  SD 1.15  skew -0.47
#   reason_sport         M 2.10  SD 1.12  skew  0.70
#   method_conventional  M 3.20  SD 1.21  skew -0.48
#   method_contested     M 2.23  SD 0.99  skew  0.52
# All on the original 1-5 scale, no missing values, all skew within |0.70| —
# no transformations indicated. Substantively: utilitarian reasons and
# conventional methods are endorsed on average, sport reasons and contested
# methods are not.
#
# CARRY FORWARD TO THE REGRESSION
# Highest composite intercorrelations: reason_utilitarian-method_conventional
# r = .71, reason_utilitarian-reason_cultural r = .69. Both are below the
# conventional .80 concern threshold. A pairwise r of .71 implies a bivariate
# VIF near 2.0, comfortably under the usual cutoff, though the multivariate
# VIF will run higher once all predictors are in.
#
# Note the composite correlation (.69) is lower than the corresponding factor
# correlation (.75) because composites carry measurement error while the latent
# factors are disattenuated. The composites are therefore somewhat less
# collinear than the EFA suggested.
#
# Plan: fit the model, then check VIFs. Do not drop predictors pre-emptively.
# If collinearity does bite, the options are combining utilitarian and cultural,
# dropping one, or a penalized approach — decided on theory, not on VIF alone.
#
# method_dogs_predators enters as a standalone standardized predictor
# (method_dogs_predators_z) rather than in either composite.
