## =============================================================================
## Hunting Survey — Design-Based Regression
## Chunk contents for hunt_regress.Rmd. Replaces the all_diagnostics scaffolding.
## Assumes the setup chunk and the Model Variables section have run: helpers,
## personal_weights with _z predictors, age_group, income_cat, design.
## =============================================================================

library(MASS)   # svyolr needs it; loaded HERE not in setup — MASS::select masks dplyr::select
library(car)    # vif

# -----------------------------------------------------------------------------
# REBUILD CATEGORICAL PREDICTORS
# -----------------------------------------------------------------------------
# Rebuilt here rather than assumed, so this file does not depend on which
# version of these factors happens to be in the session. Income labels drop the
# dollar signs: MathJax reads $...$ as inline maths and eats the label text.
personal_weights <- personal_weights %>%
  mutate(
    age_group = factor(na_if(trimws(as.character(age_group4)), ""),
                       levels = c("18-29", "30-49", "50-64", "65+")),
    income_cat = factor(na_if(trimws(as.character(income)), ""),
                        levels = c("Less than $15,000",
                                   "$15,000 to under $50,000",
                                   "$50,000 to under $75,000",
                                   "$75,000 to under $100,000",
                                   "$100,000 to under $150,000",
                                   "$150,000 to under $200,000",
                                   "More than $200,000"),
                        labels = c("Under 15k", "15-50k", "50-75k", "75-100k",
                                   "100-150k", "150-200k", "200k+")))

# -----------------------------------------------------------------------------
# PREDICTOR BLOCKS
# -----------------------------------------------------------------------------
# Entered in cognitive-hierarchy order: stable characteristics first, then
# experience, then values and basic beliefs, then identity, then the reasons and
# methods scales — which sit closest to the outcome and are entered last.
# Specification follows an assumed causal ordering:
#
#   demographics -> values -> identity -> support -> participation
#
# Everything on the right-hand side must be PRIOR to the outcome. Two variables
# that were in earlier drafts are now excluded on those grounds:
#
#   has_hunted        — having hunted is a consequence of supporting hunting,
#                       not a cause of it. Conditioning on a descendant of the
#                       outcome biases every other coefficient toward zero, and
#                       is what drove identity's delta-R2 down to .008.
#   reasons / methods — accepting particular reasons and methods is part of what
#                       supporting hunting MEANS, not an antecedent of it. Their
#                       removal also disposes of the R2 = .68 problem and of the
#                       method_contested/trapping tautology, since
#                       method_contested contained two trapping items.
#
# knows_hunter and game_meat are retained as EXPOSURE rather than behaviour:
# both are largely a function of who is around you rather than choices that
# express support. Worth defending explicitly in the text — a reviewer may read
# eating game meat as downstream of support.
#
# eats_meat_n sits with demographics: a stable dietary position, not a
# consequence of views about hunting.
blocks <- list(
  demographics = c("age_group", "income_cat", "female", "college_grad",
                   "has_pets_n", "eats_meat_n"),
  exposure     = c("knows_hunter", "game_meat"),
  values       = c("wvo_mutualism_z", "wvo_domination_z",
                   "political_ideology_z", "christian_nationalism_score_z"),
  identity     = c("ID_consum_z", "ID_non_consume_z"))

all_predictors  <- unlist(blocks, use.names = FALSE)
trap_predictors <- all_predictors   # no reasons/methods, so no tautology to avoid
blocks_trap     <- blocks

# -----------------------------------------------------------------------------
# HELPERS
# -----------------------------------------------------------------------------
build_formula <- function(dv, preds) {
  f <- as.formula(paste(dv, "~", paste(preds, collapse = " + ")))
  # as.formula() attaches the environment of this function, which then becomes
  # where lm() looks up anything not found in `data`. Point it at the global
  # environment so lookup is predictable rather than trapped in a dead frame.
  environment(f) <- globalenv()
  f
}

# R2 for a gaussian svyglm: 1 - residual deviance / null deviance
r2_svyglm <- function(m) 1 - (m$deviance / m$null.deviance)

# survey::AIC.svyglm returns a named vector, not a scalar
ic_value <- function(m, what = "AIC") {
  v <- tryCatch(AIC(m), error = function(e) NA_real_)
  if (length(v) > 1) unname(v[["AIC"]]) else unname(v)
}

# survey::BIC.svyglm REQUIRES a maximal model — it needs the largest model in the
# comparison set to work out the effective degrees of freedom. Calling BIC(m)
# alone errors, which is why the column came back empty.
bic_value <- function(m, maximal) {
  v <- tryCatch(BIC(m, maximal = maximal), error = function(e) NA_real_)
  if (length(v) > 1) unname(v[["BIC"]]) else unname(v)
}

# -----------------------------------------------------------------------------
# COMPLETE-CASE DESIGN
# -----------------------------------------------------------------------------
# Nested models must run on identical cases or the R2 comparison is meaningless.
model_cc <- personal_weights %>%
  filter(if_all(all_of(c(outcome_vars, all_predictors)), ~ !is.na(.)))

design_cc <- model_cc %>% as_survey_design(ids = 1, weights = weight)

cat("Complete cases used in all models:", nrow(model_cc),
    "of", nrow(personal_weights), "\n")

# -----------------------------------------------------------------------------
# PRE-REGRESSION DIAGNOSTICS
# -----------------------------------------------------------------------------

## --- 1. Weight distribution -------------------------------------------------
# Extreme weights inflate variance more than they correct bias. A weight CV
# above ~1.0, or a max/min ratio in the tens, is worth reporting and may justify
# trimming. Kish's effective n is the sample size an unweighted design would
# need to achieve the same precision.
# NOT named `w` — a global `w` gets picked up by lm(weights = w) inside the
# bootstrap below, silently overriding the replicate weights.
wts <- model_cc$weight

tibble(n              = length(wts),
       min            = round(min(wts), 3),
       max            = round(max(wts), 3),
       `max/min`      = round(max(wts) / min(wts), 1),
       `CV of weights` = round(sd(wts) / mean(wts), 3),
       `Kish eff. n`  = round(sum(wts)^2 / sum(wts^2)),
       `Design effect` = round(length(wts) * sum(wts^2) / sum(wts)^2, 3)) %>%
  ktab(caption = "Weight distribution and Kish effective sample size")

## --- 2. Design effects on the outcomes --------------------------------------
# How much the weighting inflates the variance of each estimate. DEFF near 1
# means the weights cost little precision.
# as.data.frame() drops the deff attribute, so pull it with the extractor
dm <- svymean(~ hunting_support + trapping_support, design_cc, deff = TRUE)

tibble(variable = names(coef(dm)),
       mean     = round(as.numeric(coef(dm)), 3),
       SE       = round(as.numeric(SE(dm)), 3),
       deff     = round(as.numeric(deff(dm)), 3)) %>%
  ktab(caption = paste("Design effects on the outcome means.",
                       "deff near 1 means the weights cost little precision."))

## --- 3. Condition indices ---------------------------------------------------
# VIF catches pairwise inflation; condition indices catch near-dependencies
# among several predictors at once, which VIF can miss. Belsley-Kuh-Welsch:
# a condition index above 30 signals a serious near-dependency.
X  <- model.matrix(build_formula("hunting_support", all_predictors), data = model_cc)
Xs <- scale(X, center = FALSE, scale = sqrt(colSums(X^2)))
sv <- svd(Xs)$d

tibble(component       = seq_along(sv),
       singular_value  = round(sv, 4),
       condition_index = round(max(sv) / sv, 1)) %>%
  slice_max(condition_index, n = 5) %>%
  ktab(caption = paste0("Five largest condition indices (overall condition ",
                        "number = ", round(max(sv) / min(sv), 1),
                        "). Above 30 indicates a serious near-dependency."))

## --- 4. Predictor correlations ----------------------------------------------
# The largest pairwise correlations among continuous predictors, as a plain
# cross-check on the VIF and condition-index results.
cont_in_model <- intersect(all_predictors, names(model_cc))
cont_in_model <- cont_in_model[map_lgl(model_cc[cont_in_model], is.numeric)]

cor(model_cc[, cont_in_model], use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("v1") %>%
  pivot_longer(-v1, names_to = "v2", values_to = "r") %>%
  filter(v1 < v2) %>%
  slice_max(abs(r), n = 10) %>%
  mutate(r = round(r, 2)) %>%
  ktab(caption = "Ten largest pairwise predictor correlations")

## --- 5. Influential observations --------------------------------------------
# Computed on an unweighted lm over the same cases: leverage and Cook's D are
# properties of the design matrix and residuals. A handful above the 4/n
# threshold is normal; a large cluster is not.
infl_fit <- lm(build_formula("hunting_support", all_predictors), data = model_cc)
cd       <- cooks.distance(infl_fit)
hats     <- hatvalues(infl_fit)
p        <- length(coef(infl_fit))

tibble(`Cook's D > 4/n`        = sum(cd > 4 / nrow(model_cc), na.rm = TRUE),
       `Cook's D > 1`          = sum(cd > 1, na.rm = TRUE),
       `Max Cook's D`          = round(max(cd, na.rm = TRUE), 3),
       `High leverage (> 2p/n)` = sum(hats > 2 * p / nrow(model_cc), na.rm = TRUE)) %>%
  ktab(caption = "Influence diagnostics. Cook's D above 1 is the level that matters.")

## --- 6. Linearity of continuous predictors ----------------------------------
# Each continuous predictor gets a quadratic term added to the full model in
# turn; a significant quadratic means the linear term is mis-specified.
# Interpret with the number of tests in mind — one or two small p-values across
# twelve tests is what chance produces.
z_preds <- all_predictors[str_detect(all_predictors, "_z$")]

map_dfr(z_preds, function(v) {
  f <- build_formula("hunting_support", c(all_predictors, paste0("I(", v, "^2)")))
  m <- svyglm(f, design = design_cc)
  s <- summary(m)$coefficients
  row <- grep(paste0("I\\(", v, "\\^2\\)"), rownames(s), value = TRUE)
  tibble(predictor = v,
         quad_b    = round(s[row, 1], 3),
         quad_p    = round(s[row, 4], 4))
}) %>%
  arrange(quad_p) %>%
  ktab(caption = paste("Linearity check: p-value for a quadratic term added to",
                       "the full model, one predictor at a time (hunting_support)."))

## --- 7. Sparse cells for the ordinal model ----------------------------------
# svyolr can behave badly when a factor level has few or no cases at an outcome
# level. Check before fitting rather than debugging a failure afterwards.
table(model_cc$age_group, model_cc$hunting_support) %>%
  as.data.frame.matrix() %>%
  rownames_to_column("age_group") %>%
  ktab(caption = "Age band by hunting support: cell counts")

table(model_cc$income_cat, model_cc$hunting_support) %>%
  as.data.frame.matrix() %>%
  rownames_to_column("income_cat") %>%
  ktab(caption = "Income by hunting support: cell counts")

# -----------------------------------------------------------------------------
# HIERARCHICAL FIT
# -----------------------------------------------------------------------------
fit_blocks <- function(dv, design_obj, block_list = blocks) {
  cumulative <- Reduce(c, block_list, accumulate = TRUE)
  map(cumulative, ~ svyglm(build_formula(dv, .x), design = design_obj)) %>%
    setNames(names(block_list))
}

block_table <- function(dv, models) {
  r2      <- map_dbl(models, r2_svyglm)
  maximal <- models[[length(models)]]
  tibble(
    Step       = seq_along(models),
    Block      = paste0("+ ", names(models)),
    k          = map_int(models, ~ length(coef(.x)) - 1L),
    R2         = round(r2, 3),
    `Delta R2` = round(c(r2[1], diff(r2)), 3),
    AIC        = round(map_dbl(models, ic_value), 1),
    BIC        = round(map_dbl(models, bic_value, maximal = maximal), 1)) %>%
    ktab(caption = paste0("Hierarchical regression on ", dv,
                          " (design-weighted, cumulative blocks)"))
}

hunt_models <- fit_blocks("hunting_support",  design_cc)
trap_models <- fit_blocks("trapping_support", design_cc, block_list = blocks_trap)

block_table("hunting_support",  hunt_models)
block_table("trapping_support", trap_models)

# Wald tests for whether each block adds anything
walk2(list(hunt_models, trap_models), c("hunting_support", "trapping_support"),
      function(mods, dv) {
        cat("\n--- Block-addition Wald tests:", dv, "---\n")
        for (i in 2:length(mods)) {
          added <- blocks[[i]]
          cat("\n+", names(blocks)[i], "\n")
          print(regTermTest(mods[[i]], as.formula(paste("~", paste(added, collapse = " + "))),
                            method = "Wald"))
        }
      })

# -----------------------------------------------------------------------------
# FULL MODELS — LINEAR AND ORDINAL
# -----------------------------------------------------------------------------
# last element = the fully specified model, whatever the final block is named
hunt_lin <- hunt_models[[length(hunt_models)]]
trap_lin <- trap_models[[length(trap_models)]]

# ordinal needs the outcome as an ordered factor inside the design
design_ord <- model_cc %>%
  mutate(across(all_of(outcome_vars), ~ factor(.x, ordered = TRUE))) %>%
  as_survey_design(ids = 1, weights = weight)

hunt_ord <- svyolr(build_formula("hunting_support",  all_predictors),
                   design = design_ord)
trap_ord <- svyolr(build_formula("trapping_support", trap_predictors),
                   design = design_ord)

tidy_svyglm <- function(m) {
  s <- summary(m)$coefficients
  tibble(term     = rownames(s),
         estimate = s[, 1], se = s[, 2], p = s[, 4]) %>%
    filter(term != "(Intercept)")
}

tidy_svyolr <- function(m) {
  s <- summary(m)$coefficients
  keep <- rownames(s)[seq_len(length(coef(m)))]
  tibble(term = keep,
         estimate = s[keep, 1], se = s[keep, 2]) %>%
    mutate(p = 2 * pnorm(abs(estimate / se), lower.tail = FALSE))
}

stars <- function(p) case_when(p < .001 ~ "***", p < .01 ~ "**",
                               p < .05 ~ "*", TRUE ~ "")

coef_table <- function(lin, ord, dv) {
  tidy_svyglm(lin) %>%
    dplyr::select(term, b = estimate, b_se = se, b_p = p) %>%
    full_join(tidy_svyolr(ord) %>%
                dplyr::select(term, logit = estimate, logit_se = se, logit_p = p),
              by = "term") %>%
    mutate(`b (linear)`  = paste0(round(b, 3), stars(b_p)),
           `SE`          = round(b_se, 3),
           `OR (ordinal)` = paste0(round(exp(logit), 2), stars(logit_p)),
           `OR 95% CI`   = paste0("[", round(exp(logit - 1.96 * logit_se), 2), ", ",
                                  round(exp(logit + 1.96 * logit_se), 2), "]")) %>%
    dplyr::select(term, `b (linear)`, SE, `OR (ordinal)`, `OR 95% CI`) %>%
    ktab(caption = paste0(
      "Full model for ", dv, ". Linear coefficients are scale points per SD of ",
      "predictor; odds ratios are from the proportional-odds model. ",
      "* p<.05, ** p<.01, *** p<.001."))
}

coef_table(hunt_lin, hunt_ord, "hunting_support")
coef_table(trap_lin, trap_ord, "trapping_support")

tibble(model = c("hunting_support", "trapping_support"),
       R2_linear = round(c(r2_svyglm(hunt_lin), r2_svyglm(trap_lin)), 3)) %>%
  ktab(caption = "Variance explained, full linear models")

# -----------------------------------------------------------------------------
# COLLINEARITY
# -----------------------------------------------------------------------------
# VIF is a property of the design matrix, so it is computed on an unweighted lm
# over the same complete cases. GVIF^(1/(2*df)) is the comparable quantity when
# factors are present — square it to read on the usual VIF scale.
vif_fit <- lm(build_formula("hunting_support", all_predictors), data = model_cc)

as.data.frame(vif(vif_fit)) %>%
  rownames_to_column("term") %>%
  setNames(c("term", "GVIF", "df", "GVIF_adj")) %>%
  mutate(`VIF equivalent` = round(GVIF_adj^2, 2)) %>%
  arrange(desc(`VIF equivalent`)) %>%
  dplyr::select(term, df, `VIF equivalent`) %>%
  ktab(caption = paste("Collinearity diagnostics. VIF equivalent above 5 warrants",
                       "attention; above 10 is a problem."))

# -----------------------------------------------------------------------------
# PROPORTIONAL ODDS CHECK
# -----------------------------------------------------------------------------
# The Brant test does not accept survey-weighted models. Instead compare
# coefficients from binary logits fitted at each cut point: under proportional
# odds they should be similar. Large, systematic divergence means the assumption
# fails and the linear model becomes the more honest primary specification.
po_check <- function(dv, design_obj, preds) {
  cuts <- 2:5
  map_dfr(cuts, function(k) {
    d <- design_obj %>% mutate(.y = as.numeric(!!sym(dv) >= k))
    m <- svyglm(build_formula(".y", preds), design = d, family = quasibinomial())
    tidy_svyglm(m) %>% mutate(cut = paste0(">= ", k))
  }) %>%
    dplyr::select(term, cut, estimate) %>%
    pivot_wider(names_from = cut, values_from = estimate) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

po_check("hunting_support", design_cc, all_predictors) %>%
  ktab(caption = paste("Proportional odds check: logit coefficients at each cut",
                       "point for hunting_support. Similar values across columns",
                       "support the assumption."))

po_check("trapping_support", design_cc, trap_predictors) %>%
  ktab(caption = paste("Proportional odds check: logit coefficients at each cut",
                       "point for trapping_support."))

# -----------------------------------------------------------------------------
# MODEL SELECTION
# -----------------------------------------------------------------------------
# Selection here is among a small set of PRE-SPECIFIED theoretical models, not a
# search over subsets. See the notes below for why stepwise is not used.
#
ic_table <- function(models, dv) {
  maximal <- models[[length(models)]]
  tibble(Step  = seq_along(models),
         Model = paste0("+ ", names(models)),
         k     = map_int(models, ~ length(coef(.x)) - 1L),
         R2    = round(map_dbl(models, r2_svyglm), 3),
         AIC   = round(map_dbl(models, ic_value), 1),
         BIC   = round(map_dbl(models, bic_value, maximal = maximal), 1)) %>%
    ktab(caption = paste0("Nested model comparison, ", dv,
                          ". Lower AIC/BIC preferred; BIC penalises complexity",
                          " more heavily."))
}

ic_table(hunt_models, "hunting_support")
ic_table(trap_models, "trapping_support")

# Candidate specifications compared on theoretical grounds, not on p-values.
# The endogeneity-sensitive comparison is Model C against Model D.
candidates <- function(dv) {
  list(
    `A: demographics`          = blocks$demographics,
    `B: + exposure`            = c(blocks$demographics, blocks$exposure),
    `C: + values`              = c(blocks$demographics, blocks$exposure,
                                   blocks$values),
    `D: + identity (full)`     = all_predictors)
}

candidate_table <- function(dv) {
  specs   <- candidates(dv)
  mods    <- map(specs, ~ svyglm(build_formula(dv, .x), design = design_cc))
  maximal <- mods[[length(mods)]]
  tibble(Model = names(specs),
         k     = map_int(mods, ~ length(coef(.x)) - 1L),
         R2    = round(map_dbl(mods, r2_svyglm), 3),
         AIC   = round(map_dbl(mods, ic_value), 1),
         BIC   = round(map_dbl(mods, bic_value, maximal = maximal), 1)) %>%
    ktab(caption = paste0("Pre-specified candidate models, ", dv,
                          ". Blocks enter in assumed causal order; D is the ",
                          "full specification."))
}

candidate_table("hunting_support")
candidate_table("trapping_support")

# -----------------------------------------------------------------------------
# MEDIATION: DO VALUES OPERATE THROUGH IDENTITY?
# -----------------------------------------------------------------------------
# The cognitive hierarchy predicts values -> identity -> attitudes. Test whether
# the identity composites carry part of the value effects.
#
#   total effect    c  : support ~ demographics + exposure + values
#   direct effect   c' : support ~ demographics + exposure + values + identity
#   indirect effect    : c - c'
#
# Confidence intervals come from survey bootstrap replicate weights rather than
# a Sobel test — Sobel assumes a normal sampling distribution for a product of
# coefficients, which it does not have, and it ignores the design entirely.
value_terms <- blocks$values

# Built from a plain svydesign rather than the srvyr wrapper — as.svrepdesign()
# on a tbl_svy can return replicate weights that do not vary, which silently
# produces zero standard errors.
rep_design <- as.svrepdesign(
  svydesign(ids = ~ 1, weights = ~ weight, data = model_cc),
  type = "bootstrap", replicates = 500)

# sanity check: replicate weights must vary, or every SE below will be zero
stopifnot(mean(apply(weights(rep_design, type = "analysis"), 1, sd)) > 0)

# returns a tibble; indirect_effects() wraps it for display
indirect_tbl <- function(dv) {
  f_total  <- build_formula(dv, c(blocks$demographics, blocks$exposure,
                                  blocks$values))
  f_direct <- build_formula(dv, c(blocks$demographics, blocks$exposure,
                                  blocks$values, blocks$identity))

  # The replicate weights are attached to the data frame rather than passed by
  # name. lm() resolves `weights` inside `data` first, so a column reference
  # cannot be captured by an unrelated object of the same name elsewhere.
  boot <- withReplicates(rep_design, function(w, data) {
    data$.repw <- w
    b_tot <- coef(lm(f_total,  data = data, weights = .repw))[value_terms]
    b_dir <- coef(lm(f_direct, data = data, weights = .repw))[value_terms]
    b_tot - b_dir
  })

  m_tot <- svyglm(f_total,  design = design_cc)
  m_dir <- svyglm(f_direct, design = design_cc)

  tibble(
    predictor = value_terms,
    total     = round(coef(m_tot)[value_terms], 3),
    direct    = round(coef(m_dir)[value_terms], 3),
    indirect  = round(as.numeric(coef(boot)), 3),
    se        = round(as.numeric(SE(boot)), 3)) %>%
    mutate(
      ci_low   = indirect - 1.96 * se,
      ci_high  = indirect + 1.96 * se,
      mediated = ifelse(ci_low > 0 | ci_high < 0, "yes", "no"),
      `95% CI`     = paste0("[", round(ci_low, 3), ", ", round(ci_high, 3), "]"),
      `% of total` = ifelse(mediated == "yes",
                            paste0(round(100 * indirect / total), "%"), "")) %>%
    dplyr::select(predictor, total, direct, indirect, se,
                  `95% CI`, mediated, `% of total`)
}

indirect_effects <- function(dv) {
  indirect_tbl(dv) %>%
    ktab(caption = paste0("Indirect effects of values through identity on ", dv,
                          ". Bootstrap CIs from 500 survey replicate weights; an",
                          " interval excluding zero indicates mediation."))
}

set.seed(42)
indirect_effects("hunting_support")
indirect_effects("trapping_support")

# The a-paths: do values actually predict identity? Mediation requires this.
map_dfr(blocks$identity, function(m) {
  fit <- svyglm(build_formula(m, c(blocks$demographics, blocks$exposure,
                                   blocks$values)), design = design_cc)
  tidy_svyglm(fit) %>%
    filter(term %in% value_terms) %>%
    mutate(mediator = m, b = paste0(round(estimate, 3), stars(p))) %>%
    dplyr::select(mediator, term, b, se)
}) %>%
  ktab(caption = "a-paths: values predicting the identity composites")

## =============================================================================
## NOTES
## =============================================================================
#
# SPECIFICATION
# Design-based models throughout, fitted on `design_cc` — the complete-case
# subset — so that nested models are comparable. Weights come from the VeraSight
# panel; ids = 1 because the sample is not clustered.
#
# Both a linear (svyglm, gaussian) and an ordinal (svyolr, proportional odds)
# model are fitted for each outcome. The linear coefficients read as scale points
# per standard deviation of predictor and are easier to communicate; the ordinal
# model is the technically correct form for a 5-point ordered outcome. Report one
# as primary and cite the other as confirming, provided they agree in sign and
# significance.
#
# BLOCKS
# Predictors enter in cognitive-hierarchy order: demographics, experience,
# values and ideology, identity, then reasons and methods. Delta R2 at each step
# shows what each construct adds beyond the preceding ones.
#
# ENDOGENEITY — STATE THIS IN THE PAPER
# The reasons and methods composites are conceptually proximate to the outcomes.
# A respondent who rates contested methods as acceptable is arguably already
# expressing something close to hunting support. Their coefficients should be
# read as association, not explanation, and the R2 jump at the final block will
# overstate explanatory power. Entering them last makes that visible: compare
# R2 at the identity step against R2 at the final step.
#
# WHY NOT ORDINAL — JUSTIFY THE DEVIATION IN THE PAPER
# Ordinal (proportional-odds) models are increasingly the default for Likert
# outcomes, so treating a 5-point scale as interval needs an argued defence
# rather than silence. The defence here is empirical, not one of convenience.
#
# The proportional-odds assumption is violated. Logit coefficients estimated at
# each cut point diverge systematically for several central predictors
# (hunting_support, cut points >=2 / >=3 / >=4 / >=5):
#
#   game_meat        1.69  1.32  1.15  1.08   (declines steadily)
#   knows_hunter     0.98  0.89  0.82 -0.07   (vanishes and flips at the top)
#   ID_consum_z      0.09  0.52  0.40  0.62   (grows several-fold)
#   female          -0.39 -0.59 -0.82 -0.62   (roughly doubles)
#
# A single odds ratio per predictor therefore averages over effects that differ
# in magnitude, and in the case of knows_hunter in direction, across the response
# scale. The ordinal model does not fail loudly; it silently reports an average
# that describes no part of the distribution well.
#
# The knows_hunter pattern is substantively interesting in its own right:
# knowing a hunter moves people out of outright opposition but does nothing to
# push them into strong support.
#
# Three options and why the third was taken:
#   1. Report the proportional-odds model anyway. Rejected — the assumption is
#      demonstrably violated, and the violation involves the strongest
#      predictors, not peripheral ones.
#   2. Partial proportional odds (vglm, generalized ordinal logit). Statistically
#      the most correct route, but no survey-weighted implementation is readily
#      available, and abandoning the design weights to gain the correct link
#      function would be a worse trade.
#   3. Linear model with design-based standard errors. Taken. Both outcomes are
#      near-symmetric with no floor or ceiling pile-up at the aggregate level
#      (hunting M = 3.11 SD = 1.37; trapping M = 2.76 SD = 1.35), interval
#      treatment of 5-point scales is well established in this literature, the
#      survey design already supplies robust sandwich standard errors, and
#      coefficients read directly as scale points.
#
# The cut-point table should be reported as an appendix or supplement so the
# reader can see the assumption was tested rather than assumed away. The ordinal
# results are retained as a robustness check: where the two agree in sign and
# significance, the interval approximation is doing no harm.
#
# The Brant test is not available here — it does not accept survey-weighted
# models — which is why the cut-point comparison is used instead.
#
# DIAGNOSTICS NOT RUN, AND WHY
# - Heteroskedasticity tests (Breusch-Pagan, White). svyglm standard errors are
#   already design-based sandwich estimators, robust to non-constant variance by
#   construction. Testing for it and then applying a robust correction you are
#   already using is redundant.
# - Residual normality (Shapiro-Wilk, Jarque-Bera). Irrelevant at n = 1,499 with
#   robust standard errors, and guaranteed to reject on a bounded 5-point
#   outcome. The document already tests normality of the raw variables; that is
#   ample.
# - Outlier detection on the outcomes. Both are bounded 1-5. There is nowhere
#   for an outlier to be.
# - Durbin-Watson / autocorrelation. Cross-sectional data, no ordering.
#
# MODEL SELECTION — THE DEFENSIBLE POSITION
# No stepwise, no automated search, no dropping predictors because p > .05.
# Stepwise selection inflates Type I error, biases standard errors downward,
# produces confidence intervals with the wrong coverage, and is unstable —
# a different random half of the data yields a different model. Under complex
# survey weighting the problems compound, because the effective sample size is
# smaller than n and the selection uncertainty is not reflected anywhere in the
# reported inference. A reviewer in this field will not object to a full
# theoretically specified model; they will object to a trimmed one.
#
# Three things make selection here defensible:
#
# 1. There is no power problem forcing a choice. 1,499 complete cases against
#    roughly 28 parameters is comfortable. Parsimony is not being purchased with
#    anything; there is no reason to trim.
# 2. The candidate set is small, pre-specified, and theoretically ordered
#    (Models A-D above). AIC and BIC are used to compare these four, not to
#    search a space of subsets. That is a legitimate use of information criteria;
#    all-subsets selection is not.
# 3. Every coefficient in the chosen model is reported, significant or not.
#    Non-significant predictors are findings, and their absence from a table is
#    what makes a reviewer suspicious.
#
# RECOMMENDATION: report Model C (demographics, experience, values, identity) as
# the substantive model and Model D as a supplementary specification. Model C
# contains only constructs that are conceptually prior to the outcome; Model D's
# R2 advantage is partly definitional. Report both — the comparison is
# informative, and choosing D alone would overstate explanatory power while
# choosing C alone would look like the reasons and methods work was discarded.
#
# If a reduced model is genuinely wanted for presentation, reduce on theory
# (drop a construct block wholesale, with a stated rationale), never on p-values,
# and report the full model alongside it.
