## =============================================================================
## Hunting Survey — Model Variable Preparation
## Chunk contents for hunt_regress.Rmd. Replaces the "Model Variables: Final
## Inventory" section.
## Assumes the setup chunk has run (libraries, helpers, personal_weights) and
## that all composites exist.
## =============================================================================

library(srvyr)
library(survey)

# -----------------------------------------------------------------------------
# CATEGORICAL PREDICTORS
# -----------------------------------------------------------------------------
# age enters as a factor rather than as band midpoints. The midpoint approach
# imposed linearity across unevenly spaced bands and invented a value for 65+.
# Income likewise: ordered, but effects on wildlife attitudes are rarely linear
# in dollars, so it enters as a factor.
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
                                   "More than $200,000")))

personal_weights %>% count(age_group)
personal_weights %>% count(income_cat)

# -----------------------------------------------------------------------------
# VARIABLE SETS
# -----------------------------------------------------------------------------
outcome_vars <- c("hunting_support", "trapping_support")

continuous_vars <- c(
  "political_ideology", "wvo_mutualism", "wvo_domination",
  "christian_nationalism_score",
  "ID_consum", "ID_non_consume",
  "reason_utilitarian", "reason_sport", "reason_cultural",
  "method_contested", "method_conventional", "method_dogs_predators")

binary_vars <- c("female", "college_grad", "eats_meat_n",
                 "has_hunted", "knows_hunter", "has_pets_n", "game_meat")

factor_vars <- c("age_group", "income_cat")

model_vars <- c(outcome_vars, continuous_vars, binary_vars, factor_vars)

missing_vars <- setdiff(model_vars, names(personal_weights))
if (length(missing_vars) > 0) {
  cat("!! NOT FOUND in personal_weights:", paste(missing_vars, collapse = ", "), "\n")
} else {
  cat("All", length(model_vars), "model variables present.\n")
}

# -----------------------------------------------------------------------------
# STANDARDIZE CONTINUOUS PREDICTORS
# -----------------------------------------------------------------------------
# Predictors are on incompatible scales (political_ideology 0-100, WVO 1-7,
# composites 1-5). Standardizing makes coefficients comparable. Outcomes are
# left on their original 1-5 scale so effects stay interpretable in scale points.
personal_weights <- personal_weights %>%
  mutate(across(all_of(continuous_vars), ~ as.numeric(scale(.x)), .names = "{.col}_z"))

continuous_z <- paste0(continuous_vars, "_z")

# -----------------------------------------------------------------------------
# SURVEY DESIGN
# -----------------------------------------------------------------------------
# Everything above this point is unweighted — appropriate for measurement
# properties. Population estimates and the regression come from the design.
design <- personal_weights %>%
  as_survey_design(ids = 1, weights = weight)

# -----------------------------------------------------------------------------
# DESCRIPTIVES — UNWEIGHTED AND WEIGHTED SIDE BY SIDE
# -----------------------------------------------------------------------------
unweighted_stats <- personal_weights %>%
  select(any_of(c(outcome_vars, continuous_vars, binary_vars))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(variable) %>%
  dplyr::summarize(n         = n(),
                   mean_unwt = mean(value),
                   sd_unwt   = sd(value),
                   .groups   = "drop")

weighted_stats <- map_dfr(c(outcome_vars, continuous_vars, binary_vars), function(v) {
  design %>%
    dplyr::summarize(mean_wt = survey_mean(.data[[v]], na.rm = TRUE, vartype = NULL),
                     var_wt  = survey_var(.data[[v]], na.rm = TRUE, vartype = NULL)) %>%
    mutate(variable = v, sd_wt = sqrt(var_wt)) %>%
    select(variable, mean_wt, sd_wt)
})

var_role <- tibble(
  variable = c(outcome_vars, continuous_vars, binary_vars),
  role     = c(rep("Outcome", length(outcome_vars)),
               rep("Continuous predictor", length(continuous_vars)),
               rep("Binary predictor", length(binary_vars))))

unweighted_stats %>%
  left_join(weighted_stats, by = "variable") %>%
  left_join(var_role, by = "variable") %>%
  mutate(across(c(mean_unwt, sd_unwt, mean_wt, sd_wt), ~ round(.x, 2)),
         role = factor(role, levels = c("Outcome", "Continuous predictor",
                                        "Binary predictor")),
         diff = round(mean_wt - mean_unwt, 2)) %>%
  arrange(role, variable) %>%
  select(role, variable, n,
         `Mean (unwt)` = mean_unwt, `SD (unwt)` = sd_unwt,
         `Mean (wt)`   = mean_wt,   `SD (wt)`   = sd_wt,
         `Diff`        = diff) %>%
  ktab(caption = paste("Model variables, unweighted and design-weighted.",
                       "Diff is weighted minus unweighted mean — large values",
                       "indicate the characteristic is associated with",
                       "under- or over-representation in the raw sample."))

# categorical predictors, weighted and unweighted proportions
map_dfr(factor_vars, function(v) {
  unwt <- personal_weights %>%
    filter(!is.na(.data[[v]])) %>%
    count(level = .data[[v]]) %>%
    mutate(variable = v, pct_unwt = round(100 * n / sum(n), 1))

  wt <- design %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(level = .data[[v]]) %>%
    dplyr::summarize(pct_wt = survey_prop(vartype = NULL)) %>%
    mutate(pct_wt = round(100 * pct_wt, 1))

  left_join(unwt, wt, by = "level") %>%
    select(variable, level, n, pct_unwt, pct_wt)
}) %>%
  ktab(caption = "Categorical predictors: unweighted and design-weighted percentages")

# -----------------------------------------------------------------------------
# EFFECTIVE SAMPLE SIZE
# -----------------------------------------------------------------------------
complete_n <- personal_weights %>%
  select(any_of(c(outcome_vars, continuous_z, binary_vars, factor_vars))) %>%
  drop_na() %>%
  nrow()

tibble(`Total respondents`               = nrow(personal_weights),
       `Complete on all model variables` = complete_n,
       `Lost to listwise deletion`       = nrow(personal_weights) - complete_n,
       `Percent retained`                = round(100 * complete_n / nrow(personal_weights), 1)) %>%
  ktab(caption = "Effective sample size under listwise deletion")

## =============================================================================
## NOTES
## =============================================================================
#
# SAMPLE
# National VeraSight panel, n = 1,513 after dropping cases without weights
# (1,619 raw responses). Region: South 573, West 336, Midwest 313,
# Northeast 275. This is NOT a Montana sample — interpretation should be
# national.
#
# WEIGHTING
# All measurement work above (EFA, reliability) is unweighted: factor structure
# is a property of the instrument, not the population. Descriptives are reported
# both ways. The regression must be design-based — svyglm() or svyolr() on
# `design`, never MASS::polr() on the raw frame, which would discard the weights
# silently.
#
# AGE
# Enters as a 4-level factor (18-29, 30-49, 50-64, 65+) rather than as band
# midpoints. The midpoint coding imposed linearity across unevenly spaced bands
# and assigned an invented value of 67 to the open-ended 65+ category.
#
# INCOME
# Seven ordered categories, entered as a factor. Income effects on wildlife
# attitudes are rarely linear in dollars. Six cases missing.
#
# STANDARDIZATION
# All continuous predictors are z-scored; outcomes are left on the original 1-5
# scale so coefficients read as scale points per SD of predictor. Note the
# z-scores are computed unweighted — if the weighted and unweighted SDs diverge
# materially in the table above, consider standardizing within the design
# instead.
#
# METRO — NOT USED
# The `metro` variable is a binary Metropolitan/Non-metro split (1,314 / 183),
# too coarse to capture the rural-urban gradient that matters here.
#
# ON USING LAT/LONG INSTEAD
# The raw Qualtrics export carries LocationLatitude and LocationLongitude for
# essentially every respondent. Two cautions before building anything on them:
#
#   1. These are IP-based geolocations, not reported addresses. Accuracy is
#      typically city-level at best and can be off by tens of miles — routinely
#      resolving to an ISP hub rather than the respondent. That error is the
#      same order of magnitude as the rural-urban distinction being measured.
#      For panel respondents answering on mobile or through a VPN it is worse.
#   2. Precise coordinates are indirect identifiers. Using them moves this
#      dataset closer to identifiable human-subjects data — worth confirming
#      the IRB protocol covers geospatial processing before starting.
#
# A better route if a rural-urban measure is needed: ask VeraSight whether the
# panel holds ZIP or county for these respondents. County FIPS supports proper
# RUCA or USDA rural-urban continuum codes, which is what the literature uses.
