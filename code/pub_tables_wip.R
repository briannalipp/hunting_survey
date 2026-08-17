## =============================================================================
## Hunting Survey — Publication Tables and Figures
## Chunk contents for hunt_regress.Rmd. Run after regression_wip.R.
## Requires: design_cc, model_cc, blocks, all_predictors, hunt_lin, trap_lin,
##           ktab(), fam, okabe_ito
## =============================================================================

# -----------------------------------------------------------------------------
# TERM LABELS AND BLOCK ASSIGNMENT
# -----------------------------------------------------------------------------
term_labels <- c(
  "age_group30-49"                = "Age 30-49",
  "age_group50-64"                = "Age 50-64",
  "age_group65+"                  = "Age 65+",
  "income_cat15-50k"              = "Income 15-50k",
  "income_cat50-75k"              = "Income 50-75k",
  "income_cat75-100k"             = "Income 75-100k",
  "income_cat100-150k"            = "Income 100-150k",
  "income_cat150-200k"            = "Income 150-200k",
  "income_cat200k+"               = "Income 200k+",
  "female"                        = "Female",
  "college_grad"                  = "College graduate",
  "has_pets_n"                    = "Has pets",
  "eats_meat_n"                   = "Eats meat",
  "knows_hunter"                  = "Knows a hunter",
  "game_meat"                     = "Eats game meat",
  "wvo_mutualism_z"               = "Mutualism",
  "wvo_domination_z"              = "Domination",
  "political_ideology_z"          = "Political ideology (liberal)",
  "christian_nationalism_score_z" = "Christian nationalism",
  "ID_consum_z"                   = "Consumptive identity",
  "ID_non_consume_z"              = "Non-consumptive identity")

block_labels <- c(demographics = "Demographics",
                  exposure     = "Exposure",
                  values       = "Values and ideology",
                  identity     = "Identity")

# match a fitted term back to its block by longest matching prefix
assign_block <- function(terms) {
  map_chr(terms, function(tm) {
    hit <- map_lgl(blocks, ~ any(str_starts(tm, fixed(.x))))
    if (any(hit)) names(blocks)[which(hit)[1]] else NA_character_
  })
}

# -----------------------------------------------------------------------------
# TIDY BOTH FULL MODELS
# -----------------------------------------------------------------------------
tidy_full <- function(m, outcome) {
  s <- summary(m)$coefficients
  tibble(term     = rownames(s),
         estimate = s[, 1],
         se       = s[, 2],
         p        = s[, 4]) %>%
    filter(term != "(Intercept)") %>%
    mutate(outcome  = outcome,
           conf_low  = estimate - qnorm(0.975) * se,
           conf_high = estimate + qnorm(0.975) * se,
           block     = assign_block(term),
           label     = coalesce(term_labels[term], term))
}

coefs <- bind_rows(
  tidy_full(hunt_lin, "Hunting support"),
  tidy_full(trap_lin, "Trapping support")) %>%
  mutate(block   = factor(block, levels = names(block_labels),
                          labels = block_labels),
         outcome = factor(outcome, levels = c("Hunting support",
                                              "Trapping support")))

# -----------------------------------------------------------------------------
# FIGURE: COEFFICIENT PLOT
# -----------------------------------------------------------------------------
# Submission chrome — no title, caption carries the detail. Blocks are encoded by
# colour AND shape so the figure survives greyscale printing.
plot_order <- coefs %>%
  filter(outcome == "Hunting support") %>%
  arrange(block, estimate) %>%
  pull(label)

coef_plot <- coefs %>%
  mutate(label = factor(label, levels = plot_order)) %>%
  ggplot(aes(x = estimate, y = label, color = block, shape = block)) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high),
                 height = 0, linewidth = 0.5) +
  geom_point(size = 2) +
  facet_wrap(~ outcome) +
  scale_color_manual(values = okabe_ito[1:4]) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  scale_y_discrete(limits = rev) +
  labs(x = "Change in support (scale points)", y = NULL,
       caption = paste0(
         "Design-weighted linear regression, n = ", nrow(model_cc),
         ". Points are coefficients with 95% confidence intervals. Continuous ",
         "predictors are standardised, so estimates are scale points per SD. ",
         "Reference categories: age 18-29, income under 15k.")) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        legend.title    = element_blank(),
        legend.position = "top",
        plot.caption    = element_text(color = "grey45", hjust = 0, size = rel(0.75)),
        axis.title      = element_text(color = "grey20"),
        strip.text      = element_text(face = "bold", hjust = 0))

coef_plot

ggsave(here("figs", "coefficient_plot.png"),
       coef_plot, width = 8, height = 6, dpi = 300, device = agg_png)

# -----------------------------------------------------------------------------
# TABLE 1: SAMPLE CHARACTERISTICS
# -----------------------------------------------------------------------------
# Unweighted and weighted, so a reader can see what the weights are correcting.
cat_summary <- map_dfr(c("age_group", "income_cat"), function(v) {
  unwt <- model_cc %>%
    count(level = .data[[v]]) %>%
    mutate(variable = v, pct_unwt = 100 * n / sum(n))
  wt <- design_cc %>%
    group_by(level = !!sym(v)) %>%
    dplyr::summarize(pct_wt = survey_prop(vartype = NULL)) %>%
    mutate(pct_wt = 100 * pct_wt)
  left_join(unwt, wt, by = "level")
})

bin_summary <- map_dfr(c("female", "college_grad", "has_pets_n", "eats_meat_n",
                         "knows_hunter", "game_meat"), function(v) {
  tibble(variable = v,
         level    = "Yes",
         n        = sum(model_cc[[v]] == 1, na.rm = TRUE),
         pct_unwt = 100 * mean(model_cc[[v]], na.rm = TRUE),
         pct_wt   = 100 * as.numeric(coef(svymean(make.formula(v), design_cc,
                                                  na.rm = TRUE))))
})

bind_rows(cat_summary, bin_summary) %>%
  mutate(variable = coalesce(term_labels[variable], variable),
         across(c(pct_unwt, pct_wt), ~ round(.x, 1))) %>%
  dplyr::select(Variable = variable, Level = level, n,
                `% unweighted` = pct_unwt, `% weighted` = pct_wt) %>%
  ktab(caption = paste0("Table 1. Sample characteristics (n = ", nrow(model_cc),
                        "). Percentages are unweighted and design-weighted."))

# -----------------------------------------------------------------------------
# TABLE 2: CONTINUOUS VARIABLE DESCRIPTIVES AND CORRELATIONS
# -----------------------------------------------------------------------------
# Reported on the original response scales, not standardised, so the means and
# SDs are interpretable.
cont_raw <- c("hunting_support", "trapping_support",
              "wvo_mutualism", "wvo_domination", "political_ideology",
              "christian_nationalism_score", "ID_consum", "ID_non_consume")

cont_labels <- c("Hunting support", "Trapping support", "Mutualism", "Domination",
                 "Political ideology", "Christian nationalism",
                 "Consumptive identity", "Non-consumptive identity")

cor_mat <- cor(model_cc[, cont_raw], use = "pairwise.complete.obs")
cor_mat[upper.tri(cor_mat, diag = TRUE)] <- NA

tibble(Variable = paste0(seq_along(cont_raw), ". ", cont_labels),
       Mean     = round(colMeans(model_cc[, cont_raw], na.rm = TRUE), 2),
       SD       = round(apply(model_cc[, cont_raw], 2, sd, na.rm = TRUE), 2)) %>%
  bind_cols(as.data.frame(round(cor_mat, 2)) %>%
              setNames(seq_along(cont_raw))) %>%
  ktab(caption = paste("Table 2. Descriptive statistics and correlations among",
                       "continuous variables. Values are on original response",
                       "scales; correlations are unweighted."))

# -----------------------------------------------------------------------------
# TABLE 3: HIERARCHICAL REGRESSION SUMMARY
# -----------------------------------------------------------------------------
bind_rows(
  tibble(Outcome = "Hunting support",
         Block   = block_labels[names(hunt_models)],
         k       = map_int(hunt_models, ~ length(coef(.x)) - 1L),
         R2      = map_dbl(hunt_models, r2_svyglm)),
  tibble(Outcome = "Trapping support",
         Block   = block_labels[names(trap_models)],
         k       = map_int(trap_models, ~ length(coef(.x)) - 1L),
         R2      = map_dbl(trap_models, r2_svyglm))) %>%
  group_by(Outcome) %>%
  mutate(`Delta R2` = round(R2 - lag(R2, default = 0), 3),
         R2         = round(R2, 3)) %>%
  ungroup() %>%
  ktab(caption = paste("Table 3. Hierarchical regression. Blocks enter in assumed",
                       "causal order; Delta R2 is the increment from adding each."))

# -----------------------------------------------------------------------------
# TABLE 4: FULL MODEL, GROUPED BY BLOCK
# -----------------------------------------------------------------------------
# kableExtra::pack_rows groups rows under block headings, so the table reads as
# theoretically organised rather than as an undifferentiated list.
coef_wide <- coefs %>%
  mutate(cell = paste0(sprintf("%.3f", estimate), stars(p),
                       " (", sprintf("%.3f", se), ")")) %>%
  dplyr::select(block, label, outcome, cell) %>%
  pivot_wider(names_from = outcome, values_from = cell) %>%
  arrange(block)

block_index <- coef_wide %>% count(block) %>% deframe()

coef_wide %>%
  dplyr::select(Predictor = label, `Hunting support`, `Trapping support`) %>%
  kable(caption = paste0(
    "Table 4. Design-weighted linear regression predicting support for hunting ",
    "and trapping (n = ", nrow(model_cc), "). Cells are unstandardised ",
    "coefficients with standard errors in parentheses. Continuous predictors ",
    "are standardised. Reference categories: age 18-29, income under 15k. ",
    "* p < .05, ** p < .01, *** p < .001.")) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE, position = "left") %>%
  pack_rows(index = block_index)

# -----------------------------------------------------------------------------
# TABLE 5: INDIRECT EFFECTS
# -----------------------------------------------------------------------------
# Produced by indirect_effects() in regression_wip.R — reported here as a
# publication table with both outcomes together.
bind_rows(
  indirect_tbl("hunting_support")  %>% mutate(Outcome = "Hunting support"),
  indirect_tbl("trapping_support") %>% mutate(Outcome = "Trapping support")) %>%
  ktab(caption = paste("Table 5. Indirect effects of values on support through",
                       "identity. Bootstrap confidence intervals from 500 survey",
                       "replicate weights."))

## =============================================================================
## NOTES
## =============================================================================
#
# WHAT IS REPORTED AND WHY
# Table 1  sample characteristics, unweighted and weighted — lets a reader see
#          what the weights correct, and is expected in survey work
# Table 2  descriptives and correlations on original scales — standard, and
#          allows re-analysis
# Table 3  hierarchical R2 by block — the theoretical argument in one table
# Table 4  full model grouped by block — every coefficient, significant or not
# Table 5  indirect effects — the mediation result
# Figure 1 coefficient plot — the same information as Table 4, readable at a
#          glance
#
# Table 4 and Figure 1 duplicate each other by design. Most journals will take
# one or the other; the plot communicates better, the table is more precise.
# Offer the table as a supplement if the plot goes in the main text.
#
# NO PREDICTORS ARE DROPPED FOR NON-SIGNIFICANCE. Christian nationalism is
# non-significant as a direct effect but carries a significant indirect effect
# through identity (34% of its total effect on hunting support, 70% on
# trapping). Non-consumptive identity is null throughout, which is itself a
# result worth reporting given the scale was constructed and validated.
# Demographic controls are retained regardless of significance.
#
# REFERENCE CATEGORIES
# age 18-29 and income under 15k. Both are the lowest level, which keeps the
# coefficient signs intuitive. State them in every table caption — readers
# cannot infer them.
