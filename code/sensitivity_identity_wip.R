## =============================================================================
## Sensitivity: is ID_consum just hunting support in disguise?
##
## identify_hunters is the strongest-loading item in the consumptive identity
## composite (.87). If the identity effect is really tautological — identifying
## with hunters standing in for supporting hunting — dropping that item should
## collapse it. If the effect survives, the construct is the broader agrarian /
## property identity the other five items describe.
##
## Run the chunks of hunt_regress.Rmd first (through the mediation chunk), so
## blocks, design_cc, model_cc and the helpers are in the session.
## =============================================================================

stopifnot(exists("design_cc"), exists("blocks"), exists("model_cc"),
          exists("build_formula"), exists("tidy_svyglm"), exists("composite"))

# -----------------------------------------------------------------------------
# ALTERNATIVE COMPOSITE
# -----------------------------------------------------------------------------
consum_items    <- c("identify_hunters", "identify_ranchers", "identify_farmers",
                     "identify_landowners", "identify_anglers",
                     "identify_small_business")
consum_items_nh <- setdiff(consum_items, "identify_hunters")

model_cc <- model_cc %>%
  mutate(ID_consum_nh   = composite(., consum_items_nh),
         ID_consum_nh_z = as.numeric(scale(ID_consum_nh)))

design_cc <- model_cc %>% as_survey_design(ids = 1, weights = weight)

# reliability of both versions
alpha_table(model_cc, list(`ID_consum (6 items)`      = consum_items,
                           `ID_consum (no hunters)`   = consum_items_nh)) %>%
  ktab(caption = "Reliability with and without identify_hunters")

# -----------------------------------------------------------------------------
# HOW CLOSE IS EACH VERSION TO THE OUTCOMES?
# -----------------------------------------------------------------------------
# A composite that is really a restatement of the outcome should correlate with
# it far more strongly than its sibling does.
# use = "complete.obs" is required: the identity ITEMS can be missing even when
# the composite is not, because the min-items rule tolerates a few gaps. Without
# it, cor() returns NA and the row silently renders blank.
r_with <- function(x, y) cor(x, y, use = "complete.obs")

tibble(measure = c("ID_consum (6 items)", "ID_consum (no hunters)",
                   "identify_hunters (single item)"),
       r_hunting  = c(r_with(model_cc$ID_consum,        model_cc$hunting_support),
                      r_with(model_cc$ID_consum_nh,     model_cc$hunting_support),
                      r_with(model_cc$identify_hunters, model_cc$hunting_support)),
       r_trapping = c(r_with(model_cc$ID_consum,        model_cc$trapping_support),
                      r_with(model_cc$ID_consum_nh,     model_cc$trapping_support),
                      r_with(model_cc$identify_hunters, model_cc$trapping_support))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  ktab(caption = paste("Correlation with the outcomes. If the single item is",
                       "much closer to support than the reduced composite is,",
                       "the tautology concern is about that item specifically."))

# -----------------------------------------------------------------------------
# REFIT THE FULL MODELS BOTH WAYS
# -----------------------------------------------------------------------------
preds_orig <- unlist(blocks, use.names = FALSE)
preds_nh   <- gsub("^ID_consum_z$", "ID_consum_nh_z", preds_orig)

compare_spec <- function(dv) {
  m_orig <- svyglm(build_formula(dv, preds_orig), design = design_cc)
  m_nh   <- svyglm(build_formula(dv, preds_nh),   design = design_cc)

  bind_rows(
    tidy_svyglm(m_orig) %>% mutate(spec = "6 items"),
    tidy_svyglm(m_nh)   %>% mutate(spec = "no hunters",
                                   term = gsub("ID_consum_nh_z", "ID_consum_z", term))) %>%
    mutate(cell = paste0(sprintf("%.3f", estimate), stars(p),
                         " (", sprintf("%.3f", se), ")")) %>%
    dplyr::select(term, spec, cell) %>%
    pivot_wider(names_from = spec, values_from = cell) %>%
    ktab(caption = paste0("Full model for ", dv,
                          ", consumptive identity with and without ",
                          "identify_hunters. R2 = ",
                          sprintf("%.3f", r2_svyglm(m_orig)), " vs ",
                          sprintf("%.3f", r2_svyglm(m_nh)), "."))
}

compare_spec("hunting_support")
compare_spec("trapping_support")

# -----------------------------------------------------------------------------
# DOES THE MEDIATION SURVIVE?
# -----------------------------------------------------------------------------
# The headline finding is that Christian nationalism reaches support only
# through consumptive identity. That is the claim most exposed to the tautology
# objection, so it is the one worth re-testing.
indirect_nh <- function(dv) {
  f_total  <- build_formula(dv, c(blocks$demographics, blocks$exposure,
                                  antecedent_terms))
  f_direct <- build_formula(dv, c(blocks$demographics, blocks$exposure,
                                  antecedent_terms, "ID_consum_nh_z",
                                  "ID_non_consume_z"))

  boot <- withReplicates(rep_design, function(w, data) {
    data$.repw <- w
    data$ID_consum_nh_z <- as.numeric(scale(composite(data, consum_items_nh)))
    coef(lm(f_total,  data = data, weights = .repw))[antecedent_terms] -
    coef(lm(f_direct, data = data, weights = .repw))[antecedent_terms]
  })

  m_tot <- svyglm(f_total,  design = design_cc)
  m_dir <- svyglm(f_direct, design = design_cc)

  tibble(predictor = antecedent_terms,
         total     = round(coef(m_tot)[antecedent_terms], 3),
         direct    = round(coef(m_dir)[antecedent_terms], 3),
         indirect  = round(as.numeric(coef(boot)), 3),
         se        = round(as.numeric(SE(boot)), 3)) %>%
    mutate(ci_low   = indirect - 1.96 * se,
           ci_high  = indirect + 1.96 * se,
           mediated = ifelse(ci_low > 0 | ci_high < 0, "yes", "no"),
           `95% CI` = paste0("[", round(ci_low, 3), ", ", round(ci_high, 3), "]"),
           `% of total` = ifelse(mediated == "yes",
                                 paste0(round(100 * indirect / total), "%"), "")) %>%
    dplyr::select(predictor, total, direct, indirect, `95% CI`, mediated,
                  `% of total`)
}

set.seed(42)
indirect_nh("hunting_support")  %>%
  ktab(caption = "Indirect effects with identify_hunters removed: hunting support")
indirect_nh("trapping_support") %>%
  ktab(caption = "Indirect effects with identify_hunters removed: trapping support")

# -----------------------------------------------------------------------------
# a-PATHS BOTH WAYS
# -----------------------------------------------------------------------------
map_dfr(c("ID_consum_z", "ID_consum_nh_z"), function(m) {
  fit <- svyglm(build_formula(m, c(blocks$demographics, blocks$exposure,
                                   antecedent_terms)), design = design_cc)
  tidy_svyglm(fit) %>%
    filter(term %in% antecedent_terms) %>%
    mutate(mediator = m, b = paste0(round(estimate, 3), stars(p))) %>%
    dplyr::select(mediator, term, b)
}) %>%
  pivot_wider(names_from = mediator, values_from = b) %>%
  ktab(caption = "a-paths into consumptive identity, with and without identify_hunters")

## =============================================================================
## HOW TO READ THIS
## =============================================================================
#
# The tautology objection is that ID_consum measures hunting support rather than
# a distinct identity. Three things would have to be true for that to hold:
#
#   1. identify_hunters correlates with support far more strongly than the other
#      five items do — check the correlation table.
#   2. Dropping it substantially reduces the identity coefficient in the full
#      models — check the side-by-side comparison.
#   3. The Christian nationalism mediation weakens or disappears — check the
#      indirect effects.
#
# If the reduced composite behaves like the full one, the construct is carried
# by ranchers, farmers, landowners, anglers and small business owners, and the
# hunting item is incidental to it. That is a strong answer to the objection and
# worth reporting as a footnote or robustness paragraph regardless of outcome.
#
# If the effect does collapse, the honest response is to report the reduced
# composite as primary and treat identify_hunters as too close to the outcome to
# include — which would be a finding about measurement, not a failure.
