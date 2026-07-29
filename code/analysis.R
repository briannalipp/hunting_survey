## =============================================================================
## Wildlife Attitudes Survey
## =============================================================================

library(here)
library(tidyverse)
library(ggrepel)
library(survey)
library(srvyr)

###   Pulling in Data   ###
species_responses <- readRDS(here("data", "species_responses.rds"))
attacc_long       <- readRDS(here("data", "attacc_long.rds"))
personal_weights  <- readRDS(here("data", "personal_weights.rds"))
survey_design     <- readRDS(here("data", "survey_design.rds"))


###   Creating a Survey Design Object for species_responses   ###
species_responses_weights <- species_responses %>%
  inner_join(
    personal_weights %>%
      select(vsid, weight),
    by = "vsid")

species_design <- species_responses_weights %>%
  as_survey_design(
    ids = vsid,
    weights = weight)





###   Weighted Summary Stats of Attitude/Hunting Acceptability   ###
species_summary <- species_design %>%
  group_by(species) %>%
  summarise(
    n_attitude = sum(!is.na(attitude)),
    mean_attitude = survey_mean(
      attitude,
      na.rm = TRUE,
      vartype = "ci"),
    
    n_approval = sum(!is.na(hunting_approval)),
    mean_approval = survey_mean(
      hunting_approval,
      na.rm = TRUE,
      vartype = "ci")) %>%
  mutate(
    mean_discrepancy = mean_approval - mean_attitude) %>%
  arrange(desc(mean_attitude))

print(species_summary, n = Inf)

################################################################################
###   Support for Hunting and Trapping    ###
hunting_support_distribution <- survey_design %>%
  group_by(hunting_support) %>%
  summarise(
    proportion = survey_mean(
      proportion = TRUE,
      na.rm = TRUE,
      vartype = "ci"))
hunting_support_distribution

trapping_support_distribution <- survey_design %>%
  group_by(trapping_support) %>%
  summarise(
    proportion = survey_mean(
      proportion = TRUE,
      na.rm = TRUE,
      vartype = "ci"))
trapping_support_distribution

## Plots
#Support for hunting
huntsupp_plot_data <- survey_design %>%
  group_by(hunting_support) %>%
  summarise(
    proportion = survey_mean(
      proportion = TRUE,
      na.rm = TRUE,
      vartype = "ci")) %>%
  mutate(
    response = factor(
      hunting_support,
      levels = 1:5,
      labels = lv_support))
names(huntsupp_plot_data)

ggplot(huntsupp_plot_data,
  aes(x = response, y = proportion)) +
  geom_col(fill = "firebrick3") +
  geom_errorbar(
    aes(
      ymin = proportion_low,
      ymax = proportion_upp),
    width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Weighted proportion",
    title = "Public Support for Hunting") +
  theme_minimal()

#Support for trapping
trapsupp_plot_data <- survey_design %>%
  group_by(trapping_support) %>%
  summarise(
    proportion = survey_mean(
      proportion = TRUE,
      na.rm = TRUE,
      vartype = "ci")) %>%
  mutate(
    response = factor(
      trapping_support,
      levels = 1:5,
      labels = lv_support))
names(trapsupp_plot_data)

ggplot(trapsupp_plot_data,
  aes(x = response, y = proportion)) +
  geom_col(fill = "dodgerblue3") +
  geom_errorbar(
    aes(
      ymin = proportion_low,
      ymax = proportion_upp),
    width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Weighted proportion",
    title = "Public Support for Trapping") +
  theme_minimal()

#Comparison between support for hunting and trapping
support_comparison <- bind_rows(
  support_plot_data %>%
    mutate(activity = "Hunting"),
  trapping_plot_data %>%
    mutate(activity = "Trapping"))

ggplot(support_comparison,
  aes(
    x = response,
    y = proportion,
    fill = activity)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(
      ymin = proportion_low,
      ymax = proportion_upp),
    width = 0.2,
    position = position_dodge(width = 0.9)) +
  scale_y_continuous(
    labels = scales::percent) +
  scale_fill_manual(values = c(
      "Hunting" = "firebrick3",
      "Trapping" = "steelblue3")) +
  labs(
    x = NULL,
    y = "Weighted proportion (95% CI)",
    fill = NULL,
    title = "Public Support for Hunting and Trapping") +
  theme_minimal()


################################################################################
###   Plots of Attitudes towards and Acceptability of Hunting Species   ###
plot_att_huntacc <- species_summary %>%
  pivot_longer(
    cols = c(mean_attitude, mean_approval),
    names_to = "measure",
    values_to = "mean_value") %>%
  mutate(measure = recode(measure,
                          mean_attitude = "Attitude",
                          mean_approval = "Approval of Hunting"))

plot_1 <- ggplot() +
  geom_segment(
    data = species_summary,
    aes(x = mean_attitude, xend = mean_approval, y = species, yend = species),
    color = "grey70", linewidth = 1) +
  geom_point(
    data = plot_att_huntacc,
    aes(x = mean_value, y = species, color = measure),
    size = 3) +
  scale_color_manual(values = c("Attitude" = "dodgerblue3", "Approval of Hunting" = "firebrick3")) +
  labs(
    x = "Weighted mean rating (1-5 scale)",
    y = NULL,
    color = NULL,
    title = "Attitudes vs Approval of Hunting by Species") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

plot_1

plot_2 <- ggplot(species_summary, aes(x = mean_attitude, y = mean_approval)) +
  geom_abline(slope = -1, intercept = 6, linetype = "dashed", color = "grey60") +
  geom_point(aes(color = mean_discrepancy), size = 3) +
  geom_text_repel(aes(label = species), size = 3.3, max.overlaps = 20) +
  scale_color_gradient2(
    low = "dodgerblue3", mid = "grey80", high = "firebrick3", midpoint = 0) +
  coord_equal(xlim = c(1, 5), ylim = c(1, 5)) +
  labs(
    x = "Attitude (weighted mean)",
    y = "Approval of Hunting (weighted mean)",
    color = "Discrepancy\n(approval - attitude)",
    title = "Attitudes vs Approval of Hunting by Species") +
  theme_minimal(base_size = 12)

plot_2

