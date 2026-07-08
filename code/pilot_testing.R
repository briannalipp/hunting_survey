## =============================================================================
## TESTING ANALYSES
## =============================================================================

library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyverse)

#Scatterplot of Attitude vs Hunting Acceptability
species_means <- species_long %>%
  group_by(species) %>%
  summarize(
    mean_attitude = mean(attitude, na.rm = TRUE),
    mean_hunting_approval = mean(hunting_approval, na.rm = TRUE),
    .groups = "drop")

ggplot(species_means, aes(x = mean_attitude, y = mean_hunting_approval, label = species)) +
  geom_point(size = 3, color = "seagreen") +
  geom_text_repel(size = 3.5, max.overlaps = 20) +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    x = "Mean attitude (1 = strongly dislike, 5 = strongly like)",
    y = "Mean hunting approval (1 = strongly disapprove, 5 = strongly approve)",
    title = "Attitudes towards Species vs Acceptability of Hunting"
  ) +
  theme_minimal()





# Model 1: does hunting approval track species characteristics, beyond liking?
# Fixed effects = characteristics; random intercept for participant
model_1 <- lmer(
  hunting_approval ~ attitude + conservation_status + edibility +
    familiarity + desire_to_see + wtp_view + wtp_hunt + danger_humans + 
    destructive_property + (1 | participant_id),
  data = filter(species_long, species_shown)
)
summary(model_1)

# Model 2: what predicts the discrepancy score itself (species-level decoupling of liking from hunting acceptance)?
model_2 <- lmer(
  discrepancy_score ~ conservation_status + edibility +
    familiarity + desire_to_see + wtp_view + wtp_hunt + danger_humans + 
    destructive_property + (1 | participant_id),
  data = filter(species_long, species_shown)
)
summary(model_2)
