
# Load packages
library(here)
library(dplyr)

# Load data
all_data <- readRDS(here("data", "personal_weights.rds"))
design<- readRDS(here("data", "survey_design.rds"))

# Pull out data I need for this analysis
my_data <- select(all_data,
      vsid,
      hunting_support,
      wvo_domination,
      wvo_mutualism,
      christian_nationalism_score,
      has_pets,
      game_meat_freq,
      hunter_status,
      eats_meat,
      political_ideology,
      gender,
      age_group4,
      education,
      income,
      region,
      metro,
      race,
      hispanic               )

# Recode the categorical data to numbers
recoded_cat <- function(x, levels_in_order) {
  as.numeric(factor(x, levels = levels_in_order))}

  lv_hispanic     <- c("No", "Yes")
  lv_race        <-  c("White", "Asian or Asian-American", "Black or African-American", "Mixed race", "Some other race")
  lv_metro       <-  c("Metropolitan", "Non-metro")
  lv_region      <-  c("Northeast", "Midwest", "South", "West")
  lv_income     <-   c("Less than $15,000", "$15,000 to under $50,000", "$50,000 to under $75,000", "$75,000 to under $100,000", "$100,000 to under $150,000", "$150,000 to under $200,000", "More than $200,000")


saveRDS(data, file = "Anna SEM/AB_data.rds")
