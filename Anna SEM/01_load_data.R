# ============================================================
# CLEAN DATA FOR HUNTING-SUPPORT SEM
# ============================================================

library(here)
library(dplyr)
library(fastDummies)

all_data <- readRDS(here("data", "personal_weights.rds"))

# ------------------------------------------------------------
# Category orders (the FIRST level becomes the reference dummy)
# ------------------------------------------------------------

lv_race <- c(
  "White",
  "Asian or Asian-American",
  "Black or African-American",
  "Native American or American Indian",
  "Pacific Islander",
  "Mixed race",
  "Some other race"
)

lv_region <- c("Northeast", "Midwest", "South", "West")

lv_age_group4 <- c("18-29", "30-49", "50-64", "65+")

lv_education <- c(
  "High school graduate or GED",   # reference category
  "Some high school or less",
  "Some college, no degree",
  "2-year or associate degree",
  "4-year or bachelor degree",
  "Post-graduate degree"
)

lv_income <- c(
  "Less than $15,000",
  "$15,000 to under $50,000",
  "$50,000 to under $75,000",
  "$75,000 to under $100,000",
  "$100,000 to under $150,000",
  "$150,000 to under $200,000",
  "More than $200,000"
)


# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

# Character version of x with blanks / whitespace-only values set to NA
# (skipped survey questions are often stored as "" instead of NA)
clean_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  x
}

# Factor with a fixed level order. Stops if a value isn't in the list
# (plain factor() would silently turn it into NA).
set_levels <- function(x, levels) {
  x <- clean_chr(x)
  bad <- setdiff(unique(na.omit(x)), levels)
  if (length(bad) > 0) {
    stop("Values not in level list: ",
         paste0("'", bad, "'", collapse = "; "))
  }
  factor(x, levels = levels)
}

# Two-category variable -> 0/1. Works for Yes/No factors, TRUE/FALSE, or 1/0.
# Stops if it sees any other value.
to_binary <- function(x,
                      yes = c("Yes", "TRUE", "1"),
                      no  = c("No", "FALSE", "0")) {
  x <- clean_chr(x)
  bad <- setdiff(unique(na.omit(x)), c(yes, no))
  if (length(bad) > 0) {
    stop("Unexpected binary values: ",
         paste0("'", bad, "'", collapse = "; "))
  }
  ifelse(is.na(x), NA_real_, as.numeric(x %in% yes))
}

# Ordinal variable already stored as numeric codes 1..n -> integer.
# Stops if the codes are text labels or fall outside 1..n.
to_ordinal <- function(x, n) {
  x <- clean_chr(x)
  out <- suppressWarnings(as.integer(x))
  if (any(is.na(out) & !is.na(x))) {
    stop("Values are text labels, not numbers. ",
         "Use as.integer(set_levels(x, <levels in order>)) instead.")
  }
  if (any(!out %in% c(1:n, NA))) {
    stop("Codes must run 1 to ", n)
  }
  out
}


# ------------------------------------------------------------
# Recode
# ------------------------------------------------------------

hunt_data <- all_data %>%
  select(
    vsid, hunting_support, game_meat_freq,
    wvo_domination, wvo_mutualism, christian_nationalism_score,
    political_ideology, has_pets, eats_meat, hunter_status,
    gender, age_group4, education, income, region, metro, race, hispanic
  ) %>%
  mutate(
    # Binary variables (0/1)
    hispanic  = to_binary(hispanic),
    metro     = to_binary(metro, yes = "Non-metro", no = "Metropolitan"),
    has_pets  = to_binary(has_pets),
    eats_meat = to_binary(eats_meat),
    
    # Hunting exposure: reference = doesn't know a hunter
    knows_hunter = case_when(
      hunter_status == "Yes, I know someone who hunts" ~ 1,
      is.na(hunter_status) ~ NA_real_,
      TRUE ~ 0
    ),
    is_hunter = case_when(
      hunter_status == "Yes, I hunt or have hunted in the past" ~ 1,
      is.na(hunter_status) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # Ordinal outcomes (integer codes starting at 1)
    game_meat_freq  = to_ordinal(game_meat_freq, 4),
    hunting_support = to_ordinal(hunting_support, 5),
    
    # Categorical demographics with explicit reference categories
    gender     = factor(gender),   # reference = first level; check below
    race       = set_levels(race, lv_race),
    region     = set_levels(region, lv_region),
    age_group4 = set_levels(age_group4, lv_age_group4),
    education  = set_levels(education, lv_education),
    income     = set_levels(income, lv_income)
  ) %>%
  select(-hunter_status) %>%
  dummy_cols(
    select_columns = c("gender", "race", "region",
                       "age_group4", "education", "income"),
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE,
    ignore_na = TRUE
  )


# ------------------------------------------------------------
# Checks
# ------------------------------------------------------------

levels(factor(all_data$gender))   # first one listed is the reference

table(hunt_data$knows_hunter, hunt_data$is_hunter, useNA = "ifany")
table(hunt_data$game_meat_freq, useNA = "ifany")
table(hunt_data$hunting_support, useNA = "ifany")

grep("^(gender|race|region|age_group4|education|income)_",
     names(hunt_data), value = TRUE)

saveRDS(hunt_data, here("Anna SEM", "AB_data.rds"))
