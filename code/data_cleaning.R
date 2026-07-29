## =============================================================================
## Wildlife Attitudes Survey
## =============================================================================

library(here)
library(tidyverse)

# -----------------------------------------------------------------------------
# PULL IN DATA
# -----------------------------------------------------------------------------

raw_data <- read.csv(here("data", "data.csv"), header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

question_text <- raw_data[1, ]

#Remove unwanted rows and columns, reset row #s
data <- raw_data[-c(1, 2), -c(1:17, 270:288)]
rownames(data) <- NULL

#Remove pilot data
data <- data %>%
  filter(!is.na(vsid) & vsid != "")

#Clean out duplicate IDs
data <- data %>%
  filter(!vsid %in% c(
    "06ae6ed6-3521-4d09-8f4c-08bd677458d8",
    "de0e17b1-5a8e-44e7-b55b-35d28c43d2b7",
    "981a6285-cc05-473e-a17b-79452e16d44e",
    "8fd1e531-e397-4ab9-96ee-4b40c4795965",
    "6fc52029-d8a3-4389-b8da-3f60b2a9d5dc",
    "c856b79a-7165-4359-9e59-9bd60f2b307c",
    "a3d82e53-05b1-4028-93af-9472d27440d3",
    "53d87812-edea-434e-a16b-074fde11af3c"))

# -----------------------------------------------------------------------------
# CREATE SPECIES KEY
# -----------------------------------------------------------------------------
species_key <- c(
  "African savanna elephant", "Tundra swan", "Gray wolf", "Coyote", "Raccoon", "American crow", "Striped skunk", "Rattlesnake", "Cougar", "Bald eagle", "Bison", "White-tailed deer", "Black bear", "Elk", "Brown rat", "Mallard duck", "American robin", "White shark")


# -----------------------------------------------------------------------------
# CREATE LONG DATASET OF SPECIES RESPONSES
# -----------------------------------------------------------------------------
#Pivot Q1-Q10 from wide to long
species_responses <- data %>%
  select(vsid, matches("^Q(1|2|3|4|5|6|7|8|9|10)_[0-9]+$")) %>%
  pivot_longer(
    cols = -vsid,
    names_to = c("question", "species_index"),
    names_pattern = "^Q([0-9]+)_([0-9]+)$",
    values_to = "response") %>%
  mutate(
    species_index = as.integer(species_index),
    species = species_key[species_index],
    question = paste0("Q", question)) %>%
  pivot_wider(
    id_cols = c(vsid, species),
    names_from = question,
    values_from = response)


# -----------------------------------------------------------------------------
# CHANGE LIKERTS TO NUMERICS
# -----------------------------------------------------------------------------
likert_to_num <- function(x, levels_in_order) {
  as.numeric(factor(x, levels = levels_in_order))}

lv_attitude     <- c("Strongly dislike", "Somewhat dislike", "Neither like nor dislike", "Somewhat like", "Strongly like")
lv_approve      <- c("Strongly disapprove", "Somewhat disapprove", "Neither approve nor disapprove", "Somewhat approve", "Strongly approve")
lv_edibility    <- c("Very disgusting", "Somewhat disgusting", "Neutral", "Somewhat appealing", "Very appealing")
lv_familiarity    <- c("Never", "Once or twice", "About three to five times", "About six to ten times", "More than ten times")
lv_desire_see   <- c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")
lv_wtp          <- c("$0", "Up to $100", "$101-$300", "$301-$500", "$501-$1000", "Over $1000")
lv_danger       <- c("Not at all dangerous", "A little dangerous", "Moderately dangerous", "A lot dangerous", "Extremely dangerous")
lv_destructive  <- c("Not at all destructive", "A little destructive", "Moderately destructive", "A lot destructive", "Extremely destructive")
lv_support      <- c("Strongly oppose", "Somewhat oppose", "Neutral", "Somewhat support", "Strongly support")
lv_acceptable5  <- c("Very unacceptable", "Unacceptable", "Neither", "Acceptable", "Very acceptable")
lv_agree7       <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")
lv_agree5       <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
lv_identify     <- c("Not at all", "Slightly", "Moderately", "Very much", "Extremely")
lv_game_freq    <- c("Never", "Once or twice per year", "At least once per month", "At least once per week")
lv_conservation <- c("Critically endangered", "Threatened", "Of concern/vulnerable", "Stable/least concern", "Overabundant")

species_responses <- species_responses %>%
  mutate(
    attitude             = likert_to_num(Q1, lv_attitude),
    hunting_approval     = likert_to_num(Q2, lv_approve),
    conservation_status  = likert_to_num(Q3, lv_conservation),
    edibility            = likert_to_num(Q4, lv_edibility),
    familiarity          = likert_to_num(Q5, lv_familiarity),
    desire_to_see        = likert_to_num(Q6, lv_desire_see),
    wtp_hunt             = likert_to_num(Q7, lv_wtp),
    wtp_view             = likert_to_num(Q8, lv_wtp),
    danger_humans        = likert_to_num(Q9, lv_danger),
    destructive_property = likert_to_num(Q10, lv_destructive),
    discrepancy_score    = hunting_approval - attitude) %>%
  select(
    vsid,
    species,
    attitude,
    hunting_approval,
    conservation_status,
    edibility,
    familiarity,
    desire_to_see,
    wtp_hunt,
    wtp_view,
    danger_humans,
    destructive_property)

#############################
###   Access attitude/hunting approval/species characteristics (i.e. Q1-Q10) via species_responses.   ###
#############################

# -----------------------------------------------------------------------------
# ATTITUDE-ACCEPTABILITY FRAMEWORK
# -----------------------------------------------------------------------------
attacc_map <- tribble(
  ~attacc_species,       ~att,        ~acc,
  "Bald eagle",          "Q16",       "Q17",
  "White-tailed deer",   "Q18",       "Q19",
  "Coyote",              "Q20",       "Q21",
  "White shark",         "Q22",       "Q23")

build_attacc_rows <- function(attacc_species, att, acc) {
  data %>%
    transmute(
      vsid,
      species = attacc_species,
        #semantic differential terms
      harmful_beneficial = as.numeric(.data[[paste0(att, "_1")]]),
      unpleasant_pleasant = as.numeric(.data[[paste0(att, "_2")]]),
      bad_good = as.numeric(.data[[paste0(att, "_3")]]),
        #acceptability terms
      overpopulated = likert_to_num(.data[[paste0(acc, "_1")]], lv_agree5),
      threat_interests = likert_to_num(.data[[paste0(acc, "_2")]], lv_agree5),
      threat_lives = likert_to_num(.data[[paste0(acc, "_3")]], lv_agree5))
}

attacc_long <- pmap_dfr(
  attacc_map,
  build_attacc_rows) %>%
  mutate(
    attacc_attitude = rowMeans(
      cbind(harmful_beneficial,
            unpleasant_pleasant,
            bad_good),
      na.rm = TRUE),
    attacc_acceptability = rowMeans(
      cbind(overpopulated,
            threat_interests,
            threat_lives),
      na.rm = TRUE)) %>%
  arrange(vsid, species)

#############################
###   Access attitude/acceptability framework (Q16-Q23) via attacc_long.   ###
#############################


# -----------------------------------------------------------------------------
# PARTICIPANT-LEVEL DATA
# -----------------------------------------------------------------------------
recode_block <- function(data, id_col, labels, level_order) {
  old_names <- names(labels)
  data %>%
    transmute(vsid = {{ id_col }}, across(all_of(old_names), ~ likert_to_num(.x, level_order))) %>%
    rename_with(~ unname(labels[.x]), .cols = all_of(old_names))
}

###   Hunting Context Questions (Q11-Q14)   ###

#Single-item
personal <- data %>%
  transmute(vsid,
            hunting_support = likert_to_num(Q11_1, lv_support),
            trapping_support = likert_to_num(Q12_1, lv_support)      )

#Multi-item
reason_labels <- c(
  Q13_1 = "reason_food", Q13_2 = "reason_recreation", Q13_3 = "reason_population_control",
  Q13_4 = "reason_trophy", Q13_5 = "reason_reduce_conflict", Q13_6 = "reason_conservation_revenue",
  Q13_7 = "reason_indigenous_practice", Q13_8 = "reason_cultural_tradition")
reason_df <- recode_block(data, vsid, reason_labels, lv_acceptable5)

method_labels <- c(
  Q14_1 = "method_baiting", Q14_2 = "method_captive_hunt", Q14_3 = "method_trapping",
  Q14_4 = "method_neck_snares", Q14_5 = "method_dogs_predators", Q14_6 = "method_dogs_birds",
  Q14_7 = "method_aerial", Q14_8 = "method_bow", Q14_9 = "method_firearms")
method_df <- recode_block(data, vsid, method_labels, lv_acceptable5)

personal <- personal %>%
  left_join(reason_df, by = "vsid") %>%
  left_join(method_df, by = "vsid")

###   WVOs    ###
wvo_labels <- c(
  Q15_1 = "wvo_animal_rights",        # mutualism
  Q15_2 = "wvo_one_family",           # mutualism
  Q15_3 = "wvo_side_by_side",         # mutualism
  Q15_4 = "wvo_human_needs_priority", # domination
  Q15_5 = "wvo_wildlife_for_use",     # domination
  Q15_6 = "wvo_kill_if_threat")       # domination
wvo_df <- recode_block(data, vsid, wvo_labels, lv_agree7)
wvo_df <- wvo_df %>%
  mutate(wvo_mutualism = rowMeans(
      cbind(
        wvo_animal_rights,
        wvo_one_family,
        wvo_side_by_side), na.rm = TRUE),
    wvo_domination = rowMeans(
      cbind(
        wvo_human_needs_priority,
        wvo_wildlife_for_use,
        wvo_kill_if_threat), na.rm = TRUE))

###   Identity    ###
identity_labels <- c(
  Q24_1 = "identify_hunters", Q24_2 = "identify_ranchers", Q24_3 = "identify_environmentalists",
  Q24_4 = "identify_farmers", Q24_5 = "identify_small_business", Q24_6 = "identify_landowners",
  Q24_7 = "identify_outdoor_rec", Q24_8 = "identify_outdoor_viewers", Q24_9 = "identify_anglers")
identity_df <- recode_block(data, vsid, identity_labels, lv_identify)

###   Christian Nationalism   ###
cns_labels <- setNames(paste0("cns_item", 1:6), paste0("Q32_", 1:6))
cns_df <- recode_block(data, vsid, cns_labels, lv_agree5)
cns_df <- cns_df %>%
  mutate(christian_nationalism_score = rowMeans(
      cbind(
        cns_item1,
        cns_item2,
        cns_item3,
        cns_item4,
        cns_item5,
        cns_item6), na.rm = TRUE))

personal <- personal %>%
  left_join(wvo_df, by = "vsid") %>%
  left_join(identity_df, by = "vsid") %>%
  left_join(cns_df, by = "vsid")

### Demographics ###
demo_survey <- data %>%
  transmute(
    vsid,
    has_pets              = Q25 == "Yes",
    game_meat_freq        = likert_to_num(Q26, lv_game_freq),
    hunter_status         = factor(
      Q27,
      levels = c(
        "No",
        "Yes, I know someone who hunts",
        "Yes, I hunt or have hunted in the past")),
    interested_in_hunting = likert_to_num(Q28_1, lv_agree5),
    hunted_species_raw    = Q29,
    eats_meat             = Q30 == "Yes, I eat meat",
    political_ideology    = as.numeric(Q31_1)) %>%        #0 (very conservative) to 100 (very liberal)
  mutate(
    hunted_ungulates       = str_detect(hunted_species_raw, "Ungulates"),
    hunted_birds_smallgame = str_detect(hunted_species_raw, "Birds or small game"),
    hunted_predators       = str_detect(hunted_species_raw, "Predators"))

personal <- personal %>%
  left_join(demo_survey, by  = "vsid")

#Join in the demographics VeraSight gave us
VS_demo <- read.csv(here("data", "demographics.csv"),
                    stringsAsFactors = FALSE)

personal <- personal %>%
  left_join(VS_demo, by = "vsid")


#############################
###   Access demographics and hunting context questions via "personal".   ###
#############################


# -----------------------------------------------------------------------------
# CREATING SURVEY OBJECT
# -----------------------------------------------------------------------------
personal_weights <- personal %>%
  filter(!is.na(weight))

survey_design <- personal_weights %>%
  as_survey_design(
    ids = 1,
    weights = weight)


# -----------------------------------------------------------------------------
# SAVE CLEANED DATA
# -----------------------------------------------------------------------------
saveRDS(species_responses, here("data", "species_responses.rds"))
saveRDS(attacc_long,       here("data", "attacc_long.rds"))
saveRDS(personal_weights,          here("data", "personal.rds"))
saveRDS(survey_design,     here("data", "survey_design.rds"))

