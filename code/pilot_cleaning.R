## =============================================================================
## Wildlife Attitudes Survey — Pilot Data Transformation
## =============================================================================
## This code reshapes the Qualtrics export from wide to long format and produce
## a data dictionary alongside the cleaned data.
## =============================================================================

library(tidyr)
library(here)
library(dplyr)
library(tidyverse)

# -----------------------------------------------------------------------------

raw_data_full <- read.csv(here("data", "pilot_data.csv"), header = TRUE,
                          stringsAsFactors = FALSE, check.names = FALSE)

question_text <- raw_data_full[1, ]
raw_data <- raw_data_full[-c(1, 2), ]

# -----------------------------------------------------------------------------
# SPECIES NAME ALIGNMENT - making sure all species calls are the same
# -----------------------------------------------------------------------------

survey_sp <- c(
  "African savanna elephant", "Tundra swan", "Gray wolf", "Coyote", "Raccoon",
  "American crow", "Striped skunk", "Rattlesnake", "Cougar", "Bald eagle",
  "Bison", "White-tailed deer", "Black bear", "Elk", "Brown rat",
  "Mallard duck", "American robin")      #, "White shark")

randomizer_sp <- c(
  "African savanna elephant", "Tundra swan", "Gray wolf", "Coyote", "Raccoon",
  "Crow", "Striped skunk", "Rattlesnake", "Cougar", "Bald eagle", "Bison",
  "White-tailed deer", "Black bear", "Elk", "Brown rat", "Mallard duck",
  "American robin")      #"White shark")

species <- setNames(survey_sp, randomizer_sp)

attacc_species <- c("Bald eagle", "White-tailed deer", "Coyote", "White shark")

# -----------------------------------------------------------------------------
# LIKERT TO NUMERIC CODE - switch likert values to #s
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
lv_support <- c("Strongly oppose", "Somewhat oppose", "Neutral", "Somewhat support", "Strongly support")
lv_acceptable5  <- c("Very unacceptable", "Unacceptable", "Neither", "Acceptable", "Very acceptable")
lv_agree7       <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")
lv_agree5       <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
lv_identify     <- c("Not at all", "Slightly", "Moderately", "Very much", "Extremely")
lv_game_freq    <- c("Never", "Once or twice per year", "At least once per month", "At least once per week")
lv_conservation <- c("Critically endangered", "Threatened", "Of concern/vulnerable", "Stable/least concern", "Overabundant")

# -----------------------------------------------------------------------------
# SPECIES-LEVEL LONG FORMAT (all 17 species x each participant)
# -----------------------------------------------------------------------------
# Q1 (species liking) and Q2 (hunting approval) were answered for all 17
# species. Q3-Q10 (characteristics) were only collected for the 5 species
# randomly assigned to that respondent -- those columns are blank (NA) for
# the other 12 species-rows. Make a species_shown flag so its easy to filter.

species_core <- raw_data %>%
  transmute(participant_id = ResponseId, across(matches("^Q(1|2|3|4|5|6|7|8|9|10)_[0-9]+$"))) %>%
  pivot_longer(
    cols = -participant_id,
    names_to = c("item", "sp_index"),
    names_pattern = "^Q([0-9]+)_([0-9]+)$",
    values_to = "value"
  ) %>%
  mutate(
    sp_index = as.integer(sp_index),
    species  = survey_sp[sp_index],
    item     = paste0("Q", item)
  ) %>%
  pivot_wider(id_cols = c(participant_id, species), names_from = item, values_from = value)

# Species shown to each participant (long form), joined by participant_id
species_shown_flags <- raw_data %>%
  transmute(participant_id = ResponseId, across(all_of(randomizer_sp))) %>%
  pivot_longer(-participant_id, names_to = "species_raw", values_to = "shown_flag") %>%
  mutate(
    species = species[species_raw],
    species_shown = shown_flag == "1"
  ) %>%
  select(participant_id, species, species_shown)

species_long <- species_core %>%
  left_join(species_shown_flags, by = c("participant_id", "species")) %>%
  transmute(
    participant_id,
    species,
    species_shown,
    attitude              = likert_to_num(Q1, lv_attitude),
    hunting_approval      = likert_to_num(Q2, lv_approve),
    conservation_status   = factor(if_else(Q3 == "I don't know", NA_character_, Q3),
                                   levels = lv_conservation, ordered = TRUE),
    conservation_dontknow = Q3 == "I don't know",
    edibility             = likert_to_num(Q4, lv_edibility),
    familiarity           = likert_to_num(Q5, lv_familiarity),
    desire_to_see         = likert_to_num(Q6, lv_desire_see),
    wtp_hunt              = likert_to_num(Q7, lv_wtp),
    wtp_view              = likert_to_num(Q8, lv_wtp),
    danger_humans         = likert_to_num(Q9, lv_danger),
    destructive_property  = likert_to_num(Q10, lv_destructive),
    discrepancy_score     = hunting_approval - attitude
  ) %>%
  arrange(participant_id, species)

# -----------------------------------------------------------------------------
# ATTITUDE-ACCEPTABILITY FRAMEWORK (4 focal species x each participant)
# -----------------------------------------------------------------------------
attacc_map <- tribble(
  ~attacc_species,       ~sem_prefix, ~control_prefix,
  "Bald eagle",          "Q15",       "Q16",
  "White-tailed deer",   "Q17",       "Q18",
  "Coyote",              "Q19",       "Q20",
  "White shark",         "Q21",       "Q22"
)

build_attacc_rows <- function(sem_prefix, control_prefix, attacc_species_name) {
  raw_data %>%
    transmute(
      participant_id           = ResponseId,
      attacc_species            = attacc_species_name,
      sem_harmful_beneficial   = as.numeric(.data[[paste0(sem_prefix, "_1")]]),
      sem_unpleasant_pleasant  = as.numeric(.data[[paste0(sem_prefix, "_2")]]),
      sem_bad_good             = as.numeric(.data[[paste0(sem_prefix, "_3")]]),
      control_overpopulated    = likert_to_num(.data[[paste0(control_prefix, "_1")]], lv_agree5),
      control_threat_interests = likert_to_num(.data[[paste0(control_prefix, "_2")]], lv_agree5),
      control_threat_lives     = likert_to_num(.data[[paste0(control_prefix, "_3")]], lv_agree5)
    )
}

attacc_long <- pmap_dfr(
  list(attacc_map$sem_prefix, attacc_map$control_prefix, attacc_map$attacc_species),
  build_attacc_rows
) %>%
  mutate(
    attacc_attitude    = rowMeans(cbind(sem_harmful_beneficial, sem_unpleasant_pleasant, sem_bad_good), na.rm = TRUE),
    attacc_accept    = rowMeans(cbind(control_overpopulated, control_threat_interests, control_threat_lives), na.rm = TRUE),
    # key attitude-acceptability discrepancy: higher = more accepting of
    # lethal control than the attitude alone would predict
    discrepancy_score = attacc_accept - attacc_attitude
  ) %>%
  arrange(participant_id, attacc_species)

# -----------------------------------------------------------------------------
# PARTICIPANT-LEVEL DATA
# -----------------------------------------------------------------------------
# Recode block of columns and rename to useful labels
recode_block <- function(data, id_col, labels, level_order) {
  old_names <- names(labels)
  data %>%
    transmute(participant_id = {{ id_col }}, across(all_of(old_names), ~ likert_to_num(.x, level_order))) %>%
    rename_with(~ unname(labels[.x]), .cols = all_of(old_names))
}

# -- Hunting context (reason) (Q11, 8 items) --
reason_labels <- c(
  Q11_1 = "reason_food", Q11_2 = "reason_recreation", Q11_3 = "reason_population_control",
  Q11_4 = "reason_trophy", Q11_5 = "reason_reduce_conflict", Q11_6 = "reason_conservation_revenue",
  Q11_7 = "reason_indigenous_practice", Q11_8 = "reason_cultural_tradition"
)
reason_df <- recode_block(raw_data, ResponseId, reason_labels, lv_acceptable5)

# -- Hunting context (method) (Q12, 9 items) --
method_labels <- c(
  Q12_1 = "method_baiting", Q12_2 = "method_captive_hunt", Q12_3 = "method_trapping",
  Q12_4 = "method_neck_snares", Q12_5 = "method_dogs_predators", Q12_6 = "method_dogs_birds",
  Q12_7 = "method_aerial", Q12_8 = "method_bow", Q12_9 = "method_firearms"
)
method_df <- recode_block(raw_data, ResponseId, method_labels, lv_acceptable5)

# -- WVOs (Q13, 6 items) --
wvo_labels <- c(
  Q13_1 = "wvo_animal_rights",        # mutualism
  Q13_2 = "wvo_one_family",           # mutualism
  Q13_3 = "wvo_side_by_side",         # mutualism
  Q13_4 = "wvo_human_needs_priority", # domination
  Q13_5 = "wvo_wildlife_for_use",     # domination
  Q13_6 = "wvo_kill_if_threat"        # domination
)
wvo_df <- recode_block(raw_data, ResponseId, wvo_labels, lv_agree7)

# -- Identity (Q23, 9 items) --
identity_labels <- c(
  Q23_1 = "identify_hunters", Q23_2 = "identify_ranchers", Q23_3 = "identify_environmentalists",
  Q23_4 = "identify_farmers", Q23_5 = "identify_small_business", Q23_6 = "identify_landowners",
  Q23_7 = "identify_outdoor_rec", Q23_8 = "identify_outdoor_viewers", Q23_9 = "identify_anglers"
)
identity_df <- recode_block(raw_data, ResponseId, identity_labels, lv_identify)

# -- Christian Nationalism Scale (Q30, 6 items, Whitehead & Perry 2020) --
cns_labels <- setNames(paste0("cns_item", 1:6), paste0("Q30_", 1:6))
cns_df <- recode_block(raw_data, ResponseId, cns_labels, lv_agree5)

# -- Demographics & single-item measures --
demographics_df <- raw_data %>%
  transmute(
    participant_id        = ResponseId,
    duration_sec          = as.numeric(`Duration (in seconds)`),
    progress              = as.numeric(Progress),
    finished              = Finished == "True",
    #general_hunting_support = likert_to_num(Q11, lv_support)
    has_pets              = Q24 == "Yes",
    game_meat_freq        = likert_to_num(Q25, lv_game_freq),
    hunter_status         = factor(Q26, levels = c("No", "Yes, I know someone who hunts", "Yes, I hunt or have hunted in the past")),
    interested_in_hunting = likert_to_num(Q27_1, lv_agree5),  # asked only of non-hunters (Q26 == "No")
    hunted_species_raw    = Q28,                               # asked only of hunters
    eats_meat             = !is.na(Q35),
    political_ideology    = as.numeric(Q29_1)                  # 0 (liberal) - 100 (conservative) slider
  ) %>%
  mutate(
    hunted_ungulates       = str_detect(hunted_species_raw, "Ungulates"),
    hunted_birds_smallgame = str_detect(hunted_species_raw, "Birds or small game"),
    hunted_predators       = str_detect(hunted_species_raw, "Predators")
  )

person_level <- demographics_df %>%
  left_join(reason_df,   by = "participant_id") %>%
  left_join(method_df,   by = "participant_id") %>%
  left_join(wvo_df,      by = "participant_id") %>%
  left_join(identity_df, by = "participant_id") %>%
  left_join(cns_df,      by = "participant_id") %>%
  mutate(
    wvo_mutualism  = rowMeans(cbind(wvo_animal_rights, wvo_one_family, wvo_side_by_side), na.rm = TRUE),
    wvo_domination = rowMeans(cbind(wvo_human_needs_priority, wvo_wildlife_for_use, wvo_kill_if_threat), na.rm = TRUE),
    christian_nationalism_score = rowMeans(cbind(cns_item1, cns_item2, cns_item3, cns_item4, cns_item5, cns_item6), na.rm = TRUE)
  )

# -----------------------------------------------------------------------------
# MERGING PERSON-LEVEL COVARIATES ON LONG FILES
# -----------------------------------------------------------------------------
person_covariates <- person_level %>%
  select(participant_id, wvo_mutualism, wvo_domination, christian_nationalism_score,
         political_ideology, hunter_status, has_pets, game_meat_freq, eats_meat)

species_long <- species_long %>% left_join(person_covariates, by = "participant_id")
attacc_long   <- attacc_long   %>% left_join(person_covariates, by = "participant_id")

# -----------------------------------------------------------------------------
# 7. Write outputs
# -----------------------------------------------------------------------------
write_csv(species_long, "species_long.csv")
write_csv(attacc_long,   "attacc_long.csv")
write_csv(person_level, "person_level.csv")

cat("species_long: ", nrow(species_long), "rows (", n_distinct(species_long$participant_id), "participants x 17 species)\n")
cat("attacc_long:   ", nrow(attacc_long),   "rows (", n_distinct(attacc_long$participant_id), "participants x 4 focal species)\n")
cat("person_level: ", nrow(person_level), "rows\n")




## =============================================================================
## Wildlife Attitudes Survey — Data Dictionary
## =============================================================================

dict_entry <- function(file, variable, label, type, scale, source_question, notes = "") {
  tibble(file = file, variable = variable, label = label, type = type,
         scale_or_levels = scale, source_item = source_question, notes = notes)
}

data_dictionary <- bind_rows(
  
  ## ---- species_long.csv --------------------------------------------------
  dict_entry("species_long.csv", "participant_id", "Unique respondent ID", "character", "Qualtrics ResponseId", "ResponseId", ""),
  dict_entry("species_long.csv", "species", "Species name (17 species)", "character", paste(survey_sp, collapse = "; "), "derived from column position", ""),
  dict_entry("species_long.csv", "species_shown", "Was this species one of the 5 randomly assigned to this respondent for the characteristics battery (Q3-Q10)?", "logical", "TRUE/FALSE", "randomizer embedded-data columns", "Q1/Q2 were collected for all 17 species regardless of this flag; Q3-Q10 are NA when FALSE"),
  dict_entry("species_long.csv", "liking", "Species liking/disliking", "numeric (1-5)", "1=Strongly dislike ... 5=Strongly like", "Q1", ""),
  dict_entry("species_long.csv", "hunting_approval", "Approval of legal hunting of this species", "numeric (1-5)", "1=Strongly disapprove ... 5=Strongly approve", "Q2", ""),
  dict_entry("species_long.csv", "conservation_status", "Perceived conservation status", "ordered factor (1-5)", "Critically endangered < Threatened < Of concern/vulnerable < Stable/least concern < Overabundant", "Q3", "NA if respondent selected 'I don't know' -- see conservation_dontknow"),
  dict_entry("species_long.csv", "conservation_dontknow", "Respondent selected 'I don't know' for conservation status", "logical", "TRUE/FALSE", "Q3", ""),
  dict_entry("species_long.csv", "eating_appeal", "Appeal of eating meat from this species", "numeric (1-5)", "1=Very disgusting ... 5=Very appealing", "Q4", ""),
  dict_entry("species_long.csv", "encounter_freq", "Frequency of personally encountering species in the wild, last 5 years", "numeric (1-5)", "1=Never ... 5=More than ten times", "Q5", ""),
  dict_entry("species_long.csv", "desire_to_see", "Desire to see this species in the wild", "numeric (1-5)", "1=Not at all ... 5=A great deal", "Q6", ""),
  dict_entry("species_long.csv", "wtp_hunt", "Perceived willingness-to-pay to hunt this species (binned)", "numeric (1-6)", "1=$0, 2=Up to $100, 3=$101-$300, 4=$301-$500, 5=$501-$1000, 6=Over $1000", "Q7", "Treat as ordinal; midpoint recoding available on request"),
  dict_entry("species_long.csv", "wtp_view", "Perceived willingness-to-pay to view this species", "numeric (1-6)", "same bins as wtp_hunt", "Q8", ""),
  dict_entry("species_long.csv", "danger_humans", "Perceived danger to humans", "numeric (1-5)", "1=Not at all dangerous ... 5=Extremely dangerous", "Q9", ""),
  dict_entry("species_long.csv", "destructive_property", "Perceived destructiveness to property", "numeric (1-5)", "1=Not at all destructive ... 5=Extremely destructive", "Q10", ""),
  dict_entry("species_long.csv", "discrepancy_score", "hunting_approval - liking", "numeric (-4 to 4)", "positive = supports hunting more than liking predicts; negative = opposes hunting despite/because of liking", "derived", "Core attitude-acceptability decoupling measure at the species level"),
  
  ## ---- attacc_long.csv ------------------------------------------------------
  dict_entry("attacc_long.csv", "participant_id", "Unique respondent ID", "character", "Qualtrics ResponseId", "ResponseId", ""),
  dict_entry("attacc_long.csv", "attacc_species", "Focal species (4 species, asked of all respondents)", "character", paste(attacc_species, collapse = "; "), "Q15-Q22 blocks"),
  dict_entry("attacc_long.csv", "sem_harmful_beneficial", "Semantic differential: Harmful(1)-Beneficial(5)", "numeric (1-5)", "1=Harmful ... 5=Beneficial", "Q15/17/19/21 _1", ""),
  dict_entry("attacc_long.csv", "sem_unpleasant_pleasant", "Semantic differential: Unpleasant(1)-Pleasant(5)", "numeric (1-5)", "1=Unpleasant ... 5=Pleasant", "Q15/17/19/21 _2", ""),
  dict_entry("attacc_long.csv", "sem_bad_good", "Semantic differential: Bad(1)-Good(5)", "numeric (1-5)", "1=Bad ... 5=Good", "Q15/17/19/21 _3", ""),
  dict_entry("attacc_long.csv", "attacc_attitude", "Mean of the 3 semantic-differential items", "numeric (1-5)", "mean of sem_* items", "derived", "Overall focal-species attitude score"),
  dict_entry("attacc_long.csv", "control_overpopulated", "Agreement that lethal control is acceptable when species becomes overpopulated", "numeric (1-5)", "1=Strongly disagree ... 5=Strongly agree", "Q16/18/20/22 _1", "Lowest escalation level"),
  dict_entry("attacc_long.csv", "control_threat_interests", "Agreement that lethal control is acceptable when species threatens human interests", "numeric (1-5)", "1=Strongly disagree ... 5=Strongly agree", "Q16/18/20/22 _2", "Middle escalation level"),
  dict_entry("attacc_long.csv", "control_threat_lives", "Agreement that lethal control is acceptable when species threatens human lives", "numeric (1-5)", "1=Strongly disagree ... 5=Strongly agree", "Q16/18/20/22 _3", "Highest escalation level"),
  dict_entry("attacc_long.csv", "control_accept", "Mean of the 3 control-acceptability items", "numeric (1-5)", "mean of control_* items", "derived", "Overall lethal-control acceptability score"),
  dict_entry("attacc_long.csv", "discrepancy_score", "control_accept - focal_attitude", "numeric (-4 to 4)", "positive = more accepting of control than attitude predicts", "derived", "Core attitude-acceptability decoupling measure for the focal battery"),
  
  ## ---- person_level.csv: administrative -------------------------------------
  dict_entry("person_level.csv", "participant_id", "Unique respondent ID", "character", "Qualtrics ResponseId", "ResponseId", ""),
  dict_entry("person_level.csv", "duration_sec", "Survey completion time in seconds", "numeric", "", "Duration (in seconds)", ""),
  dict_entry("person_level.csv", "progress", "Percent of survey completed", "numeric (0-100)", "", "Progress", ""),
  dict_entry("person_level.csv", "finished", "Respondent reached the end of the survey", "logical", "TRUE/FALSE", "Finished", ""),
  
  ## ---- person_level.csv: reason acceptability (Q11) -------------------------
  dict_entry("person_level.csv", "reason_food", "Acceptability of hunting to obtain food", "numeric (1-5)", "1=Very unacceptable ... 5=Very acceptable", "Q11_1", ""),
  dict_entry("person_level.csv", "reason_recreation", "Acceptability of hunting for recreation/sport", "numeric (1-5)", "same scale", "Q11_2", ""),
  dict_entry("person_level.csv", "reason_population_control", "Acceptability of hunting to control wildlife populations", "numeric (1-5)", "same scale", "Q11_3", ""),
  dict_entry("person_level.csv", "reason_trophy", "Acceptability of hunting to obtain a trophy", "numeric (1-5)", "same scale", "Q11_4", ""),
  dict_entry("person_level.csv", "reason_reduce_conflict", "Acceptability of hunting to reduce human-wildlife conflict", "numeric (1-5)", "same scale", "Q11_5", ""),
  dict_entry("person_level.csv", "reason_conservation_revenue", "Acceptability of hunting to generate conservation revenue", "numeric (1-5)", "same scale", "Q11_6", ""),
  dict_entry("person_level.csv", "reason_indigenous_practice", "Acceptability of hunting as Indigenous/Tribal cultural practice", "numeric (1-5)", "same scale", "Q11_7", ""),
  dict_entry("person_level.csv", "reason_cultural_tradition", "Acceptability of hunting as cultural/family tradition", "numeric (1-5)", "same scale", "Q11_8", ""),
  
  ## ---- person_level.csv: method acceptability (Q12) -------------------------
  dict_entry("person_level.csv", "method_baiting", "Acceptability of hunting over bait", "numeric (1-5)", "1=Very unacceptable ... 5=Very acceptable", "Q12_1", ""),
  dict_entry("person_level.csv", "method_captive_hunt", "Acceptability of captive (fenced-in) hunts", "numeric (1-5)", "same scale", "Q12_2", ""),
  dict_entry("person_level.csv", "method_trapping", "Acceptability of trapping (leg-hold/body-grip)", "numeric (1-5)", "same scale", "Q12_3", ""),
  dict_entry("person_level.csv", "method_neck_snares", "Acceptability of neck snares", "numeric (1-5)", "same scale", "Q12_4", ""),
  dict_entry("person_level.csv", "method_dogs_predators", "Acceptability of using dogs to hunt predators", "numeric (1-5)", "same scale", "Q12_5", ""),
  dict_entry("person_level.csv", "method_dogs_birds", "Acceptability of using dogs to hunt birds", "numeric (1-5)", "same scale", "Q12_6", ""),
  dict_entry("person_level.csv", "method_aerial", "Acceptability of aerial hunting", "numeric (1-5)", "same scale", "Q12_7", ""),
  dict_entry("person_level.csv", "method_bow", "Acceptability of bow hunting", "numeric (1-5)", "same scale", "Q12_8", ""),
  dict_entry("person_level.csv", "method_firearms", "Acceptability of firearms hunting", "numeric (1-5)", "same scale", "Q12_9", ""),
  
  ## ---- person_level.csv: WVO (Q13) ------------------------------------------
  dict_entry("person_level.csv", "wvo_animal_rights", "Belief: animals should have rights similar to humans (mutualism)", "numeric (1-7)", "1=Strongly disagree ... 7=Strongly agree", "Q13_1", ""),
  dict_entry("person_level.csv", "wvo_one_family", "Belief: all living things are part of one family (mutualism)", "numeric (1-7)", "same scale", "Q13_2", ""),
  dict_entry("person_level.csv", "wvo_side_by_side", "Belief: humans and wildlife should live side by side without fear (mutualism)", "numeric (1-7)", "same scale", "Q13_3", ""),
  dict_entry("person_level.csv", "wvo_human_needs_priority", "Belief: human needs should take priority over wildlife protection (domination)", "numeric (1-7)", "same scale", "Q13_4", ""),
  dict_entry("person_level.csv", "wvo_wildlife_for_use", "Belief: wildlife exist primarily for people to use (domination)", "numeric (1-7)", "same scale", "Q13_5", ""),
  dict_entry("person_level.csv", "wvo_kill_if_threat", "Belief: acceptable to kill wildlife that threaten property (domination)", "numeric (1-7)", "same scale", "Q13_6", ""),
  dict_entry("person_level.csv", "wvo_mutualism", "Mutualism subscale score (mean of 3 items)", "numeric (1-7)", "mean of wvo_animal_rights, wvo_one_family, wvo_side_by_side", "derived", "Teel & Manfredo (2009) abbreviated wording"),
  dict_entry("person_level.csv", "wvo_domination", "Domination subscale score (mean of 3 items)", "numeric (1-7)", "mean of wvo_human_needs_priority, wvo_wildlife_for_use, wvo_kill_if_threat", "derived", "Teel & Manfredo (2009) abbreviated wording"),
  
  ## ---- person_level.csv: identity / symbolism (Q23) -------------------------
  dict_entry("person_level.csv", "identify_hunters", "Extent of personal identification with hunters/trappers", "numeric (1-5)", "1=Not at all ... 5=Extremely", "Q23_1", ""),
  dict_entry("person_level.csv", "identify_ranchers", "Extent of personal identification with ranchers", "numeric (1-5)", "same scale", "Q23_2", ""),
  dict_entry("person_level.csv", "identify_environmentalists", "Extent of personal identification with environmentalists", "numeric (1-5)", "same scale", "Q23_3", ""),
  dict_entry("person_level.csv", "identify_farmers", "Extent of personal identification with farmers", "numeric (1-5)", "same scale", "Q23_4", ""),
  dict_entry("person_level.csv", "identify_small_business", "Extent of personal identification with small business owners", "numeric (1-5)", "same scale", "Q23_5", ""),
  dict_entry("person_level.csv", "identify_landowners", "Extent of personal identification with private landowners", "numeric (1-5)", "same scale", "Q23_6", ""),
  dict_entry("person_level.csv", "identify_outdoor_rec", "Extent of personal identification with outdoor recreationists", "numeric (1-5)", "same scale", "Q23_7", ""),
  dict_entry("person_level.csv", "identify_outdoor_viewers", "Extent of personal identification with outdoor viewers (birders, photographers)", "numeric (1-5)", "same scale", "Q23_8", ""),
  dict_entry("person_level.csv", "identify_anglers", "Extent of personal identification with anglers/fishers", "numeric (1-5)", "same scale", "Q23_9", ""),
  
  ## ---- person_level.csv: Christian Nationalism Scale (Q30) ------------------
  dict_entry("person_level.csv", "cns_item1", "CNS item 1: federal government should declare US a Christian nation", "numeric (1-5)", "1=Strongly disagree ... 5=Strongly agree", "Q30_1", "Whitehead & Perry (2020)"),
  dict_entry("person_level.csv", "cns_item2", "CNS item 2", "numeric (1-5)", "same scale", "Q30_2", ""),
  dict_entry("person_level.csv", "cns_item3", "CNS item 3", "numeric (1-5)", "same scale", "Q30_3", ""),
  dict_entry("person_level.csv", "cns_item4", "CNS item 4", "numeric (1-5)", "same scale", "Q30_4", ""),
  dict_entry("person_level.csv", "cns_item5", "CNS item 5", "numeric (1-5)", "same scale", "Q30_5", ""),
  dict_entry("person_level.csv", "cns_item6", "CNS item 6", "numeric (1-5)", "same scale", "Q30_6", ""),
  dict_entry("person_level.csv", "christian_nationalism_score", "Mean of 6 CNS items", "numeric (1-5)", "mean of cns_item1-6", "derived", ""),
  
  ## ---- person_level.csv: demographics ---------------------------------------
  dict_entry("person_level.csv", "has_pets", "Currently owns/lives with pets", "logical", "TRUE/FALSE", "Q24", ""),
  dict_entry("person_level.csv", "game_meat_freq", "Frequency household consumes game meat", "numeric (1-4)", "1=Never ... 4=At least once per week", "Q25", ""),
  dict_entry("person_level.csv", "hunter_status", "Hunter identity / knows a hunter", "factor (3 levels)", "No; Yes, know someone who hunts; Yes, hunt(ed) personally", "Q26", ""),
  dict_entry("person_level.csv", "interested_in_hunting", "Interest in hunting in the future", "numeric (1-5)", "1=Strongly disagree ... 5=Strongly agree", "Q27_1", "Only asked of respondents who answered 'No' to Q26; NA otherwise"),
  dict_entry("person_level.csv", "hunted_species_raw", "Raw comma-separated species-type hunted", "character", "free text from multi-select", "Q28", "Only asked of respondents who hunt; see hunted_* dummy columns"),
  dict_entry("person_level.csv", "hunted_ungulates", "Has hunted ungulates (deer, elk)", "logical", "TRUE/FALSE", "Q28", "Derived from hunted_species_raw"),
  dict_entry("person_level.csv", "hunted_birds_smallgame", "Has hunted birds/small game", "logical", "TRUE/FALSE", "Q28", "Derived from hunted_species_raw"),
  dict_entry("person_level.csv", "hunted_predators", "Has hunted predators (wolves, coyotes, foxes)", "logical", "TRUE/FALSE", "Q28", "Derived from hunted_species_raw"),
  dict_entry("person_level.csv", "eats_meat", "Eats meat as part of regular diet", "logical", "TRUE/FALSE", "Q35", "Sparsely asked in this pilot -- confirm display logic before relying on this in the live survey"),
  dict_entry("person_level.csv", "political_ideology", "Political ideology slider", "numeric (0-100)", "0=most liberal ... 100=most conservative", "Q29_1", "")
)

write_csv(data_dictionary, "data_dictionary.csv")
cat("data_dictionary.csv written with", nrow(data_dictionary), "variable entries\n")
