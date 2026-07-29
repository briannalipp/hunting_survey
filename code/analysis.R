## =============================================================================
## Wildlife Attitudes Survey
## =============================================================================

library(here)
library(tidyverse)

###   Pulling in Data   ###
species_responses <- readRDS(here("data", "species_responses.rds"))
attacc_long       <- readRDS(here("data", "attacc_long.rds"))
personal          <- readRDS(here("data", "personal.rds"))