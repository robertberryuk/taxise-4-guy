#> Load code libraries

# Data management
library(tidyverse)
# Relative paths
library(here)
#> rredlist package for scientifc name conversion and IUCN analysis
library(rredlist)
#> taxize library
library(taxize)

#> SECTION 1 - TEST SCRIPT TO PROCESS AND ANALYSE A SINGLE COUNTRY ####

#> Generate an IUCN API key variable
IUCN_REDLIST_KEY <- "a7fea8d57cc08aa668c3b656eb725a4430d35f82e2c407c2ea3bab40543dce89"


#> Import test data
df <- read_csv(here("In", "IUCN", "Test", "simple_summary_Afghanistan.csv"))


# #> Convert scientific name to common name and add new column

#> See: https://bioinformatics.stackexchange.com/questions/578/how-to-convert-species-names-into-common-names
# df$UID <- get_uid(df$scientificName)
#
# df <- df |>
#   select(UID, everything())
#
# species <- df$scientificName
#
# uids <- get_uid(species)
#
# uids_df <- as.data.frame(uids)



