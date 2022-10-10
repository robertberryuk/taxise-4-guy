#> Load code libraries

# Data management
library(tidyverse)
# Relative paths
library(here)
#> Redlist taxonomic tools
library(rredlist)
# #> Redlist API
# library(redlistr)
# Taxonomic analysis
library(taxize)


#> Load data
IUCN.1 <- read_csv(here("In", "Raw_Data", "IUCN-Data-Testing.csv"))

#> Generate an IUCN API key variable
IUCN_REDLIST_KEY <- "a7fea8d57cc08aa668c3b656eb725a4430d35f82e2c407c2ea3bab40543dce89"

#> Get overall species count on red list
species.count <- rl_sp_count(key = IUCN_REDLIST_KEY)
species.count.df <- as.data.frame(species.count)

#> Get list of IUCN country codes and country names
countries.list <- rredlist::rl_countries(key = IUCN_REDLIST_KEY)
countries.df <- as.data.frame(countries.list)

#> Test retrieving species by country
#> Get species info for a specific country - example of DRC
species.DRC <- rredlist::rl_sp_country("CD", key = IUCN_REDLIST_KEY)
species.DRC.df <- as.data.frame(species.DRC)

#> Test the species occurence function:


#> Test retrieving species occurrence 
#> Get values from the scientific species name column
species <- as.list(df$scientificName)


convertNames <- function(x){
  
  taxize::iucn_id(x, key =  "a7fea8d57cc08aa668c3b656eb725a4430d35f82e2c407c2ea3bab40543dce89")
  
  
}
  

df$commonName <- map_df(df$scientificName, convertNames)
#> Export as CSV
write_csv(species.DRC.df, here("Out", "IUCN_Species_DRC.csv"))

?map_df


#> 





























?iucn_getname

