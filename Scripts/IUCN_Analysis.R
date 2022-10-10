#> IUCN Analysis for Guy
#> Objective 1: Summary of IUCN status for each animal class - per country
#> Objective 2: Summary of IUCN population trend for each animal class - per country
#> Export results as individual country tables and append 
#> Input data downloaded from IUCN - one input file per coutry


#> Load required R packages

# Data management
library(tidyverse)
# Relative paths
library(here)


#> Create a list of ALL possible red list categories - this is required to "fill-in" any country tables that are missing a red list category / categories - so that all country tables have the same format and number of red list category columns
nms.rl.cats <- c("Not_Evaluated", "Data_Deficient", "Least_Concern", "Near_Threatened", "Vulnerable", "Endangered", "Critically_Endangered", "Extinct_in_the_Wild",  "Extinct") 


#> Import test data
df <- read_csv(here("In", "IUCN", "simple_summary_Afghanistan.csv"))

#> TASK 1 - Summarise className by red list category ####

#> Create a summary count of classes by red list category
sum.class <- df |> 
  #> group data by class and red list category
  group_by(className, redlistCategory) |> 
  #> Count observations by group
  summarise(Count = n()) |> 
  #> Spread the table (sp that red list categories become columns)
  pivot_wider(names_from = redlistCategory, values_from = Count) |> 
  #> replace spaces in column names with underscores
  rename_all(~str_replace_all(., "\\s+", "_"))
 

#> Find missing red list category columns based on the "nms.rl.cats" list generated previously
missing.rl <- setdiff(nms.rl.cats, names(sum.class))

#> Add missing columns
sum.class[missing.rl] <- 0

#> Change all NAs in data frame to zeros
sum.class[is.na(sum.class)] <- 0

#> Reorder columns for consistency
#> Order from IUCN website is: "Not Evaluated, Data Deficient, Least Concern, Near Threatened, Vulnerable, Endangered, Critically Endangered, Extinct in the Wild and Extinct".
df.class.final <- sum.class |> 
  select(className, Not_Evaluated, Data_Deficient, Least_Concern, Near_Threatened, Vulnerable, Endangered, Critically_Endangered, Extinct_in_the_Wild, Extinct)

#> Export as CSV
write_csv(df.class.final, here("Out", "IUCN_Test", "RedListCat-Afghanistan.csv"))


#APPEND TO MAIN TABLE IN FULL SCRIPT




#> TASK 2 - Summarise and count "populationTrend" by red list category ####

#> Create a list of ALL possible population trend categories - this is required to "fill-in" any country tables that are missing a population trend category / categories - so that all country tables have the same format and number of population trend category columns
nms.pop.cats <- c("Increasing", "Decreasing", "Stable", "Unknown" ) 

#> Create a summary count of classes by population trend catagory
sum.pop <- df |> 
  #> Group data
  group_by(className, populationTrend) |> 
  #> Count observations by group
  summarise(Count = n()) |> 
  #> Spread the table (red list categories as columns)
  pivot_wider(names_from = populationTrend, values_from = Count) |> 
  #> Replace spaces in column names with underscores
  rename_all(~str_replace_all(., "\\s+", "_")) 

#> Reorder columns
sum.pop <- sum.pop |> 
  select(order(colnames(sum.pop)))

#> Change NAs to zeros
sum.pop[is.na(sum.pop)] <- 0 


#> Export as CSV
write_csv(sum.pop, here("Out", "IUCN_Test", "PopTrend-Afghanistan.csv"))








