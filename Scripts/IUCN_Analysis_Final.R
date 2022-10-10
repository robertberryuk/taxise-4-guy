#> IUCN Analysis for Guy
#> Objective 1: Summary of IUCN status for each animal class - per country
#> Objective 2: Summary of IUCN population trend for each animal class - per country
#> Export results as individual country tables and combined (all countries) tables
#> Input data downloaded from IUCN - one input file per country


#> Load required R packages
# Data management
library(tidyverse)
# Relative paths
library(here)



#> Firstly, delete any previously generated output files created by this program
#> Create variable containing file path to output folder
fold.rem <- here("Out", "IUCN_Country_Analysis")
# get all files in the directories, recursively
f.rem <- list.files(fold.rem, include.dirs = F, full.names = T, recursive = T)
# Remove the files
file.remove(f.rem)


#> Create a list of input files (i.e. country CSVs)
fold.in <- here("In", "IUCN")
# get all files in the directories, recursively
f.in <- list.files(fold.in, include.dirs = F, full.names = T, recursive = T)


#> Create a list of ALL possible red list categories - this is required to "fill-in" any country tables that are missing a red list category / categories - so that all country tables have the same format and number of red list category columns
nms.rl.cats <- c("Not_Evaluated", "Data_Deficient", "Least_Concern", "Near_Threatened", "Vulnerable", "Endangered", "Critically_Endangered", "Extinct_in_the_Wild",  "Extinct") 



#> Master analysis function which imports data, runs analysis, and exports results as summary tables
fun_analysis <- function(x){
  
#> Import data (country CSV)
df <- read_csv(x)

#> Extract country name from input file name string
#> Get file name string
c.string <- x
#> Extract country using a regular expression. Solution from here: https://stackoverflow.com/questions/23518325/how-to-extract-substring-between-patterns-and-in-r
c.name <- gsub(".*[_]([^.]+)[.].*", "\\1", c.string)


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
  select(className, Not_Evaluated, Data_Deficient, Least_Concern, Near_Threatened, Vulnerable, Endangered, Critically_Endangered, Extinct_in_the_Wild, Extinct) |> 
  #Add country name column
  mutate(CountryName = c.name) |> 
  #> Move "CountryName" column to first position
  select(CountryName, everything())

#> Export as CSV
write_csv(df.class.final, here("Out", "IUCN_Country_Analysis", "By_Country", "Red_List_Categories", paste("RedListCat-", c.name, ".csv")))




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
  rename_all(~str_replace_all(., "\\s+", "_")) |> 
  #Add country name column
  mutate(CountryName = c.name) 

#> Reorder columns
sum.pop <- sum.pop |> 
  select(order(colnames(sum.pop)))|> 
  #> Move "CountryName" column to first position
  select(CountryName, everything())


#> Change NAs to zeros
sum.pop[is.na(sum.pop)] <- 0 


#> Export as CSV
write_csv(sum.pop, here("Out", "IUCN_Country_Analysis", "By_Country", "Population_Trend", paste0("PopTrend-", c.name, ".csv")))


}



#> Run the IUCN red list categories analysis function
run.analysis <- map(f.in, fun_analysis)





#> TASK 3 - Combine output tables into single unified tables (all countries)

#> Combined redlist summary table (all countries)
#> Create folder path variable
fold.out.redlist <- here("Out", "IUCN_Country_Analysis", "By_Country", "Red_List_Categories")
#> Read files and combine into single data frame
df.redlist.all <- list.files( path=fold.out.redlist, full.names=TRUE ) %>% 
  map_dfr(read_csv) # _dfr to be explicit about *row* binding
#> Export as CSV
write_csv(df.redlist.all, here("Out", "IUCN_Country_Analysis", "All-Combined", "Summary_RedList_All-Countries.csv"))


#> Combined population trennd summary table (all countries)
#> Create folder path variable
fold.out.poptrend <- here("Out", "IUCN_Country_Analysis", "By_Country", "Population_Trend")
#> Read files and combine into single data frame
df.redlist.all <- list.files( path=fold.out.poptrend, full.names=TRUE ) %>% 
  map_dfr(read_csv) # _dfr to be explicit about *row* binding
#> Export as CSV
write_csv(df.redlist.all, here("Out", "IUCN_Country_Analysis", "All-Combined", "Summary_PopTrend_All-Countries.csv"))



