#> Preparing data biodiversity and invasive species for Guy's PhD. Code by Robert Berry - rberry@glos.ac.uk
#> Task 1 BIODIVERSITY: Join split columns and merge individual country files into one large file. Create long version and wide version (years as columns)
#> Task 2 INVASIVE SPECIES: Group data by class and calculate grouped statistics as specified in Guy's email



#> Load required code libraries (R packages)
library(tidyverse) # data munging 
library(here) # relative file path management



#> Task 1: BIODIVERSITY DATA PROCESSING ####

#> Get path to main biodiversity input data folder
project.folder <- here("In", "Data_Analysis_May23", "Bio_Intact_Index_data_for_R")
#> Get paths to sub-folders (one for each variable to process)
main.folders <- list.dirs(project.folder, recursive = FALSE)
#> Loop through the folders - each folder is a particular variable (e.g. crop area)
for(i in 1:length(main.folders)){
#> Target folder as variable
fold.in <- main.folders[i]
#> List input CSVs, read-in, and bind together into a single table
files <- list.files(fold.in, include.dirs = F, full.names = T, recursive = T)
#> Specify new column names (original column names too long)
new.colnames <- c("Year", "CropArea_Historical", "CropArea_Projected")
#> Create list to hold imported and cleaned files
files.list <- list()
#> Loop through each file in "files", change column names, and add dataframe object to list
for(i in 1:length(files)){
  #> Read CSV
  x <- read.csv(files[i])
  #> Get the country name from the file name string
  str.filename <- basename(files[i])
  str.country <- sub("^.*?-(.*?)-.*$", "\\1", str.filename)
  #> Get the variable name from the file name string
  str.folder <- basename(fold.in)
  # #> Find the index of the first underscore using grep
  # underscore_pos <- grep("_", main.folders)[i]
  # #> Extract characters after the first underscore using substring
  # str.var <- substr(str.folders, underscore_pos + 1, nchar(str.folders))
  #> Check if the variable name contains "ssp" - these files have a slightly different format and their columns needs to be manipulated differently to the other variables 
  if(grepl("ssp", str.folder, ignore.case = TRUE)){
    #> Select columns 1, 2, & 5
    x <- subset(x, select = c(1, 2, 5))
  } else {
    #> Select first three columns 
    x <- x[,1:3]
  }
   #> Change column names
  colnames(x) <- new.colnames
  #> Add new column "Country" and populate with country name string extracted above
  x$Country <- str.country
  #> Move new "Country" column to first position
  x <- x |> 
    select(Country, everything())
  #> Add data frame to list "files.list"
  files.list[[i]] <- x
  #> Close inner loop (country files)
}

#> Bind the list of cleaned data frames into a single table
data <- do.call(rbind.data.frame, files.list)
#> Combine data columns
data$CropArea_Combined <- coalesce(data$CropArea_Historical, data$CropArea_Projected)
#> Remove unwanted columns
data <- data |> 
  select(Country, Year, CropArea_Combined) |> 
  filter(Year >=2011 & Year <=2023)
#> Export long version of the data frame
write_csv(data, here("Out", "Data_Processing4Guy_230425", "Bio_Intact_Index_data_for_R", str.folder, paste0("Processed_RB_LONG.csv")))
#> Widen data (years into columns
df_wide <- pivot_wider(data, names_from = Year, values_from = CropArea_Combined)
#> )
#> Export wide version of the data frame (years as columns)
write_csv(df_wide, here("Out", "Data_Processing4Guy_230425", "Bio_Intact_Index_data_for_R", str.folder, paste0("Processed_RB_WIDE.csv")))

#> Close outer loop (variable folders)
}





#> Task 2: INVASIVE SPECIES DATA PROCESSING ####

#> First import all files and create single table so that unique categories for each target variable can be caluclated prior to creating sumamrary stats
#> Path to input files folder
fold.in <- here("In", "Data_Analysis_May23", "Invasive_Species_for_R_analysis", "griis")
#> List files
files <- list.files(fold.in, include.dirs = F, full.names = T, recursive = T)
#> Create list to hold imported and cleaned files
files.list <- list()

for(i in 1:length(files)){
  #> Read CSV
  x <- read.csv(files[i])
  #> Select relevant columns 
  x <- x |> 
    select(class, isInvasive, establishmentMeans)
  #> Get the country name from the file name string
  str.filename <- basename(files[i])
  str.country <- sub("^[^-]*-(.*?)_.*$", "\\1", str.filename)
  #> Add new column "Country" and populate with country name string extracted above
  x$Country <- str.country
  #> Move new "Country" column to first position
  x <- x |> 
    select(Country, everything())
  #> Add data frame to list "files.list"
  files.list[[i]] <- x
#> Close loop
}
#> Bind the list of cleaned data frames into a single table
data <- do.call(rbind.data.frame, files.list)
#> Inspect unique values for target variable columns
isInvasive.unique <- as.data.frame(unique(data$isInvasive))
#> Export as CSV
write_csv(isInvasive.unique, here("Out", "Data_Processing4Guy_230425", "Invasive_Unique_Values_Check", "isInvasive_Unique_Values.csv"))
establishment.unique <- as.data.frame(unique(data$establishmentMeans))
write_csv(establishment.unique, here("Out", "Data_Processing4Guy_230425", "Invasive_Unique_Values_Check", "establishment_Unique_Values.csv"))
  
#> Calculate summary stats
data.sum <- data|> 
  select(Country, class, isInvasive, establishmentMeans) |> 
  arrange(Country, class) |> 
  group_by(Country, class) |> 
  summarise(
    Invasive_Y = sum(isInvasive=="Invasive", isInvasive=="invasive", isInvasive=="yes", isInvasive=="YES", isInvasive=="TRUE", isInvasive=="Invasive?", isInvasive=="Invasive in the north of the island (122)."),
    Invasive_NULL = sum(isInvasive=="Null", isInvasive=="FALSE", is.na(isInvasive)),
    Est_Alien = sum(establishmentMeans=="Alien", establishmentMeans=="alien"),
    Est_Native_Alien = sum(establishmentMeans=="Native|Alien", establishmentMeans=="native|alien", establishmentMeans=="Alien/Native"),
    Est_Native_Invasive = sum(establishmentMeans=="Native|Invasive"), 
    Est_Crypto_Uncertain = sum(establishmentMeans=="Cryptogenic|Uncertain", establishmentMeans=="cryptogenic|uncertain", establishmentMeans=="Cryptogenic|Unknown", establishmentMeans=="Cryptogenic/uncertain", establishmentMeans=="cryptogenic/uncertain", establishmentMeans=="Cryptogenic|uncertain", establishmentMeans=="Cryptogenic|Uncerain"),
    Est_Uncertain = sum(establishmentMeans=="Uncertain"),
    Est_Present = sum(establishmentMeans=="Present"),
    Est_Introduced = sum(establishmentMeans=="introduced"),
    Est_Blank = sum(establishmentMeans=="")) 
#> Export as CSV
write_csv(data.sum, here("Out", "Data_Processing4Guy_230425", "Invasive_Unique_Values_Check", "Invasive_Species_Processed_RB.csv"))
 



