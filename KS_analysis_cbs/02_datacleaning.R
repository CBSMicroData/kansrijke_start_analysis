##### Preprocessing plan #####
# Connect to SQL
# Import Perined2000_2020_e_CBKV1 into R
# Remove cases with missing child ID and save as Perined in SQL
# Remove unwanted variables from Perined
# Clean Perined
# Create new variables in Perined
# Save Perined to SQL
# Disconnect from SQL
# Save Perined.rds

# Data0 <- Perined
# Remove Perined

# Connect to SQL
# Import a table in R selecting only desired variables, matching for ID_ki, ID_ma, or ID_pa in Perined (whichever is relevant) to limit amount of data being imported
# If tables available throughout multiple years, repeat across all available years (range = 1999-2021)
# Disconnect from SQL
# Link to Data0/create new variables
# Clean variables
# Remove imported table(s)
# Repeat until all desired variables are included in Data0

# Save Data0.rds

##### Load libraries #####
library(dplyr)
library(dbplyr)
library(odbc)
library(DBI)
library(tidyverse)

sort(unique(odbcListDrivers()[[1]]))

##### Read and write access via owner account SQL Server #####
con <- dbConnect(odbc(), 
                 Driver = "ODBC Driver 13 for SQL Server",
                 Server = "S0DSQL0141B\\I01",
                 Database = "RA8552",
                 UID = "RA8552",
                 PWD = "keP452@DR")
# List all tables in SQL
dbListTables(con)

### importing Perined from SQL into tibble
Perined <- tbl(con, "Perined2000_2020_e_CBKV1") |> 
  select(CBKSOORTNR_KIND, RINPERSOON_KIND, CBKSOORTNR_MOEDER, RINPERSOON_MOEDER, jaar, ddgeb, ddgebm, lft, par, sterfte, 
         v_red_mort_vroeg, ddmort, v_sterfteperiode_kind, g_sterfteperiode_kind, amww, amddd, type_partus, beginbaring_basis,
         beginbaring_detail, omv, gesl, geboortegew, hoftiezer, apgar5, nicuopname, congenafw, etnic, pc4, ses, 
         ses_percentiel, bmi, rookgedrag, alcoholgebruik, whodef) |> 
  collect()

## Renaming columns
Perined <- Perined |> 
  rename("RINPERSOON_ki" = "RINPERSOON_KIND", "RINPERSOONS_ma" = "CBKSOORTNR_KIND",
         "RINPERSOON_ma" = "RINPERSOON_MOEDER", "RINPERSOONS_ma" = "CBKSOORTNR_MOEDER",
         "jaar_3" = "jaar", "dd_3" = "ddgeb", "ddgeb_ma" = "ddgebm", "lft_ma" = "lft",
         "sterfte6" = "sterfte", "rookgedrag_ma" = "rookgedrag", "alcoholgebruik_ma" = "alcoholgebruik")

## Creating ID variables by concatenating RINPERSOONS + RINPERSOON
Perined <- Perined |> 
  unite("ID_ki", RINPERSOONS_ki:RINPERSOON_ki, sep = "", remove = F)
Perined <- Perined |> 
  unite("ID_ma", RINPERSOONS_ma:RINPERSOON_ma, sep = "", remove = F)


## Cleaning variables
Perined <- Perined |> 
  mutate(
    ID_ki = ifelse(is.na(ID_ki) | ID_ki == "  " | ID_ki == "00:00:00R", NA, ID_ki),
    ID_ma = ifelse(is.na(ID_ma) | ID_ma == "" | ID_ma == "G---------" | ID_ma == "G ", NA, ID_ma),
    dd_3 = ymd(dd_3),
    dd_1 = dd_3 %m-% months(12),
    dd_2 = dd_3 %m-% months(9),
    dd_4 %m+% months(12),
    dd_5 %m+% months(24),
    jaar_3 = as.integer(jaar_3), # keep as integer for preprocessing; convert to factor for analyses
    jaar_1 = jaar3 - 1, # keep as integer for preprocessing; convert to factor for analyses
    jaar_2 = as.integer(year(dd_2))
    jaar_4 = jaar3 + 1, # keep as integer for preprocessing; convert to factor for analyses
    jaar_5 = jaar3 + 2, # keep as integer for preprocessing; convert to factor for analyses
    
  )

Perined[ID_MOEDER == "" | ID_MOEDER == "G---------" | ID_MOEDER == "G ", ID_MOEDER := NA]
nrow(Perined[is.na(ID_MOEDER)])

Perined[ID_VADER == "" | ID_VADER == "G---------" | ID_VADER == "G", ID_VADER := NA]
nrow(Perined[is.na(ID_VADER)])


## Creating new variables
ID_ma



## Exclude if missing ID_ki
Perined <- Perined |> 
  filter(!is.na(RINPERSOON_ki) & !is.na(RINPERSOON_ki))











##### Save Perined #####
# As table in SQL server
dbWriteTable(con, "Perined", Perined, overwrite = T, row.names = F)

### Disconnect from SQL
dbDisconnect(con)

# as Perined.rds
saveRDS(Data1, file = "Perined.rds")

# Number of rows in database
Perined |> tally()

# Summary of database
Perined |> glimpse()

# Check first 6 rows of a specified column
Perined |> select(jaar) |> head()

# Count number of each factor in a column
Perined |> as.tibble() |> count(jaar)

# Number of missing values in a column
Perined |> count(is.na(ID_ki))



##### Create Data0 #####
Data0 <- Perined

## Add rowID to Data0
Data0 <- Data0 |> mutate(rowID = seq.int(nrow(Data0)), .before = CBKSOORTNR_KIND)
