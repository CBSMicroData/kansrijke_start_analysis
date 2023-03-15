### Run data generation file
source("src/generate_data.R")

##### import .sav files #####
### List all .sav files in project directory
listoffiles <- list.files(path = ".", pattern = ".sav", all.files = FALSE, full.names = FALSE, recursive = TRUE)
listoffiles

### Create names for each sav file
library(stringr)
listofnames <- list()
n = 1
for (i in listoffiles) {
  listofnames[n] <- gsub(" ", "", str_split(str_split(i, "/")[[1]][4], ".sav")[[1]][1])
  n <- n + 1
}
listofnames

# rename data.frames starting with numbers
listofnames[[6]] <- "PRN2008V2"
listofnames[[7]] <- "PRN2009V2"
listofnames[[8]] <- "PRN2010V1"
listofnames[[9]] <- "PRN2011V1"
listofnames[[10]] <- "PRN2012V1"

### Import all .sav files
library(haven)
n = 1
for (i in listoffiles) {
  assign(paste0(listofnames[n]), read_sav(i))
  n <- n + 1
}

# Number of unique people = 174150
table(duplicated(rinobjectnummer))

# Are all people included in all tables? YES
table(rinpersoon %in% ZVWZORGKOSTEN2015TABV1$RINPERSOON)

# Do some tables include the same people multiple times? YES
table(duplicated(ZVWZORGKOSTEN2015TABV1$RINPERSOON))


### append data.frames from multiple years, creating a new column to indicate year
## Create list of data.frames
# listofdfs <- list(GBAADRESOBJECT2018V1, GBAPERSOON2018TABV2, HOOGSTEOPL2014TABV3, HOOGSTEOPL2015TABV3,
#                   HOOGSTEOPL2016TABV2, HOOGSTEOPL2017TABV3, HOOGSTEOPL2018TABV3, INPA2011TABV2,
#                   INPA2012TABV2, INPA2013TABV2, INPA2014TABV2, INPA2015TABV2, INPA2016TABV3, INPA2017TABV3,
#                   INPA2018TABV2, INSCHRWPOTAB2014V4, INSCHRWPOTAB2015V5, INSCHRWPOTAB2016V4, INSCHRWPOTAB2017V5,
#                   INSCHRWPOTAB2018V3, JGDBESCHERM2015BUSV2, JGDBESCHERM2016BUSV3, JGDBESCHERM2017BUSV3,
#                   JGDBESCHERM2018BUSV3, JGDBESCHERM2019BUSV2, KINDOUDER2018TABV1, PERSOONINK2006TABV3,
#                   PERSOONINK2007TABV3, PERSOONINK2008TABV3, PERSOONINK2009TABV2, PERSOONINK2010TABV3, PRN2008V2,
#                   PRN2009V2, PRN2010V1, PRN2011V1, PRN2012V1, PRN2013V1, PRN2014V2, PRN2015V1, PRN2016V1,
#                   SECMBUS2018V1, SPOLISBUS2017TABV5, SPOLISBUS2018TABV2, VSLGWB2019TAB03V1,
#                   VSLPOSTCODEBUSV2020031, ZVWZORGKOSTEN2015TABV1, ZVWZORGKOSTEN2016TABV1, ZVWZORGKOSTEN2017TABV1)

## Create new column for each table representing the year
HOOGSTEOPL2014TABV3$jaar <- as.factor(as.character(2014))
HOOGSTEOPL2015TABV3$jaar <- as.factor(as.character(2015))
HOOGSTEOPL2016TABV2$jaar <- as.factor(as.character(2016))
HOOGSTEOPL2017TABV3$jaar <- as.factor(as.character(2017))
HOOGSTEOPL2018TABV3$jaar <- as.factor(as.character(2018))
INPA2011TABV2$jaar <- as.factor(as.character(2011))
INPA2012TABV2$jaar <- as.factor(as.character(2012))
INPA2013TABV2$jaar <- as.factor(as.character(2013))
INPA2014TABV2$jaar <- as.factor(as.character(2014))
INPA2015TABV2$jaar <- as.factor(as.character(2015))
INPA2016TABV3$jaar <- as.factor(as.character(2016))
INPA2017TABV3$jaar <- as.factor(as.character(2017))
INPA2018TABV2$jaar <- as.factor(as.character(2018))
INSCHRWPOTAB2014V4$jaar <- as.factor(as.character(2014))
INSCHRWPOTAB2015V5$jaar <- as.factor(as.character(2015))
INSCHRWPOTAB2016V4$jaar <- as.factor(as.character(2016))
INSCHRWPOTAB2017V5$jaar <- as.factor(as.character(2017))
INSCHRWPOTAB2018V3$jaar <- as.factor(as.character(2018))
JGDBESCHERM2015BUSV2$jaar <- as.factor(as.character(2011))
JGDBESCHERM2016BUSV3$jaar <- as.factor(as.character(2011))
JGDBESCHERM2017BUSV3$jaar <- as.factor(as.character(2011))
JGDBESCHERM2018BUSV3$jaar <- as.factor(as.character(2011))
JGDBESCHERM2019BUSV2$jaar <- as.factor(as.character(2011))
PERSOONINK2006TABV3$jaar <- as.factor(as.character(2006))
PERSOONINK2007TABV3$jaar <- as.factor(as.character(2007))
PERSOONINK2008TABV3$jaar <- as.factor(as.character(2008))
PERSOONINK2009TABV2$jaar <- as.factor(as.character(2009))
PERSOONINK2010TABV3$jaar <- as.factor(as.character(2010))
PRN2008V2$jaar <- as.factor(as.character(2008))
PRN2009V2$jaar <- as.factor(as.character(2009))
PRN2010V1$jaar <- as.factor(as.character(2010))
PRN2011V1$jaar <- as.factor(as.character(2011))
PRN2012V1$jaar <- as.factor(as.character(2012))
PRN2013V1$jaar <- as.factor(as.character(2013))
PRN2014V2$jaar <- as.factor(as.character(2014))
PRN2015V1$jaar <- as.factor(as.character(2015))
PRN2016V1$jaar <- as.factor(as.character(2016))
SPOLISBUS2017TABV5$jaar <- as.factor(as.character(2017))
SPOLISBUS2018TABV2$jaar <- as.factor(as.character(2018))
ZVWZORGKOSTEN2015TABV1$jaar <- as.factor(as.character(2015))
ZVWZORGKOSTEN2016TABV1$jaar <- as.factor(as.character(2016))
ZVWZORGKOSTEN2017TABV1$jaar <- as.factor(as.character(2017))


## appending rows of equivalent tables
HOOGSTEOPL <- rbind(HOOGSTEOPL2014TABV3, HOOGSTEOPL2015TABV3, HOOGSTEOPL2016TABV2, HOOGSTEOPL2017TABV3, HOOGSTEOPL2018TABV3)
INPA <- rbind(INPA2011TABV2, INPA2012TABV2, INPA2013TABV2, INPA2014TABV2, INPA2015TABV2, INPA2016TABV3, INPA2017TABV3, INPA2018TABV2)
INSCHRWPO <- rbind(INSCHRWPOTAB2014V4, INSCHRWPOTAB2015V5, INSCHRWPOTAB2016V4, INSCHRWPOTAB2017V5, INSCHRWPOTAB2018V3)
JGDBESCHERM <- rbind(JGDBESCHERM2015BUSV2, JGDBESCHERM2016BUSV3, JGDBESCHERM2017BUSV3, JGDBESCHERM2018BUSV3, JGDBESCHERM2019BUSV2)
PERSOONINK <- rbind(PERSOONINK2006TABV3, PERSOONINK2007TABV3, PERSOONINK2008TABV3, PERSOONINK2009TABV2, PERSOONINK2010TABV3)
PRN <- rbind(PRN2008V2, PRN2009V2, PRN2010V1, PRN2011V1, PRN2012V1, PRN2013V1, PRN2014V2, PRN2015V1, PRN2016V1)
SPOLIS <- rbind(SPOLISBUS2017TABV5, SPOLISBUS2018TABV2)
ZVWZORGKOSTEN <- rbind(ZVWZORGKOSTEN2015TABV1, ZVWZORGKOSTEN2016TABV1, ZVWZORGKOSTEN2017TABV1)

## remove appended tables
rm(HOOGSTEOPL2014TABV3, HOOGSTEOPL2015TABV3,
   HOOGSTEOPL2016TABV2, HOOGSTEOPL2017TABV3, HOOGSTEOPL2018TABV3, INPA2011TABV2,
   INPA2012TABV2, INPA2013TABV2, INPA2014TABV2, INPA2015TABV2, INPA2016TABV3, INPA2017TABV3,
   INPA2018TABV2, INSCHRWPOTAB2014V4, INSCHRWPOTAB2015V5, INSCHRWPOTAB2016V4, INSCHRWPOTAB2017V5,
   INSCHRWPOTAB2018V3, JGDBESCHERM2015BUSV2, JGDBESCHERM2016BUSV3, JGDBESCHERM2017BUSV3,
   JGDBESCHERM2018BUSV3, JGDBESCHERM2019BUSV2, PERSOONINK2006TABV3,
   PERSOONINK2007TABV3, PERSOONINK2008TABV3, PERSOONINK2009TABV2, PERSOONINK2010TABV3, PRN2008V2,
   PRN2009V2, PRN2010V1, PRN2011V1, PRN2012V1, PRN2013V1, PRN2014V2, PRN2015V1, PRN2016V1,
   SPOLISBUS2017TABV5, SPOLISBUS2018TABV2, ZVWZORGKOSTEN2015TABV1, ZVWZORGKOSTEN2016TABV1, ZVWZORGKOSTEN2017TABV1)

## PRN, GBAPERSOON2018TABV2, VSLPOSTCODEBUSV2020031, VSLGWB2019TAB03V1, and GBAADRESOBJECT2018V1 (overlap in dates) cannot include duplicates
## Will only keep data from 2008 from PRN and remove all other duplicates
PRN <- PRN |> 
  filter(jaar == 2008, !duplicated(RINPERSOON_KIND))
GBAPERSOON2018TABV2 <- GBAPERSOON2018TABV2 |> 
  filter(!duplicated(RINPERSOON))
VSLPOSTCODEBUSV2020031 <- VSLPOSTCODEBUSV2020031 |> 
  filter(!duplicated(RINOBJECTNUMMER))
VSLGWB2019TAB03V1 <- VSLGWB2019TAB03V1 |> 
  filter(!duplicated(RINOBJECTNUMMER))
table(duplicated(GBAADRESOBJECT2018V1$RINPERSOON)) #GBAADRESOBJECT2018V1 doesn't have duplicated RINPERSOON



##### Creating Data1 #####
library(tidyverse)
### Creating Data1
Data1 <- PRN

#### Renaming columns
### Adding "_1", "_2", "_3", "_4", and "_5" to a variable name represent time points of
### "at conception", "during pregnancy", "at birth", "at 1 year of age", and "at 2 years of age",
### respectively
Data1 <- Data1 |> 
  rename("RINPERSOONS_ki" = "RINPERSOONS_KIND_UITGEBREID", "RINPERSOON_ki" = "RINPERSOON_KIND", "jaar_3" = "jaar", 
         "gesl" = "Geslachtkind", "amddd" = "Amddd", "geboortegew" = "Gewichtkind_ruw")

### Create ID_ki (concatenation of RINPERSOONS and RINPERSOON)
Data1 <- Data1 |> 
  mutate(ID_ki = paste0(RINPERSOONS_ki, RINPERSOON_ki))

### Cleaning data
# Check column classes
sapply(Data1, class)

# Convert to factor
Data1 <- Data1 |> 
  mutate_at(c("gesl"), as.character) |> 
  mutate_at(c("gesl"), as.factor)
sapply(Data1, class)

### Creating variables
# preterm birth (1 = yes, 0 = no)
# low birth weight (1 = yes, 0 = no)
Data1 <- Data1 |> 
  mutate(preterm = as.factor(as.character(ifelse(amddd < (37*7), 1, 0))),
         lbw = as.factor(as.character(ifelse(geboortegew < 2500, 1, 0))))


##### Linking parents #####
Data1 <- Data1 |> 
  left_join(KINDOUDER2018TABV1, by = c("RINPERSOONS_ki" = "RINPERSOONS",
                                       "RINPERSOON_ki" = "RINPERSOON"))
### Renaming columns
Data1 <- Data1 |> 
  rename("RINPERSOON_ma" = "RINPERSOONMa", "RINPERSOONS_ma" = "RINPERSOONSMa",
         "RINPERSOON_pa" = "RINPERSOONpa", "RINPERSOONS_pa" = "RINPERSOONSpa")

## Exclude if missing both parents
Data1 <- Data1 |> 
  filter(!is.na(RINPERSOON_ma) & !is.na(RINPERSOON_pa))

## Creating ID_ma and ID_pa
Data1 <- Data1 |> 
  mutate(ID_ma = paste0(RINPERSOONS_ma, RINPERSOON_ma), ID_pa = paste0(RINPERSOONS_pa, RINPERSOON_pa))


### If ID_ki = ID_ma or ID_pa, ID_ma or ID_pa <- NA (BECAUSE THERE IS AN OVERLAP BETWEEN CHILDREN'S IDS AND PARENT'S IDS)
Data1 <- Data1 |> 
  mutate(RINPERSOONS_ma = ifelse(!is.na(ID_ma) & ID_ma %in% ID_ki, NA, RINPERSOONS_ma),
         RINPERSOON_ma = ifelse(!is.na(ID_ma) & ID_ma %in% ID_ki, NA, RINPERSOON_ma),
         ID_ma = ifelse(!is.na(ID_ma) & ID_ma %in% ID_ki, NA, ID_ma),
         RINPERSOONS_pa = ifelse(!is.na(ID_pa) & (ID_pa %in% ID_ki | ID_pa %in% ID_ma), NA, RINPERSOONS_pa),
         RINPERSOON_pa = ifelse(!is.na(ID_pa) & (ID_pa %in% ID_ki | ID_pa %in% ID_ma), NA, RINPERSOON_pa),
         ID_pa = ifelse(!is.na(ID_pa) & (ID_pa %in% ID_ki | ID_pa %in% ID_ma), NA, ID_pa))

### Excluding children without at least one registered parent
Data1 <- Data1 |> 
  filter(!is.na(ID_ma) | !is.na(ID_pa))

##### Linking residence, postcode, municipality, district, neighborhood #####
### Check whether dates in GBAADRESOBJECT2018V1 into date class
sapply(GBAADRESOBJECT2018V1, class)

#### For child
### Assuming that time of conception was 01-07-2017 (mid-2017)
Data1 <- Data1 |> 
  left_join(GBAADRESOBJECT2018V1[GBAADRESOBJECT2018V1$GBADATUMAANVANGADRESHOUDING < "2017-07-01" & 
                                   GBAADRESOBJECT2018V1$GBADATUMEINDEADRESHOUDING > "2017-06-30", 
                                 c("RINPERSOONS", "RINPERSOON", "SOORTOBJECTNUMMER", "RINOBJECTNUMMER")], 
            by = c("RINPERSOONS_ki" = "RINPERSOONS", "RINPERSOON_ki" = "RINPERSOON"))

Data1 <- Data1 |> 
  left_join(VSLPOSTCODEBUSV2020031[, c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "POSTCODENUM")], 
            by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))

Data1 <- Data1 |> 
  left_join(VSLGWB2019TAB03V1, 
            by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))

# Renaming variables
Data1 <- Data1 |> 
  rename("SOORTOBJECTNUMMER_ki" = "SOORTOBJECTNUMMER", "RINOBJECTNUMMER_ki" = "RINOBJECTNUMMER",
         "postcode4_ki" = "POSTCODENUM", "gemeentecode_ki" = "Gem2019",
         "wijkcode_ki" = "WC2019", "buurtcode_ki" = "BC2019")


#### For mother
### Assuming that time of conception was 01-07-2017 (mid-2017)
Data1 <- Data1 |> 
  left_join(GBAADRESOBJECT2018V1[GBAADRESOBJECT2018V1$GBADATUMAANVANGADRESHOUDING < "2017-07-01" & 
                                   GBAADRESOBJECT2018V1$GBADATUMEINDEADRESHOUDING > "2017-06-30", 
                                 c("RINPERSOONS", "RINPERSOON", "SOORTOBJECTNUMMER", "RINOBJECTNUMMER")], 
            by = c("RINPERSOONS_ma" = "RINPERSOONS", "RINPERSOON_ma" = "RINPERSOON"))

Data1 <- Data1 |> 
  left_join(VSLPOSTCODEBUSV2020031[, c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "POSTCODENUM")], 
            by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))

Data1 <- Data1 |> 
  left_join(VSLGWB2019TAB03V1, 
            by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))

# Renaming variables
Data1 <- Data1 |> 
  rename("SOORTOBJECTNUMMER_ma" = "SOORTOBJECTNUMMER", "RINOBJECTNUMMER_ma" = "RINOBJECTNUMMER",
         "postcode4_ma" = "POSTCODENUM", "gemeentecode_ma" = "Gem2019",
         "wijkcode_ma" = "WC2019", "buurtcode_ma" = "BC2019")

#### For father
### Assuming that time of conception was 01-07-2017 (mid-2017)
Data1 <- Data1 |> 
  left_join(GBAADRESOBJECT2018V1[GBAADRESOBJECT2018V1$GBADATUMAANVANGADRESHOUDING < "2017-07-01" & 
                                   GBAADRESOBJECT2018V1$GBADATUMEINDEADRESHOUDING > "2017-06-30", 
                                 c("RINPERSOONS", "RINPERSOON", "SOORTOBJECTNUMMER", "RINOBJECTNUMMER")], 
            by = c("RINPERSOONS_pa" = "RINPERSOONS", "RINPERSOON_pa" = "RINPERSOON"))

Data1 <- Data1 |> 
  left_join(VSLPOSTCODEBUSV2020031[, c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "POSTCODENUM")], 
            by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))

Data1 <- Data1 |> 
  left_join(VSLGWB2019TAB03V1, 
            by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))

# Renaming variables
Data1 <- Data1 |> 
  rename("SOORTOBJECTNUMMER_pa" = "SOORTOBJECTNUMMER", "RINOBJECTNUMMER_pa" = "RINOBJECTNUMMER",
         "postcode4_pa" = "POSTCODENUM", "gemeentecode_pa" = "Gem2019",
         "wijkcode_pa" = "WC2019", "buurtcode_pa" = "BC2019")

# Transforming postcode4, gemeentecode, wijkcode, and buurtcode into factors
Data1 <- Data1 |> 
  mutate(postcode4_ki = as.factor(as.character(postcode4_ki)),
         gemeentecode_ki = as.factor(as.character(gemeentecode_ki)),
         wijkcode_ki = as.factor(as.character(wijkcode_ki)),
         buurtcode_ki = as.factor(as.character(buurtcode_ki)),
         postcode4_ma = as.factor(as.character(postcode4_ma)),
         gemeentecode_ma = as.factor(as.character(gemeentecode_ma)),
         wijkcode_ma = as.factor(as.character(wijkcode_ma)),
         buurtcode_ma = as.factor(as.character(buurtcode_ma)),
         postcode4_pa = as.factor(as.character(postcode4_pa)),
         gemeentecode_pa = as.factor(as.character(gemeentecode_pa)),
         wijkcode_pa = as.factor(as.character(wijkcode_pa)),
         buurtcode_pa = as.factor(as.character(buurtcode_pa)))

##### Linking GBAPERSOON2018TABV2 #####
#### For child
Data1 <- Data1 |> 
  left_join(GBAPERSOON2018TABV2[, c("RINPERSOONS", "RINPERSOON", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG", "GBAGENERATIE")], 
            by = c("RINPERSOONS_ki" = "RINPERSOONS", "RINPERSOON_ki" = "RINPERSOON"))

# Renaming variables
Data1 <- Data1 |> 
  rename("generatie_ki" = "GBAGENERATIE")

# Defining birth date
Data1 <- Data1 |> 
  mutate(dd_3 = as.Date(paste0(jaar_3,"-",GBAGEBOORTEMAAND,"-",GBAGEBOORTEDAG)))

# remove GBAGEBOORTEMAAND & GBAGEBOORTEDAG
Data1 <- Data1 |> 
  select(-c("GBAGEBOORTEMAAND", "GBAGEBOORTEDAG"))


#### For mother
Data1 <- Data1 |> 
  left_join(GBAPERSOON2018TABV2, 
            by = c("RINPERSOONS_ma" = "RINPERSOONS", "RINPERSOON_ma" = "RINPERSOON"))

# Renaming variables
Data1 <- Data1 |> 
  rename("generatie_ma" = "GBAGENERATIE", "geboorteland_ma" = "GBAGEBOORTELAND", "herkomst_ma" = "GBAHERKOMSTGROEPERING")

# Defining birth date
Data1 <- Data1 |> 
  mutate(ddgeb_ma = paste0(GBAGEBOORTEJAAR,"-",GBAGEBOORTEMAAND,"-",GBAGEBOORTEDAG),
         ddgeb_ma = replace(ddgeb_ma, ddgeb_ma == "NA-NA-NA", NA),
         ddgeb_ma = as.Date(ddgeb_ma))

# remove GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG & GBAGESLACHT
Data1 <- Data1 |> 
  select(-c("GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG", "GBAGESLACHT"))


#### For father
Data1 <- Data1 |> 
  left_join(GBAPERSOON2018TABV2, 
            by = c("RINPERSOONS_pa" = "RINPERSOONS", "RINPERSOON_pa" = "RINPERSOON"))

# Renaming variables
Data1 <- Data1 |> 
  rename("generatie_pa" = "GBAGENERATIE", "geboorteland_pa" = "GBAGEBOORTELAND", "herkomst_pa" = "GBAHERKOMSTGROEPERING")

# Defining birth date
Data1 <- Data1 |> 
  mutate(ddgeb_pa = paste0(GBAGEBOORTEJAAR,"-",GBAGEBOORTEMAAND,"-",GBAGEBOORTEDAG),
         ddgeb_pa = replace(ddgeb_pa, ddgeb_pa == "NA-NA-NA", NA),
         ddgeb_pa = as.Date(ddgeb_pa))

# remove GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG & GBAGESLACHT
Data1 <- Data1 |> 
  select(-c("GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG", "GBAGESLACHT"))


# Transforming generatie, geboorteland, and herkomst into factor
Data1 <- Data1 |> 
  mutate(generatie_ki = as.factor(as.character(generatie_ki)),
         geboorteland_ma = as.factor(as.character(geboorteland_ma)),
         herkomst_ma = as.factor(as.character(herkomst_ma)),
         generatie_ma = as.factor(as.character(generatie_ma)),
         geboorteland_pa = as.factor(as.character(geboorteland_pa)),
         herkomst_pa = as.factor(as.character(herkomst_pa)),
         generatie_pa = as.factor(as.character(generatie_pa)))


##### Linking SECMBUS #####
#### For child
Data1 <- Data1 |> 
  left_join(SECMBUS2018V1[, c("RINPERSOONS", "RINPERSOON", "SECM")], 
            by = c("RINPERSOONS_ki" = "RINPERSOONS", "RINPERSOON_ki" = "RINPERSOON"))

# Renaming variables
Data1 <- Data1 |> 
  rename(SECM_ki = SECM)

#### For mother
Data1 <- Data1 |> 
  left_join(SECMBUS2018V1[, c("RINPERSOONS", "RINPERSOON", "SECM")], 
            by = c("RINPERSOONS_ma" = "RINPERSOONS", "RINPERSOON_ma" = "RINPERSOON"))

# Renaming variables
Data1 <- Data1 |> 
  rename(SECM_ma = SECM)

#### For father
Data1 <- Data1 |> 
  left_join(SECMBUS2018V1[, c("RINPERSOONS", "RINPERSOON", "SECM")], 
            by = c("RINPERSOONS_pa" = "RINPERSOONS", "RINPERSOON_pa" = "RINPERSOON"))

# Renaming variables
Data1 <- Data1 |> 
  rename(SECM_pa = SECM)

# Transforming SECM into factor
Data1 <- Data1 |> 
  mutate(SECM_ki = as.factor(as.character(SECM_ki)),
         SECM_ma = as.factor(as.character(SECM_ma)),
         SECM_pa = as.factor(as.character(SECM_pa)))

##### Linking attained EDU #####
#### For child, 2018
Data1 <- Data1 |> 
  left_join(HOOGSTEOPL[HOOGSTEOPL$jaar == 2018, c("RINPERSOONS", "RINPERSOON", "OPLNIVSOI2016AGG4HBMETNIRWO")], 
            by = c("RINPERSOONS_ki" = "RINPERSOONS", "RINPERSOON_ki" = "RINPERSOON"))

# Renaming variables
Data1 <- Data1 |> 
  rename("OPLNIVHB_ki_2018" = "OPLNIVSOI2016AGG4HBMETNIRWO")


#### For mother, 2014
Data1 <- Data1 |> 
  left_join(HOOGSTEOPL[HOOGSTEOPL$jaar == 2014, c("RINPERSOONS", "RINPERSOON", "OPLNIVSOI2016AGG4HBMETNIRWO")], 
            by = c("RINPERSOONS_ma" = "RINPERSOONS", "RINPERSOON_ma" = "RINPERSOON"))

# Renaming variables
Data1 <- Data1 |> 
  rename("OPLNIVHB_ma" = "OPLNIVSOI2016AGG4HBMETNIRWO")


#### For father, 2014
Data1 <- Data1 |> 
  left_join(HOOGSTEOPL[HOOGSTEOPL$jaar == 2014, c("RINPERSOONS", "RINPERSOON", "OPLNIVSOI2016AGG4HBMETNIRWO")], 
            by = c("RINPERSOONS_pa" = "RINPERSOONS", "RINPERSOON_pa" = "RINPERSOON"))

# Renaming variables
Data1 <- Data1 |> 
  rename("OPLNIVHB_pa" = "OPLNIVSOI2016AGG4HBMETNIRWO")

# Cleaning variable (9999 = NA)
Data1 <- Data1 |> 
  mutate(OPLNIVHB_ki_2018 = ifelse(OPLNIVHB_ki_2018 == 9999, NA, OPLNIVHB_ki_2018),
         OPLNIVHB_ma = ifelse(OPLNIVHB_ma == 9999, NA, OPLNIVHB_ma),
         OPLNIVHB_pa = ifelse(OPLNIVHB_pa == 9999, NA, OPLNIVHB_pa))

### Convert attained education into ISCED
Data1 <- Data1 |> 
  mutate(ISCED_ki_2018 = as.factor(as.character(ifelse(is.na(OPLNIVHB_ki_2018), NA,
                                                       ifelse(OPLNIVHB_ki_2018 <= 1999, 1,
                                                              ifelse(OPLNIVHB_ki_2018 >= 3000, 3, 2))))),
         ISCED_ma = as.factor(as.character(ifelse(is.na(OPLNIVHB_ma), NA,
                                                  ifelse(OPLNIVHB_ma <= 1999, 1,
                                                         ifelse(OPLNIVHB_ma >= 3000, 3, 2))))),
         ISCED_pa = as.factor(as.character(ifelse(is.na(OPLNIVHB_pa), NA,
                                                  ifelse(OPLNIVHB_pa <= 1999, 1,
                                                         ifelse(OPLNIVHB_pa >= 3000, 3, 2))))))

# Transforming SECM into factor
Data1 <- Data1 |> 
  mutate(SECM_ki = as.factor(as.character(SECM_ki)),
         SECM_ma = as.factor(as.character(SECM_ma)),
         SECM_pa = as.factor(as.character(SECM_pa)))

### Create edulate: secondary school diploma NOT attained by ## years of age
Data1 <- Data1 |> 
  mutate(edulate = as.factor(as.character(ifelse(is.na(OPLNIVHB_ki_2018), NA,
                                                 ifelse(OPLNIVHB_ki_2018 <= 1999, 1, 0)))))

