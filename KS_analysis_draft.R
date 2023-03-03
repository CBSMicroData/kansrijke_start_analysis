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


##### MICE #####
### Create 3 imputed datasets
library(mice)
library(lattice)

for (i in 2:3) {
  
  Data1imp <- Data1
  
  init <- mice(Data1, maxit = 0)
  meth <- init$method
  predM <- init$predictorMatrix
  
  ## Exclude the following from being used as predictors
  predM[, c("RINPERSOONS_ki", "RINPERSOON_ki", "ID_ki", "RINPERSOONS_ma", "RINPERSOON_ma",  "ID_ma", 
            "RINPERSOONS_pa", "RINPERSOON_pa", "ID_pa", 
            "SOORTOBJECTNUMMER_ki", "RINOBJECTNUMMER_ki", "postcode4_ki", "gemeentecode_ki", "wijkcode_ki", "buurtcode_ki", 
            "SOORTOBJECTNUMMER_ma", "RINOBJECTNUMMER_ma", "postcode4_ma", "gemeentecode_ma", "wijkcode_ma", "buurtcode_ma", 
            "SOORTOBJECTNUMMER_pa", "RINOBJECTNUMMER_pa", "postcode4_pa", "gemeentecode_pa", "wijkcode_pa", "buurtcode_pa", 
            "dd_3", "ddgeb_ma", "ddgeb_pa", "OPLNIVHB_ki_2018", "OPLNIVHB_ma", "OPLNIVHB_pa")] <- 0
    
  ## Skip the following variables from being imputed
  meth[c("RINPERSOONS_ki", "RINPERSOON_ki", "ID_ki", "RINPERSOONS_ma", "RINPERSOON_ma",  "ID_ma", 
            "RINPERSOONS_pa", "RINPERSOON_pa", "ID_pa", 
            "SOORTOBJECTNUMMER_ki", "RINOBJECTNUMMER_ki", "postcode4_ki", "gemeentecode_ki", "wijkcode_ki", "buurtcode_ki", 
            "SOORTOBJECTNUMMER_ma", "RINOBJECTNUMMER_ma", "postcode4_ma", "gemeentecode_ma", "wijkcode_ma", "buurtcode_ma", 
            "SOORTOBJECTNUMMER_pa", "RINOBJECTNUMMER_pa", "postcode4_pa", "gemeentecode_pa", "wijkcode_pa", "buurtcode_pa", 
            "dd_3", "ddgeb_ma", "ddgeb_pa", "OPLNIVHB_ki_2018", "OPLNIVHB_ma", "OPLNIVHB_pa")] <- ""
  
  ### No error when performing imputation for full dataset
  ## Creating imputations
  Data1imp <- mice(Data1imp, m = 1, method = meth, predictorMatrix = predM, maxit = 5)
  ## Combining observed and imputed values
  Data1imp <- complete(Data1imp, 1)
  
  ### Saving tibble
  saveRDS(Data1imp, file = paste0("Data1imp",i,".rds"))
}

#### To be able to perform on a much larger data set, divide into gemeentes
### Creating empty Data1imp tibble to be filled with in loop
# Data1imp <- Data1 |> 
#   filter(is.na(ID_ki))
### Loop below encounters an error: dims [product ##] do not match the length of object [71]
# for (i in na.omit(unique(Data1$gemeentecode_ki))) {
#   Data1gem <- Data1 |> 
#     filter(gemeentecode_ki == i)
#   
#   ## Creating imputations
#   Data1gem <- mice(Data1gem, m = 1, method = meth, predictorMatrix = predM, maxit = 5)
#   ## Combining observed and imputed values
#   Data1gem <- complete(Data1gem, 1)
#   
#   Data1imp <- rbind(Data1imp, Data1gem)
# }


  
##### Performing analyses #####
#### outcome = edulate
## Consider glmnet (more efficient?)
# library(glmnet)
# fit <- glmnet(Data1imp[, c("ISCED_ma", "ISCED_pa")], Data1imp$edulate, family = "binomial")
# print(fit)
# coef(fit)

### Checking for factorial polynomial transformations of continuous variables
# library(mfp)



### standard logistic regression
## fit for all imputed datasets
for (i in 1:3) {
  readRDS(file = paste0("Data1imp",i,".rds"))
  
  fit <- glm(edulate ~ gesl + lbw + preterm + ISCED_ma + ISCED_pa + SECM_ma + SECM_pa,
               family=binomial(link='logit'), data=Data1imp)
  summary(fit)
  
  saveRDS(fit, file = paste0("fit",i,".rds"))
}


### import all model fits and pool models
##Create list of model fits
listofmodels <- list()

for (i in 1:3) {
  listofmodels[[i]] <- assign(paste0("fit",i), readRDS(file = paste0("fit",i,".rds")))
}

## convert list back into mira type (for MICE functions)
library(mice)
library(broom)
listofmodels <- as.mira(listofmodels)
# pool model fits
fit_pool <- pool(listofmodels)
summary(fit_pool)

# To extract ORs and 95% CIs and p-values
fit_pool_summ <- summary(fit_pool)
fit_pool_OR <- cbind(names(readRDS(file = "fit1.rds")$coefficients), exp(fit_pool_summ[,2]), exp(fit_pool_summ[,2]-1.96*(fit_pool_summ[,3])), 
           exp(fit_pool_summ[,2]+1.96*(fit_pool_summ[,3])), fit_pool_summ[,4])
colnames(fit_pool_OR) <- (c("Variable", "OR", "95% Lower", "95% Upper", "p-value"))
fit_pool_OR

# ### ALTERNATIVE: Using mitools (https://bookdown.org/mwheymans/bookmi/data-analysis-after-multiple-imputation.html)
# library(mitools)
# 
# coefs <- MIextract(listofmodels, fun = coefficients)
# se <- MIextract(listofmodels, fun = vcov)


##### Model performance in ONE dataset #####
### Hacky way to use predict(): replace stored coefficients from one model fit with final pooled estimates
### https://stackoverflow.com/questions/52713733/how-to-use-predict-function-with-my-pooled-results-from-mice
### Stef van Buuren suggested alternative ways: https://github.com/amices/mice/issues/82
fit_pool_glm <- readRDS(file = "fit1.rds")
fit_pool_glm$coefficients <- summary(fit_pool)$estimate

# import one imputed dataset (just for outcome)
Data1imp <- readRDS(file = "Data1imp1.rds")

## Accuracy
predMODEL <- predict(fit_pool_glm, newdata = Data1imp, type='response')
fitMODEL <- ifelse(predMODEL >= quantile(predMODEL, 0.6),1,0) ## Define model threshold
misClasificErrorMODEL <- mean(fitMODEL != Data1imp$edulate)
print(paste('Accuracy MODEL',1-misClasificErrorMODEL))

## AUC
library(pROC)
predictionMODEL <- roc(Data1imp$edulate, predMODEL)
predictionMODEL
ci.auc(predictionMODEL, conf.level=0.95, method="delong")

## sensitivity, specificity, PPV, NPV
library(caret)
mtxMODEL <- confusionMatrix(table(fitMODEL, Data1imp$edulate))$table
mtxMODEL # columns represent actual outcomes; rows represent predicted outcomes
prop.table(mtxMODEL, 2)

## Sensitivity
sens_errorsMODEL <- sqrt(sensitivity(mtxMODEL) * (1 - sensitivity(mtxMODEL)) / sum(mtxMODEL[,1]))
sensLowerMODEL <- sensitivity(mtxMODEL) - 1.96 * sens_errorsMODEL
sensUpperMODEL <- sensitivity(mtxMODEL) + 1.96 * sens_errorsMODEL

## Specificity
spec_errorsMODEL <- sqrt(specificity(mtxMODEL) * (1 - specificity(mtxMODEL)) / sum(mtxMODEL[,2]))
specLowerMODEL <- specificity(mtxMODEL) - 1.96 * spec_errorsMODEL
specUpperMODEL <- specificity(mtxMODEL) + 1.96 * spec_errorsMODEL

## Positive Predictive Values
ppv_errorsMODEL <- sqrt(posPredValue(mtxMODEL) * (1 - posPredValue(mtxMODEL)) / sum(mtxMODEL[1,]))
ppvLowerMODEL <- posPredValue(mtxMODEL) - 1.96 * ppv_errorsMODEL
ppvUpperMODEL <- posPredValue(mtxMODEL) + 1.96 * ppv_errorsMODEL

## Negative Predictive Values
npv_errorsMODEL <- sqrt(negPredValue(mtxMODEL) * (1 - negPredValue(mtxMODEL)) / sum(mtxMODEL[2,]))
npvLowerMODEL <- negPredValue(mtxMODEL) - 1.96 * npv_errorsMODEL
npvUpperMODEL <- negPredValue(mtxMODEL) + 1.96 * npv_errorsMODEL

paste0(format(round(confusionMatrix(table(fitMODEL, Data1imp$edulate))[["byClass"]][["Sensitivity"]], 3), nsmall = 3),", ", 
       format(round(sensLowerMODEL, 3), nsmall = 3), "-", format(round(sensUpperMODEL, 3), nsmall = 3))

paste0(format(round(confusionMatrix(table(fitMODEL, Data1imp$edulate))[["byClass"]][["Specificity"]], 3), nsmall = 3),", ", 
       format(round(specLowerMODEL, 3), nsmall = 3), "-", format(round(specUpperMODEL, 3), nsmall = 3))

paste0(format(round(confusionMatrix(table(fitMODEL, Data1imp$edulate))[["byClass"]][["Pos Pred Value"]], 3), nsmall = 3),", ", 
       format(round(ppvLowerMODEL, 3), nsmall = 3), "-", format(round(ppvUpperMODEL, 3), nsmall = 3))

paste0(format(round(confusionMatrix(table(fitMODEL, Data1imp$edulate))[["byClass"]][["Neg Pred Value"]], 3), nsmall = 3),", ", 
       format(round(npvLowerMODEL, 3), nsmall = 3), "-", format(round(npvUpperMODEL, 3), nsmall = 3))


##### Model performance across multiple imputed datasets (with confidence bands) #####
### Hacky way to use predict(): replace stored coefficients from one model fit with final pooled estimates
### https://stackoverflow.com/questions/52713733/how-to-use-predict-function-with-my-pooled-results-from-mice
### Stef van Buuren suggested alternative ways: https://github.com/amices/mice/issues/82
library(tidyverse)
fit_pool_glm <- readRDS(file = "fit1.rds")
fit_pool_glm$coefficients <- summary(fit_pool)$estimate

# import one imputed dataset (just for outcome)
Data1imp <- readRDS(file = "Data1imp1.rds")

# Create empty tibble
tableMODEL <- tibble(
  modelname = character(),
  outcomename = character(),
  dataset = character(),
  n = integer(),
  cases = integer(),
  ROCAUC = numeric(),
  threshold = numeric(),
  true_positive = integer(),
  false_positive = integer(),
  true_negative = integer(),
  false_negative = integer(),
  true_positive_prop = numeric(),
  false_positive_prop = numeric(),
  true_negative_prop = numeric(),
  false_negative_prop = numeric(),
  sensitivity = numeric(),
  specificity = numeric(),
  PPV = numeric(),
  NPV = numeric()
)

# Define the following:
modelname1 <- "fit_pool_glm"
outcome1 <- Data1imp$edulate
outcomename1 <- "edulate"
dataset1 <- "Data1imp"
threshold1 <- as.list(seq(0.2,0.8,0.2))

#### Start loop
for (i in 1:3) { # Define number of datasets
  # import one imputed dataset (just for outcome)
  Data1imp <- readRDS(file = paste0(dataset1, i, ".rds")) ## Define dataset
  
  ### Start loop for each threshold
  for (j in threshold1) {
    ## Accuracy
    predMODEL <- predict(get(modelname1), newdata = Data1imp, type='response') ## Define model
    fitMODEL <- ifelse(predMODEL >= quantile(predMODEL, j), 1, 0) ## Define model threshold
    misClasificErrorMODEL <- mean(fitMODEL != outcome1) ## Define dataset and outcome
    
    ## AUC
    library(pROC)
    predictionMODEL <- roc(outcome1, predMODEL) ## Define dataset and outcome
    
    ## sensitivity, specificity, PPV, NPV
    library(caret)
    mtxMODEL <- confusionMatrix(table(fitMODEL, outcome1))$table ## Define dataset and outcome
    mtxMODEL # columns represent actual outcomes; rows represent predicted outcomes
    mtxMODEL_prop <- prop.table(mtxMODEL, 2)
    
    ## Sensitivity
    sens_errorsMODEL <- sqrt(sensitivity(mtxMODEL) * (1 - sensitivity(mtxMODEL)) / sum(mtxMODEL[,1]))
    
    ## Specificity
    spec_errorsMODEL <- sqrt(specificity(mtxMODEL) * (1 - specificity(mtxMODEL)) / sum(mtxMODEL[,2]))
    
    ## Positive Predictive Values
    ppv_errorsMODEL <- sqrt(posPredValue(mtxMODEL) * (1 - posPredValue(mtxMODEL)) / sum(mtxMODEL[1,]))
    
    ## Negative Predictive Values
    npv_errorsMODEL <- sqrt(negPredValue(mtxMODEL) * (1 - negPredValue(mtxMODEL)) / sum(mtxMODEL[2,]))
    
    ## Summarize in tibble
    tableMODEL <- tableMODEL |> 
      add_row(
        modelname = modelname1,
        outcomename = outcomename1,
        dataset = paste0(dataset1, i),
        n = length(Data1imp),
        cases = length(outcome1 == 1),
        ROCAUC = predictionMODEL$auc[1],
        threshold = j,
        true_positive = mtxMODEL[4],
        false_positive = mtxMODEL[2],
        true_negative = mtxMODEL[1],
        false_negative = mtxMODEL[3],
        true_positive_prop = mtxMODEL_prop[4],
        false_positive_prop = mtxMODEL_prop[2],
        true_negative_prop = mtxMODEL_prop[1],
        false_negative_prop = mtxMODEL_prop[3],
        sensitivity = sens_errorsMODEL,
        specificity = spec_errorsMODEL,
        PPV = ppv_errorsMODEL,
        NPV = npv_errorsMODEL
      )
  }
}

### find median, upper, and lower confidence bands across imputed datasets for each threshold
### Find confidence band for each metric (width of confidence band depends on # datasets)
### i.e., 11 datasets = 80% CBs, 21 datasets = 90% CBs, 41 datasets = 95% CBs
## Define lower and upper CB
lowerCB <- 0.10
upperCB <- 0.90

for (j in threshold1) {
  # Median
  tableMODEL <- tableMODEL |> 
    add_row(
      modelname = modelname1,
      outcomename = outcomename1,
      dataset = paste0(dataset1,"1-3 median"), # Define number of datasets
      n = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_positive + false_positive + true_negative + false_negative))),
      cases = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_positive + false_negative))),
      ROCAUC = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(ROCAUC))),
      threshold = j,
      true_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_positive))),
      false_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(false_positive))),
      true_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_negative))),
      false_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(false_negative))),
      true_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_positive_prop))),
      false_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(false_positive_prop))),
      true_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_negative_prop))),
      false_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(false_negative_prop))),
      sensitivity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(sensitivity))),
      specificity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(specificity))),
      PPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(PPV))),
      NPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(NPV)))
    )
  # Lower CB
  tableMODEL <- tableMODEL |> 
    add_row(
      modelname = modelname1,
      outcomename = outcomename1,
      dataset = paste0(dataset1,"1-3 ",lowerCB*100,"% band"), # Define number of datasets
      n = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive + false_positive + true_negative + false_negative, probs = lowerCB))),
      cases = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive + false_negative, probs = lowerCB))),
      ROCAUC = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(ROCAUC, probs = lowerCB))),
      threshold = j,
      true_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive, probs = lowerCB))),
      false_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_positive, probs = lowerCB))),
      true_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_negative, probs = lowerCB))),
      false_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_negative, probs = lowerCB))),
      true_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive_prop, probs = lowerCB))),
      false_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_positive_prop, probs = lowerCB))),
      true_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_negative_prop, probs = lowerCB))),
      false_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_negative_prop, probs = lowerCB))),
      sensitivity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(sensitivity, probs = lowerCB))),
      specificity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(specificity, probs = lowerCB))),
      PPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(PPV, probs = lowerCB))),
      NPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(NPV, probs = lowerCB)))
    )
  # upper CB
  tableMODEL <- tableMODEL |> 
    add_row(
      modelname = modelname1,
      outcomename = outcomename1,
      dataset = paste0(dataset1,"1-3 ",upperCB*100,"% band"), # Define number of datasets
      n = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive + false_positive + true_negative + false_negative, probs = upperCB))),
      cases = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive + false_negative, probs = upperCB))),
      ROCAUC = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(ROCAUC, probs = upperCB))),
      threshold = j,
      true_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive, probs = upperCB))),
      false_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_positive, probs = upperCB))),
      true_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_negative, probs = upperCB))),
      false_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_negative, probs = upperCB))),
      true_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive_prop, probs = upperCB))),
      false_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_positive_prop, probs = upperCB))),
      true_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_negative_prop, probs = upperCB))),
      false_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_negative_prop, probs = upperCB))),
      sensitivity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(sensitivity, probs = upperCB))),
      specificity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(specificity, probs = upperCB))),
      PPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(PPV, probs = upperCB))),
      NPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(NPV, probs = upperCB)))
    )
}


## Save tibble
saveRDS(tableMODEL, file = "tableMODEL.rds")



##### Model performance per municipality across multiple imputed datasets (with confidence bands) #####
### Hacky way to use predict(): replace stored coefficients from one model fit with final pooled estimates
### https://stackoverflow.com/questions/52713733/how-to-use-predict-function-with-my-pooled-results-from-mice
### Stef van Buuren suggested alternative ways: https://github.com/amices/mice/issues/82
library(tidyverse)
fit_pool_glm <- readRDS(file = "fit1.rds")
fit_pool_glm$coefficients <- summary(fit_pool)$estimate

# import one imputed dataset (just for outcome)
Data1imp <- readRDS(file = "Data1imp1.rds")

# Create empty tibble
tableMODEL_gem <- tibble(
  modelname = character(),
  outcomename = character(),
  dataset = character(),
  municipality = character(),
  n = integer(),
  cases = integer(),
  ROCAUC = numeric(),
  threshold = numeric(),
  true_positive = integer(),
  false_positive = integer(),
  true_negative = integer(),
  false_negative = integer(),
  true_positive_prop = numeric(),
  false_positive_prop = numeric(),
  true_negative_prop = numeric(),
  false_negative_prop = numeric(),
  sensitivity = numeric(),
  specificity = numeric(),
  PPV = numeric(),
  NPV = numeric()
)

# Define the following:
modelname1 <- "fit_pool_glm"
outcomename1 <- "edulate"
dataset1 <- "Data1imp"
threshold1 <- as.list(seq(0.2,0.8,0.2))
municipality1 <- as.list(as.character(unique(na.omit(Data1imp$gemeentecode_ki))))

#### Start loop
for (i in 1:3) { # Define number of datasets
  # import one imputed dataset (just for outcome)
  Data1imp <- readRDS(file = paste0(dataset1, i, ".rds")) ## Define dataset
  
  ### Start loop for each municipality
  for (k in municipality1) {
    Data1imp_mun <- Data1imp |> 
      filter(gemeentecode_ki == k)
    predMODEL <- predict(get(modelname1), newdata = Data1imp_mun, type='response') ## Define model
    
    ### Start loop for each threshold
    for (j in threshold1) {
      
      fitMODEL <- ifelse(predMODEL >= quantile(predMODEL, j), 1, 0) ## Define model threshold
      outcome1 <- Data1imp_mun$edulate
      
      ## Accuracy
      misClasificErrorMODEL <- mean(fitMODEL != outcome1) ## Define dataset and outcome
    
      ## AUC
      library(pROC)
      predictionMODEL <- roc(outcome1, predMODEL) ## Define dataset and outcome
      
      ## sensitivity, specificity, PPV, NPV
      library(caret)
      mtxMODEL <- confusionMatrix(table(fitMODEL, outcome1))$table ## Define dataset and outcome
      mtxMODEL # columns represent actual outcomes; rows represent predicted outcomes
      mtxMODEL_prop <- prop.table(mtxMODEL, 2)
      
      ## Sensitivity
      sens_errorsMODEL <- sqrt(sensitivity(mtxMODEL) * (1 - sensitivity(mtxMODEL)) / sum(mtxMODEL[,1]))
      
      ## Specificity
      spec_errorsMODEL <- sqrt(specificity(mtxMODEL) * (1 - specificity(mtxMODEL)) / sum(mtxMODEL[,2]))
      
      ## Positive Predictive Values
      ppv_errorsMODEL <- sqrt(posPredValue(mtxMODEL) * (1 - posPredValue(mtxMODEL)) / sum(mtxMODEL[1,]))
      
      ## Negative Predictive Values
      npv_errorsMODEL <- sqrt(negPredValue(mtxMODEL) * (1 - negPredValue(mtxMODEL)) / sum(mtxMODEL[2,]))
      
      ## Summarize in tibble
      tableMODEL_gem <- tableMODEL_gem |> 
        add_row(
          modelname = modelname1,
          outcomename = outcomename1,
          dataset = paste0(dataset1, i),
          municipality = k,
          n = length(Data1imp_mun),
          cases = length(outcome1 == 1),
          ROCAUC = predictionMODEL$auc[1],
          threshold = j,
          true_positive = mtxMODEL[4],
          false_positive = mtxMODEL[2],
          true_negative = mtxMODEL[1],
          false_negative = mtxMODEL[3],
          true_positive_prop = mtxMODEL_prop[4],
          false_positive_prop = mtxMODEL_prop[2],
          true_negative_prop = mtxMODEL_prop[1],
          false_negative_prop = mtxMODEL_prop[3],
          sensitivity = sens_errorsMODEL,
          specificity = spec_errorsMODEL,
          PPV = ppv_errorsMODEL,
          NPV = npv_errorsMODEL
      )
    }
  }
}

### find median, upper, and lower confidence bands across imputed datasets for each threshold
### Find confidence band for each metric (width of confidence band depends on # datasets)
### i.e., 11 datasets = 80% CBs, 21 datasets = 90% CBs, 41 datasets = 95% CBs
## Define lower and upper CB
lowerCB <- 0.10
upperCB <- 0.90

for (k in municipality1) {
  for (j in threshold1) {
    # Median
    tableMODEL_gem <- tableMODEL_gem |> 
      add_row(
        modelname = modelname1,
        outcomename = outcomename1,
        dataset = paste0(dataset1,"1-3 median"), # Define number of datasets
        municipality = k,
        n = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_positive + false_positive + true_negative + false_negative))),
        cases = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_positive + false_negative))),
        ROCAUC = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(ROCAUC))),
        threshold = j,
        true_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_positive))),
        false_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(false_positive))),
        true_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_negative))),
        false_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(false_negative))),
        true_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_positive_prop))),
        false_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(false_positive_prop))),
        true_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_negative_prop))),
        false_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(false_negative_prop))),
        sensitivity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(sensitivity))),
        specificity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(specificity))),
        PPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(PPV))),
        NPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(NPV)))
      )
    # Lower CB
    tableMODEL_gem <- tableMODEL_gem |> 
      add_row(
        modelname = modelname1,
        outcomename = outcomename1,
        dataset = paste0(dataset1,"1-3 ",lowerCB*100,"% band"), # Define number of datasets
        municipality = k,
        n = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive + false_positive + true_negative + false_negative, probs = lowerCB))),
        cases = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive + false_negative, probs = lowerCB))),
        ROCAUC = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(ROCAUC, probs = lowerCB))),
        threshold = j,
        true_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive, probs = lowerCB))),
        false_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_positive, probs = lowerCB))),
        true_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_negative, probs = lowerCB))),
        false_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_negative, probs = lowerCB))),
        true_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive_prop, probs = lowerCB))),
        false_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_positive_prop, probs = lowerCB))),
        true_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_negative_prop, probs = lowerCB))),
        false_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_negative_prop, probs = lowerCB))),
        sensitivity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(sensitivity, probs = lowerCB))),
        specificity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(specificity, probs = lowerCB))),
        PPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(PPV, probs = lowerCB))),
        NPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(NPV, probs = lowerCB)))
      )
    # upper CB
    tableMODEL_gem <- tableMODEL_gem |> 
      add_row(
        modelname = modelname1,
        outcomename = outcomename1,
        dataset = paste0(dataset1,"1-3 ",upperCB*100,"% band"), # Define number of datasets
        municipality = k,
        n = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive + false_positive + true_negative + false_negative, probs = upperCB))),
        cases = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive + false_negative, probs = upperCB))),
        ROCAUC = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(ROCAUC, probs = upperCB))),
        threshold = j,
        true_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive, probs = upperCB))),
        false_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_positive, probs = upperCB))),
        true_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_negative, probs = upperCB))),
        false_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_negative, probs = upperCB))),
        true_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive_prop, probs = upperCB))),
        false_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_positive_prop, probs = upperCB))),
        true_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_negative_prop, probs = upperCB))),
        false_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_negative_prop, probs = upperCB))),
        sensitivity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(sensitivity, probs = upperCB))),
        specificity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(specificity, probs = upperCB))),
        PPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(PPV, probs = upperCB))),
        NPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(NPV, probs = upperCB)))
      )
  }
}

## Save tibble
saveRDS(tableMODEL_gem, file = "tableMODEL_gem.rds")


#####  #####