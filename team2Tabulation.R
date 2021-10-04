#### Load Libraries and data ####

library(RSQLite) #Use SQLite database to store and read data
library(dplyr) # Data manipulation
library(tidyverse) #Data science package
library(tidyr) #Data science package
#library(readstata13) #Read Stata files into R

# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository) # Required for file.choose() function


# Establish connection to the SQLite databases
mydb <- dbConnect(RSQLite::SQLite(), "data/secure/sqlite/census2020.SQLite")
t2 <- dbConnect(RSQLite::SQLite(), "data/open/team2/team2.SQLite")

#### Read data from SQLite database and run summary ####
provPop <- dbGetQuery(mydb, "SELECT province, 
                                    sum(province_factor) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province ")

provSex <- dbGetQuery(mydb, "SELECT province, sex,
                                round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, sex")

#Table:3:1:Population living in Private households by ethic origin and sex,Province and urban-rural residence.
tab3_1EthnicOrigin <- dbGetQuery(mydb, "SELECT province,area_council,urban_rural,sex,ethnicity, hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, urban_rural, area_council, ethnicity, hhld_type") 


#Table:3;2;Male population living in private households by ethic origin and sex,province and urban-rural residence.
tab3_2EthnicMalePop <- dbGetQuery(mydb, "SELECT province,area_council,urban_rural,totmale,ethnicity,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, urban_rural, area_council, ethnicity,totmale,hhld_type") 

#Table: 3:3: Female population living in private households by ethic origin and sex,province and urban-rural residence
tab3_3EthnicFemalePop <- dbGetQuery(mydb, "SELECT province,area_council,urban_rural,totfemale,ethnicity,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, urban_rural, area_council, ethnicity,totfemale,hhld_type") 


#Table:3:4: Population living in Private household by ethic origin,5-year age groups and sex
tab3_4Ethnic5yearAgeGrp <- dbGetQuery(mydb, "SELECT province,area_council,urban_rural,age_5yr_grp_80,ethnicity,hhld_type,sex,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, urban_rural, area_council, ethnicity,age_5yr_grp_80,hhld_type,sex") 

#Table:3:5: Total population by denominations and area council by province and urban-rural residence.
tab3_5Religion <- dbGetQuery(mydb, "SELECT province,area_council,urban_rural,religion,hhld_type,sex,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, urban_rural, area_council,religion,sex,hhld_type") 

#Table: 3:6: Male Population by denominations and area council by province and urban-rural residence.
tab3_6ReligionMale <- dbGetQuery(mydb, "SELECT province,area_council,urban_rural,religion,totmale,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, urban_rural, area_council,religion,totmale,hhld_type") 

#Table:3:7: Female Population by denominations and area council by province and urban-rural residence.
tab3_7ReligionFemale <- dbGetQuery(mydb, "SELECT province,area_council,urban_rural,religion,totfemale,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, urban_rural, area_council,religion,totfemale,hhld_type") 

#Table:3:8: Total population by denominations,5-year age groups and sex.
tab3_8Religion5yearAgeGroup <- dbGetQuery(mydb, "SELECT province,religion,age_5yr_grp_80,sex,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province,religion,age_5yr_grp_80,sex,hhld_type") 

#Table:3:9: Population living in Private households by citizenship and sex,area council by province and urban-rural residence.
tab3_9Citizenship <- dbGetQuery(mydb, "SELECT province,area_council,urban_rural,citizenship,sex,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, urban_rural, area_council,citizenship,sex,hhld_type") 

#Table:3:10: Population living in private households by 5-year age groups,sex and citizenship
tab3_10CitizenshipBy5yearAgeGroup <- dbGetQuery(mydb, "SELECT province,area_council,urban_rural,citizenship,sex,age_5yr_grp_80,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, urban_rural, area_council,citizenship,sex,age_5yr_grp_80,hhld_type") 

#Table:3:11: Non-Vanuatu citizens living in private households by country of citizenship and by sex.
tab3_11NonCitizenship <- dbGetQuery(mydb, "SELECT province,other_citizenship,sex,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province,other_citizenship,sex,hhld_type") 

#Table :3:12: Total registered births in private households by sex and area council by province and urban-rural residence.
tab3_12RegisteredBirths <- dbGetQuery(mydb, "SELECT province,urban_rural,area_council,sex,birth_certificate,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province,urban_rural,area_council,birth_certificate,sex")



#Table:3:13: Total population living in private households with valid National ID card by sex and area council by province and urban-rural residence.
tab3_13NationalIDCard <- dbGetQuery(mydb, "SELECT province,urban_rural,area_council,sex,national_id,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province,urban_rural,area_council,sex,national_id,hhld_type")

#Table:3:14: Total population living in Private households with valid Electoral card by sex and area council by province and urban-rural residence.
tab3_14ElectrolCard <- dbGetQuery(mydb, "SELECT province,urban_rural,area_council,sex,national_id,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province,urban_rural,area_council,sex,national_id,hhld_type")

#Table:3:15: Total population by marital status and sex, and by province and urban-rural residence.
tab3_15MaritalStatusandSex <- dbGetQuery(mydb, "SELECT province,urban_rural,sex,marital_status,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province,urban_rural,sex,marital_status,hhld_type")

#Table:3:16: Type of legal marriage by sex, area council by province and urban-rural residence.
tab3_16LegalMarriageBySex <- dbGetQuery(mydb, "SELECT province,area_council, urban_rural,sex,custMarriage_1, custMarriage_2, custMarriage_3,hhld_type,
                              round(sum(province_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1, marital_status = 2
                              GROUP BY province,area_council, urban_rural,sex,custMarriage_1, custMarriage_2, custMarriage_3,hhld_type")


# Select variables eg province & sex
                              # sum using correction factors from province
                              #From table in excel eg person
                              # WHERE also acts like filter
                              #GROUP BY province & Sex


acSex <- dbGetQuery(mydb, "SELECT area_council, sex,
                                round(sum(ac_factor)) as population
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY area_council, sex")


# Create pivot table
provSexPivot <- provSex %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = sex, values_from = population, values_fill = 0) %>%
  ungroup()


acSexPivot <- acSex %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = sex, values_from = population, values_fill = 0) %>%
  ungroup()

#write table to SQLite database
dbWriteTable(t2, "provSexPivot", provSexPivot, overwrite = TRUE)







acPop <- dbGetQuery(mydb, "SELECT area_council,
                                  sum(ac_factor) as population
                           FROM person
                           WHERE can_enumerate = 1
                           GROUP BY area_council")

pop_hh <- dbGetQuery(mydb, "SELECT province,
                                   round(sum(hhld_ac_factor * hhsize)) as population
                            FROM household
                            WHERE can_enumerate = 1
                            GROUP BY province ")


livestock <- dbGetQuery(mydb, "SELECT household.area_council,
                                      livestock.livestock_id,
                                      round(sum(household.hhld_ac_factor * livestock.num_livestock)) as totallivestock  
                              FROM household
                              INNER JOIN livestock ON household.interview_key = livestock.interview_key
                              WHERE household.can_enumerate = 1
                              GROUP BY household.area_council, livestock.livestock_id
                              ORDER BY household.island
                        ")

#Uaing pivot_wider to cross-tabulate with 2 variables
livestockpivot <- livestock %>%
  pivot_wider(names_from = livestock_id, values_from = totallivestock, values_fill = 0) %>%
  ungroup()

#### Write computed tables to teams SQLite database #### 
dbWriteTable(t1, "provPop", provPop, overwrite=TRUE)
dbWriteTable(t1, "acPop", acPop, overwrite=TRUE)
dbWriteTable(t1, "livestockpivot", livestockpivot, overwrite=TRUE)
