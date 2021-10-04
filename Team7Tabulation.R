#### Load Libraries and data ####

# Clear the environment
rm(list = ls())

#install.packages("RSQLite") #Use SQLite database to store and read data
#install.packages("dplyr") # Data manipulation
#install.packages("tidyverse") #Data science package
#install.packages("tidyr") #Data science package
#install.packages("readstata13") #Read Stata files into R

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
t7 <- dbConnect(RSQLite::SQLite(), "data/open/team7/team7.SQLite")

#### Read data from SQLite database and run summary ####
provPop <- dbGetQuery(mydb, "SELECT province, 
                                    sum(province_factor) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province ")

provSex <- dbGetQuery(mydb, "SELECT province, sex, round(sum(province_factor)) as population
                      FROM person
                      WHERE can_enumerate = 1
                      GROUP BY province, sex ")


test <- dbGetQuery(mydb, "SELECT area_council, province, sex, round(sum(province_factor)) as population
                      FROM person
                      WHERE can_enumerate = 1
                      GROUP BY area_council, province, sex ")


# create a pivot table

provSexPivot <- provSex %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = sex, values_from = population, values_fill = 0) %>%
  ungroup()

#write table to SQlite Dtabase
dbWriteTable(t7, "provSexpivot", provSexPivot, overwrite = TRUE)  

acPop <- dbGetQuery(mydb, "SELECT area_council,
                                  sum(ac_factor) as population
                           FROM person
                           WHERE can_enumerate = 1
                           GROUP BY area_council")

acPopSex <- dbGetQuery(mydb, "SELECT area_council, sex,
                                  round(sum(ac_factor)) as population
                           FROM person
                           WHERE can_enumerate = 1
                           GROUP BY area_council, sex")

# Create privot for area council by sex

acPopSexPivot <- acPopSex %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = sex, values_from = population, values_fill = 0) %>%
  ungroup()



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

#Using pivot_wider to cross-tabulate with 2 variables
livestockpivot <- livestock %>%
  pivot_wider(names_from = livestock_id, values_from = totallivestock, values_fill = 0) %>%
  ungroup()

#### Table 9.1: Number of private households in 2009 and 2020 by area council by province and urban-rural ####

#### Table 9.2: Household size by area council by province and urban-rural ####

#### Table 9.3: Number of private households by type of living quarters, area council by province and urban-rural ####

#### Table 9.4: Number of private households by housing tenure, area council by province and urban-rural ####

#### Table 9.5: Number of private households by land tenure, area council by province and urban-rural ####

#### Table 9.5: Number of privately owned houses by land tenure, area council by province and urban-rural ####

#### Table 9.6: Number of privately owned houses by age of building, area council by province and urban-rural (question I4) ####

#### Table 9.7: Number of private households by number of rooms, area council by province and urban-rural (Question I5) ####

#### Table 9.8: Main material used for floors, wall and roof by area council by province and urban-rural ####

#### Table 9.9: Number of private households by main source of drinking water, area council by province and urban-rural ####

#### Table 9.10: Number of private households by main source of washing water, area council by province and urban-rural ####

#### Table 9.11: Number of private households by main toilet facility, area council by province and urban-rural ####

#### Table 9.12: Number of private households by main form of household rubbish disposal, area council by province and urban-rural ####

#### Table 9.13: Number of private households by main source of cooking energy, area council by province and urban-rural ####

#### Table 9.14: Number of private households whether having any facilities for handwashing by area council by province and urban-rural ####

#### Table 9.15: Number of private households by main source of lighting, area council by province and urban-rural ####

#### Table 9.16: Number of private households by main source of electricity, area council by province and urban rural ####

#### Table 9.17a: Total number of private households growing crops, type of crop and by area council by province and urban-rural ####

#### Table 9.17b: Number of private households growing crops on own account whether for household consumption, sale or exchange by area council by province and urban-rural ####

#### Table 9.18a: Total number of livestock owned by private household by area council by province and urban-rural ####

#### Table 9.18b: Number of private households owned livestock whether for household consumption, sale or exchange by area council by province and urban-rural ####

#### Table 9.19: Number of private households engaged in fishing in the last 12 months by area council by province and urban-rural ####

#### Table 9.20: Number of private households having land used for forestry by area council by province and urban-rural ####

#### Table 9.21: Number of private households owned a mean of transport in working condition by area council by province and urban-rural ####

#### Table 9.22: Number of private households owned an appliance in working condition by area council by province and urban-rural ####

#### Table 9.23: Number of private households owned a means of communication in working condition by area council by province and urban-rural ####

#### Table 9.24: Number of insecticides treated bed nets in private households by area council by province and urban-rural  ####

#### Table 9.25: Number of private households by main source of household income, area council by province and urban-rural #### 

####Table 9.26: Number of private households receiving any goods in kind from overseas or elsewhere in Vanuatu by area council by province and urban-rural ####




#### Write computed tables to teams SQLite database #### 
#dbWriteTable(t1, "provPop", provPop, overwrite=TRUE)
#dbWriteTable(t1, "acPop", acPop, overwrite=TRUE)
#dbWriteTable(t1, "livestockpivot", livestockpivot, overwrite=TRUE)
